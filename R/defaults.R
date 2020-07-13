#' Default Metrics for R Processes
#'
#' Registers the [standard process metrics](https://prometheus.io/docs/instrumenting/writing_clientlibs/#process-metrics)
#' for Prometheus clients. Not all metrics are supported on all operating
#' systems.
#'
#' @param registry A `Registry` object. See [registry()].
#' @return Called for side effects only.
#'
#' @examples
#' register_default_metrics()
#' render_metrics()
#'
#' @export
register_default_metrics = function(registry = global_registry()) {
  existing <- registry$collector("default")
  if (!is.null(existing)) {
    invisible(existing)
  } else {
    invisible(DefaultCollector$new(registry = registry))
  }
}

DefaultCollector <- R6::R6Class(
  "DefaultCollector",
  public = list(
    initialize = function(registry = global_registry()) {
      # Most metrics are supported only on Linux, for now, since we can get
      # them from /proc/self fairly easily.
      private$has_procfs <- grepl("linux", R.version$os, fixed = TRUE)
      if (!private$has_procfs) {
        private$metrics <- list(
          "process_cpu_seconds_total" = counter_metric(
            "process_cpu_seconds_total",
            "Total user and system CPU time spent in seconds.",
            registry = registry
          )
        )
      } else {
        private$metrics <- list(
          "process_cpu_seconds_total" = counter_metric(
            "process_cpu_seconds_total",
            "Total user and system CPU time spent in seconds.",
            registry = registry
          ),
          "process_open_fds" = gauge_metric(
            "process_open_fds", "Number of open file descriptors.",
            registry = registry
          ),
          "process_resident_memory_bytes" = gauge_metric(
            "process_resident_memory_bytes", "Resident memory size in bytes.",
            registry = registry
          ),
          "process_virtual_memory_bytes" = gauge_metric(
            "process_virtual_memory_bytes", "Virtual memory size in bytes.",
            registry = registry
          ),
          "process_heap_bytes" = gauge_metric(
            "process_heap_bytes", "Process heap size in bytes.",
            registry = registry
          ),
          "process_start_time_seconds" = gauge_metric(
            "process_start_time_seconds",
            "Start time of the process since unix epoch in seconds.",
            registry = registry
          )
        )
        # Read the process creation time just once.
        ctime <- unclass(file.info("/proc/self")$ctime)
        private$metrics$process_start_time_seconds$set(trunc(ctime))
        self$update()
      }
      registry$register_collector("default", self)
      private$registry <- registry
    },

    update = function() {
      # Since process_cpu_seconds_total is a counter, we can't just set() it.
      cputime <- sum(proc.time()[1:2], na.rm = TRUE)
      private$metrics$process_cpu_seconds_total$inc(
        cputime - private$last_cputime
      )
      private$last_cputime <- cputime

      if (private$has_procfs) {
        # list.files() opens a fd of its own, don't count that.
        open_fds <- length(list.files("/proc/self/fd")) - 1
        private$metrics$process_open_fds$set(open_fds)

        status <- readLines("/proc/self/status", encoding = "UTF-8")
        vmstats <- status[grepl("^Vm(Size|RSS|Data)", status)]
        vmstats <- sapply(strsplit(vmstats, "\\s+"), function(row) {
          out <- as.numeric(row[2]) * 1024
          names(out) <- row[1]
          out
        })
        names(vmstats) <- gsub(":", "", names(vmstats), fixed = TRUE)
        private$metrics$process_resident_memory_bytes$set(vmstats["VmRSS"])
        private$metrics$process_virtual_memory_bytes$set(vmstats["VmSize"])
        private$metrics$process_heap_bytes$set(vmstats["VmData"])
      }
    },

    unregister = function() {
      unregistered <- sapply(private$metrics, function(m) m$unregister())
      private$registry$unregister_collector("default")
      invisible(sum(unregistered))
    }
  ),
  private = list(
    metrics = NULL, has_procfs = FALSE, registry = NULL, last_cputime = 0
  )
)
