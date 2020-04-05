#' Metrics
#'
#' @description
#'
#' A metric is a measure which can be aggregated into a time series, and comes
#' in one of four types: counters, gauges, histograms, and summaries.
#'
#' @param name The name of the metric.
#' @param help A brief, one-sentence explanation of the metric's meaning.
#' @param ... Currently ignored.
#' @param registry Where to register the metric for later retrieval.
#'
#' @seealso The official documenation on [Metric Types](https://prometheus.io/docs/concepts/metric_types/).
#' @name metrics
#' @export
counter_metric <- function(name, help, ..., registry = global_registry()) {
  existing <- registry$metric(name, type = "counter")
  if (is.null(existing)) {
    Counter$new(name = name, help = help, ..., registry = registry)
  } else {
    existing
  }
}

#' @rdname metrics
#' @export
gauge_metric <- function(name, help, ..., registry = global_registry()) {
  existing <- registry$metric(name, type = "gauge")
  if (is.null(existing)) {
    Gauge$new(name = name, help = help, ..., registry = registry)
  } else {
    existing
  }
}

Metric <- R6::R6Class(
  "Metric",
  public = list(
    initialize = function(name, help, type = "gauge", ...,
                          registry = global_registry()) {
      if (!grepl("^[a-zA-Z_:][a-zA-Z0-9_:]*$", name)) {
        stop("Invalid metric name: '", name, "'.")
      }
      private$name <- name
      private$help <- help
      private$type <- type
      # TODO: Validate labels.
      private$labels <- list(...)
      private$registry <- registry

      registry$register(name = name, type = type, self)
    },

    unregister = function() {
      private$registry$unregister(name = private$name, type = private$type)
    }
  ),
  private = list(
    name = NULL, help = NULL, type = NULL, labels = NULL, registry = NULL,

    header = function() {
      sprintf(
        "# HELP %s %s\n# TYPE %s %s", private$name, private$help,
        private$name, private$type
      )
    }
  )
)

Counter <- R6::R6Class(
  "Counter", inherit = Metric,
  public = list(
    initialize = function(name, help, ..., registry = global_registry()) {
      # Compatibility with OpenMetrics.
      name <- gsub("_total$", "", name)
      super$initialize(name, help, type = "counter", ..., registry = registry)
    },

    render = function() {
      sprintf(
        "%s\n%s_total%s %s\n", private$header(), private$name, "", private$value
      )
    },

    inc = function(by = 1, ...) {
      stopifnot(by > 0)
      private$value <- private$value + by
      invisible(private$value)
    },

    reset = function() {
      private$value <- 0
    }
  ),
  private = list(
    value = 0
  )
)

Gauge <- R6::R6Class(
  "Gauge", inherit = Metric,
  public = list(
    initialize = function(name, help, ..., registry = global_registry()) {
      super$initialize(name, help, type = "gauge", ..., registry = registry)
    },

    render = function() {
      sprintf(
        "%s\n%s%s %s\n", private$header(), private$name, "", private$value
      )
    },

    inc = function(by = 1, ...) {
      private$value <- private$value + by
      invisible(private$value)
    },

    dec = function(by = 1, ...) {
      private$value <- private$value - by
      invisible(private$value)
    },

    set = function(value, ...) {
      stopifnot(is.numeric(value))
      private$value <- value
    },

    set_to_current_time = function(...) {
      private$value <- unclass(Sys.time())
    },

    reset = function() {
      private$value <- 0
    }
  ),
  private = list(
    value = 0
  )
)
