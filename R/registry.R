#' Metric Registries
#'
#' @description
#'
#' A registry is a collection of one or more metrics. By default, metrics are
#' added to the object returned by `global_registry()`, but new registries can
#' also be created with `registry()`. Use `collect_metrics()` to return all
#' metrics that a registry is aware of, or `render_metrics()` to render all of
#' them in aggregate.
#'
#' @return `registry()` and `global_registry()` return `Registry` objects (see
#'   Details), while `collect_metrics()` returns a list of [`metrics`] and
#'   `render_metrics()` returns a string.
#'
#' @details
#'
#' `Registry` objects have methods, but they are not intended to be called by
#' users and have no stable API.
#'
#' @name registry
#' @export
registry <- function() {
  Registry$new()
}

#' @rdname registry
#' @export
global_registry <- function() {
  .registry
}

#' @param registry A `Registry` object, defaulting to the shared global one.
#' @rdname registry
#' @export
collect_metrics <- function(registry = global_registry()) {
  registry$collect()
}

#' @param registry A `Registry` object, defaulting to the shared global one.
#' @rdname registry
#' @export
render_metrics <- function(registry = global_registry()) {
  registry$render_all()
}

Registry <- R6::R6Class(
  "Registry",
  public = list(
    initialize = function() {
      # TODO: This might work as a list column.
      private$metrics <- list()
      private$index <- data.frame(
        name = character(), type = character(), stringsAsFactors = FALSE,
        check.names = FALSE, check.rows = FALSE
      )
      private$collectors <- list()
    },

    register = function(name, type, metric) {
      row <- which(private$index$name == name)
      if (length(row) == 0) {
        new_metric <- data.frame(
          name = name, type = type, stringsAsFactors = FALSE,
          check.names = FALSE, check.rows = FALSE
        )
        private$index <- rbind(private$index, new_metric)
        private$metrics[[nrow(private$index)]] <- metric
      } else {
        private$metrics[[row]] <- metric
      }
    },

    register_collector = function(name, collector) {
      private$collectors[[name]] <- collector
    },

    unregister = function(name, type) {
      row <- which(private$index$name == name)
      if (length(row) != 0 && is.environment(private$metrics[[row]])) {
        # We don't need to modify the data frame.
        # Note: Use NA instead of NULL to prevent entries from being dropped.
        private$metrics[[row]] <- NA
        invisible(TRUE)
      } else {
        invisible(FALSE)
      }
    },

    unregister_collector = function(name) {
      if (!is.null(private$collectors[[name]])) {
        private$collectors[[name]] <- NULL
        invisible(TRUE)
      } else {
        invisible(FALSE)
      }
    },

    metric = function(name, type) {
      row <- which(private$index$name == name & private$index$type == type)
      if (length(row) != 0 && is.environment(private$metrics[[row]])) {
        private$metrics[[row]]
      } else {
        NULL
      }
    },

    collector = function(name) {
      private$collectors[[name]]
    },

    collect = function() {
      for (collector in private$collectors) {
        collector$update()
      }
      private$metrics[vapply(private$metrics, is.environment, logical(1))]
    },

    render_all = function() {
      entries <- vapply(self$collect(), function(x) x$render(), character(1))
      paste(c(entries, "# EOF"), collapse = "")
    },

    reset_all = function() {
      for (metric in self$collect()) {
        metric$reset()
      }
    }
  ),
  private = list(
    index = NULL, metrics = NULL, collectors = NULL
  )
)

.registry <- Registry$new()
