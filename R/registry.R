#' Metric Registries
#'
#' @description
#'
#' A registry is a collection of one or more metrics. By default, metrics are
#' added to the `global_registry()`, but new registries can also be created with
#' `registry()`. Use `collect_metrics()` to return all metrics that a registry
#' is aware of (usually for rendering).
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
    },

    register = function(name, type, metric) {
      row <- which(private$index$name == name & private$index$type == type)
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

    unregister = function(name, type) {
      row <- which(private$index$name == name & private$index$type == type)
      if (length(row) != 0) {
        # We don't need to modify the data frame.
        private$metrics[[row]] <- NULL
      }
    },

    metric = function(name, type) {
      row <- which(private$index$name == name & private$index$type == type)
      if (length(row) != 0) {
        private$metrics[[row]]
      } else {
        NULL
      }
    },

    collect = function() {
      private$metrics[vapply(private$metrics, length, integer(1)) != 0]
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
    index = NULL, metrics = NULL
  )
)

.registry <- Registry$new()
