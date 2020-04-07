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

#' @param buckets A sequence of buckets to bin observations into.
#' @rdname metrics
#' @export
histogram_metric <- function(name, help, buckets = 2 ^ (0:11), ...,
                             registry = global_registry()) {
  existing <- registry$metric(name, type = "histogram")
  if (is.null(existing)) {
    Histogram$new(
      name = name, help = help, buckets = buckets, ..., registry = registry
    )
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
      private$registry <- registry
      private$labels <- parse_labels(list(...))

      # We only need to check spec validity for label names once.
      valid <- grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", names(private$labels))
      if (!all(valid)) {
        stop(
          "One or more invalid metric labels: '",
          paste(names(private$labels)[!valid], collapse = "', '"), "'."
        )
      }

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
      if (is.null(private$labels)) {
        # Fast version, just a value.
        private$value <- 0
      } else {
        private$value <- new.env(parent = emptyenv())
        # Initialize the counter for the default labels.
        private$value[[encode_labels(private$labels)]] <- 0
      }
    },

    render = function() {
      if (is.null(private$labels)) {
        sprintf(
          "%s\n%s_total %s\n", private$header(), private$name, private$value
        )
      } else {
        entries <- vapply(ls(private$value), function(key) {
          sprintf("%s_total{%s} %s", private$name, key, private$value[[key]])
        }, character(1))
        sprintf(
          "%s\n%s\n", private$header(), paste(entries, collapse = "\n")
        )
      }
    },

    inc = function(by = 1, ...) {
      stopifnot(by > 0)
      if (is.null(private$labels)) {
        private$value <- private$value + by
      } else {
        key <- encode_labels(merge_labels(list(...), private$labels))
        value <- private$value[[key]]
        if (is.null(value)) {
          private$value[[key]] <- by
        } else {
          private$value[[key]] <- value + by
        }
      }
    },

    reset = function() {
      if (is.null(private$labels)) {
        private$value <- 0
      } else {
        private$value <- new.env(parent = emptyenv())
        private$value[[encode_labels(private$labels)]] <- 0
      }
    }
  ),
  private = list(
    value = NULL
  )
)

Gauge <- R6::R6Class(
  "Gauge", inherit = Metric,
  public = list(
    initialize = function(name, help, ..., registry = global_registry()) {
      super$initialize(name, help, type = "gauge", ..., registry = registry)
      if (is.null(private$labels)) {
        # Fast version, just a value.
        private$value <- 0
      } else {
        private$value <- new.env(parent = emptyenv())
        private$value[[encode_labels(private$labels)]] <- 0
      }
    },

    render = function() {
      if (is.null(private$labels)) {
        sprintf(
          "%s\n%s %s\n", private$header(), private$name, private$value
        )
      } else {
        entries <- vapply(ls(private$value), function(key) {
          sprintf("%s{%s} %s", private$name, key, private$value[[key]])
        }, character(1))
        sprintf(
          "%s\n%s\n", private$header(), paste(entries, collapse = "\n")
        )
      }
    },

    inc = function(by = 1, ...) {
      if (is.null(private$labels)) {
        private$value <- private$value + by
      } else {
        key <- encode_labels(merge_labels(list(...), private$labels))
        value <- private$value[[key]]
        if (is.null(value)) {
          private$value[[key]] <- by
        } else {
          private$value[[key]] <- value + by
        }
      }
    },

    dec = function(by = 1, ...) {
      if (is.null(private$labels)) {
        private$value <- private$value - by
      } else {
        key <- encode_labels(merge_labels(list(...), private$labels))
        value <- private$value[[key]]
        if (is.null(value)) {
          private$value[[key]] <- -by
        } else {
          private$value[[key]] <- value - by
        }
      }
    },

    set = function(value, ...) {
      stopifnot(is.numeric(value))
      if (is.null(private$labels)) {
        private$value <- value
      } else {
        key <- encode_labels(merge_labels(list(...), private$labels))
        private$value[[key]] <- value
      }
    },

    set_to_current_time = function(...) {
      self$set(value = unclass(Sys.time(), ...))
    },

    reset = function() {
      if (is.null(private$labels)) {
        private$value <- 0
      } else {
        private$value <- new.env(parent = emptyenv())
        private$value[[encode_labels(private$labels)]] <- 0
      }
    }
  ),
  private = list(
    value = NULL
  )
)

Histogram <- R6::R6Class(
  "Histogram", inherit = Metric,
  public = list(
    initialize = function(name, help, buckets = 2 ^ (0:11), ...,
                          registry = global_registry()) {
      super$initialize(name, help, type = "histogram", ..., registry = registry)
      buckets <- sort(buckets)
      private$buckets <- c(buckets, Inf)
      # Note: Buckets without a .0 seem to need them to satisfy the parser, but
      # otherwise the usual numeric formatting seems to be acceptable.
      buckets_str <- as.character(buckets)
      have_digits <- abs(buckets - trunc(buckets)) > 0
      buckets_str[!have_digits] <- sprintf("%.1f", buckets[!have_digits])
      private$le <- c(buckets_str, "+Inf")
      self$reset()
    },

    render = function() {
      if (is.null(private$labels)) {
        buckets <- paste0(
          private$name, "_bucket{le=\"", private$le, "\"} ", private$dist
        )
        sums <- sprintf("%s_sum %s", private$name, private$sum)
        count <- sprintf("%s_count %s", private$name, private$count)
        sprintf(
          "%s\n%s\n", private$header(),
          paste(c(buckets, sums, count), collapse = "\n")
        )
      } else {
        # Matching labels need to be printed together to satisfy the OpenMetrics
        # parser.
        keys <- ls(private$dist)
        blocks <- character(length(keys))
        for (i in 1:length(keys)) {
          key <- keys[i]
          buckets <- paste0(
            private$name, "_bucket{", key, ",le=\"", private$le, "\"} ",
            private$dist[[key]]
          )
          sums <- sprintf("%s_sum{%s} %s", private$name, key, private$sum[[key]])
          count <- sprintf(
            "%s_count{%s} %s", private$name, key, private$count[[key]]
          )
          blocks[i] <- paste(c(buckets, sums, count), collapse = "\n")
        }
        sprintf(
          "%s\n%s\n", private$header(), paste(blocks, collapse = "\n")
        )
      }
    },

    observe = function(value, ...) {
      stopifnot(is.numeric(value))
      dist <- as.integer(value <= private$buckets)

      # Update the running sum and count values.
      if (is.null(private$labels)) {
        private$dist <- private$dist + dist
        private$count <- private$count + 1
        private$sum <- private$sum + value
      } else {
        key <- encode_labels(merge_labels(list(...), private$labels))
        current <- private$dist[[key]]
        # We know that if one entry is missing they all are.
        if (is.null(current)) {
          private$dist[[key]] <- dist
          private$count[[key]] <- 1
          private$sum[[key]] <- value
        } else {
          private$dist[[key]] <- private$dist[[key]] + dist
          private$count[[key]] <- private$count[[key]] + 1
          private$sum[[key]] <- private$sum[[key]] + value
        }
      }
    },

    reset = function() {
      if (is.null(private$labels)) {
        private$sum <- 0
        private$count <- 0
        private$dist <- rep(0, times = length(private$buckets))
      } else {
        private$dist <- new.env(parent = emptyenv())
        private$sum <- new.env(parent = emptyenv())
        private$count <- new.env(parent = emptyenv())
        key <- encode_labels(private$labels)
        private$sum[[key]] <- 0
        private$count[[key]] <- 0
        private$dist[[key]] <- rep(0, times = length(private$buckets))
      }
    }
  ),
  private = list(
    buckets = numeric(), le = character(), dist = NULL, sum = NULL, count = NULL
  )
)

parse_labels <- function(labels) {
  if (length(labels) == 0) {
    return(NULL)
  }
  if (is.null(names(labels)) || any(nchar(names(labels)) == 0)) {
    stop("All labels must be named.", call. = FALSE)
  }
  strings <- vapply(
    labels, function(x) is.character(x) && length(x) == 1, logical(1)
  )
  if (!all(strings)) {
    stop("All labels must be strings.", call. = FALSE)
  }
  labels
}

merge_labels <- function(labels, defaults) {
  if (length(labels) == 0) {
    defaults
  } else {
    out <- utils::modifyList(defaults, parse_labels(labels))
    out[names(out) %in% names(defaults)]
  }
}

encode_labels <- function(labels) {
  paste0(names(labels), "=\"", labels, "\"", collapse = ",")
}
