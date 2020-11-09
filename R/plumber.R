#' Metrics for Plumber APIs
#'
#' @description
#'
#' Automatically wrap a Plumber API app, adding metrics for HTTP request count
#' and duration, and then expose them on a `/metrics` endpoint.
#'
#' The endpoint will check the `METRICS_HTTP_AUTHORIZATION` environment
#' variable, and if present will use it as the expected
#' [`Authorization`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
#' header of the request to the `/metrics` endpoint. This can be used to
#' implement basic HTTP authentication for access to runtime metrics.
#'
#' @param app A Plumber router object.
#' @param registry A `Registry` object. See [registry()].
#' @return A modified Plumber router.
#'
#' @examples
#' if (requireNamespace("plumber", quietly = TRUE)) {
#'   app <- plumber::plumber$new() # Normally this is plumber::plumb().
#'   app <- register_plumber_metrics(app)
#' \dontrun{
#'   app$run()
#' }
#' }
#' @export
register_plumber_metrics <- function(app, registry = global_registry()) {
  stopifnot(inherits(app, c("plumber", "Plumber")))

  # Define two simple metrics. The most common conventions seem to be:
  #
  # * The http_* namespace.
  # * Uppercase HTTP verbs in a "method" label.
  # * HTTP status as a "status" label.
  # * The path/route/endpoint as "path".
  # * Second precision for request duration and the default buckets.
  #
  # See:
  # * https://pypi.org/project/prometheus-flask-exporter/
  # * https://swaggerstats.io/guide/prometheus.html#metrics
  # * https://www.npmjs.com/package/prometheus-api-metrics
  # * https://github.com/tdeekens/promster
  requests <- counter_metric(
    "http_request", "Running total of HTTP requests.",
    labels = c("path", "method", "status"), registry = registry
  )
  duration <- histogram_metric(
    "http_request_duration_seconds", "Duration of HTTP requests, in seconds.",
    # These are what node.js's prom-client uses.
    buckets = c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10),
    labels = c("path", "method", "status"), registry = registry
  )

  preroute_hook <- function(req, res) {
    # This is a bit of a hack, but it works nicely. Hopefully support is added
    # to httpuv directly: https://github.com/rstudio/httpuv/pull/262
    req$start_time <- unclass(Sys.time())
  }

  postroute_hook <- function(req, res) {
    http_status <- as.character(res$status)
    http_method <- tolower(req$REQUEST_METHOD)
    elapsed <- unclass(Sys.time()) - req$start_time
    requests$inc(
      path = req$PATH_INFO, method = req$REQUEST_METHOD, status = http_status
    )
    duration$observe(
      elapsed, path = req$PATH_INFO, method = req$REQUEST_METHOD,
      status = http_status
    )

    invisible(NULL)
  }

  app$registerHook("preroute", preroute_hook)
  app$registerHook("postroute", postroute_hook)
  app$handle("GET", "/metrics", function(req, res) {
    # Check for authorization, if set.
    auth_header <- Sys.getenv("METRICS_HTTP_AUTHORIZATION")
    if (nchar(auth_header) > 0 && (is.null(req$HTTP_AUTHORIZATION) ||
                                   req$HTTP_AUTHORIZATION != auth_header)) {
      res$setHeader("Content-Type", "text/plain")
      res$setHeader("WWW-Authenticate", "Basic realm=\"Runtime metrics\"")
      res$status <- 401L
      res$body <- "Unauthorized"
      return(res)
    }
    res$status <- 200L

    if (!is.null(req$HTTP_ACCEPT) &&
        grepl("application/openmetrics-text", req$HTTP_ACCEPT, fixed = TRUE)) {
      res$setHeader("Content-Type", .content_type)
      res$body <- registry$render_all()
    } else {
      res$setHeader("Content-Type", .legacy_content_type)
      res$body <- registry$render_all(format = "legacy")
    }

    res
  })

  app
}
