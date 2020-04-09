#' Metrics for Shiny Applications
#'
#' @description
#'
#' Automatically wrap a Shiny app, adding metrics for the current session count
#' and the duration of reactive flushes, and then expose them on a `/metrics`
#' endpoint.
#'
#' The endpoint will check the `METRICS_HTTP_AUTHORIZATION` environment
#' variable, and if present will use it as the expected
#' [`Authorization`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
#' header of the request to the `/metrics` endpoint. This can be used to
#' implement basic HTTP authentication for access to runtime metrics.
#'
#' @param app An object created with [shiny::shinyApp()].
#' @param registry A `Registry` object. See [registry()].
#' @return A modified Shiny app object.
#'
#' @export
register_shiny_metrics <- function(app, registry = openmetrics::global_registry()) {
  stopifnot(inherits(app, "shiny.appobj"))

  # Grab Shiny internals. This is a hack to avoid triggering R CMD check warnings
  # for the use of shiny:::*.
  httpResponse <- get0("httpResponse", envir = asNamespace("shiny"))
  handlerManager <- get0("handlerManager", envir = asNamespace("shiny"))
  if (is.null(httpResponse) || is.null(handlerManager)) {
    stop("This version of Shiny is not supported.")
  }

  session_count <- gauge_metric(
    "shiny_sessions", "Running count of open Shiny sessions.",
    registry = registry
  )
  duration <- histogram_metric(
    "shiny_flush_duration_seconds", "Duration of reactive flushes, in seconds.",
    # This seems to be in the right range for most flushes.
    buckets = c(0.0001, 0.00025, 0.0005, 0.001, 0.0025, 0.01),
    registry = registry
  )

  flush_timestamp <- NULL
  server_fun <- app$serverFuncSource()
  app$serverFuncSource <- function() {
    function(input, output, session) {
      session_count$inc() # Up the session count.

      # Collect metrics on various Shiny events.
      shiny::onSessionEnded(function() {
        session_count$dec()
      })
      shiny::onFlush(function() {
        flush_timestamp <<- unclass(Sys.time())
      }, once = FALSE)
      shiny::onFlushed(function() {
        duration$observe(unclass(Sys.time()) - flush_timestamp)
      }, once = FALSE)

      server_fun(input, output, session)
    }
  }

  metrics_handler <- function(req) {
    if (req$PATH_INFO != "/metrics") {
      NULL
    } else {
      # Check for authorization, if set.
      auth_header <- Sys.getenv("METRICS_HTTP_AUTHORIZATION")
      if (nchar(auth_header) > 0 && (is.null(req$HTTP_AUTHORIZATION) ||
                                     req$HTTP_AUTHORIZATION != auth_header)) {
        httpResponse(
          content_type = "text/plain", status = 401L,
          headers = list(
            "Content-Type" = "text/plain",
            "WWW-Authenticate" = "Basic realm=\"Runtime metrics\""
          ),
          content = registry$render_all()
        )
      } else {
        httpResponse(
          content_type = "text/plain;version=0.0.4",
          content = registry$render_all()
        )
      }
    }
  }

  handlerManager$addHandler(metrics_handler, "/metrics")
  app
}
