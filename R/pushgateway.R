#' Pushgateway Integration
#'
#' @description
#'
#' Some workloads may not want to run an HTTP server to expose metrics,
#' especially in the case of short-lived batch jobs. For these cases metrics
#' can also be manually "pushed" to a Prometheus Pushgateway instance, though
#' [there are drawbacks](https://prometheus.io/docs/practices/pushing/).
#'
#' `push_to_gateway()` is used to push metrics, and `delete_from_gateway()` is
#' used to clean them up when the workload is finished.
#'
#' @param url The URL of the Pushgateway
#' @param job A value for the `job` label applied to all pushed metrics.
#' @param instance A value for the `instance` label applied to all pushed
#'   metrics, or `NA` to leave it unset.
#' @param registry A `Registry` object, defaulting to the shared global one.
#' @param ... Additional named string arguments converted to labels. Beware
#'   that these are not yet checked for URL safety.
#'
#' @return `NULL`, invisibly.
#'
#' @examples
#' \dontrun{
#' register_default_metrics()
#' push_to_gateway("localhost:9091", job = "batch-job-1")
#' # Some time later...
#' delete_from_gateway("localhost:9091", job = "batch-job-1")
#' }
#'
#' @rdname pushgateway
#' @export
push_to_gateway <- function(url, job, instance = NA,
                            registry = global_registry(), ...) {
  labels <- pushgateway_labels(job = job, instance = instance, ...)
  path <- sprintf(
    "/metrics/%s", paste(names(labels), labels, sep = "/", collapse = "/")
  )
  rendered <- registry$render_all(format = "legacy")
  response <- httr::RETRY(
    "POST", url, path = path, body = rendered, encode = "form",
    # Pushgateway does not support the OpenMetrics format.
    config = httr::content_type(.legacy_content_type),
    # Pushgateway will send a 400 if the metric conflicts with an existing one,
    # in which case we don't want to retry (since it will always fail).
    terminate_on = 400
  )
  httr::stop_for_status(response)
  invisible(NULL)
}

#' @rdname pushgateway
#' @export
delete_from_gateway <- function(url, job, instance = NA, ...) {
  labels <- pushgateway_labels(job = job, instance = instance, ...)
  path <- sprintf(
    "/metrics/%s", paste(names(labels), labels, sep = "/", collapse = "/")
  )
  response <- httr::RETRY("DELETE", url, path = path)
  httr::stop_for_status(response)
  invisible(NULL)
}

pushgateway_labels <- function(job, instance, ...) {
  labels <- list(job = job)
  labels$instance <- if (!is.na(instance)) instance
  labels <- c(labels, list(...))
  # TODO: Base64 encode URL-breaking labels.
  parse_labels(labels)
}
