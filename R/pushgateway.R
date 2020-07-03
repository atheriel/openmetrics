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
#' @param ... Currently ignored.
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
  if (is.na(instance)) {
    path <- sprintf("/metrics/job/%s", job)
  } else {
    path <- sprintf("/metrics/job/%s/instance/%s", job, instance)
  }
  rendered <- render_metrics(registry = registry)
  # Pushgateway seems to barf on OpenMetric's "# EOF" trailer, so scrub that.
  rendered <- sub("# EOF", "", rendered, fixed = TRUE)
  response <- httr::RETRY(
    "POST", url, path = path, body = rendered, encode = "form",
    # Pushgateway will send a 400 if the metric conflicts with an existing one,
    # in which case we don't want to retry (since it will always fail).
    terminate_on = 400
  )
  httr::warn_for_status(response)
  invisible(NULL)
}

#' @rdname pushgateway
#' @export
delete_from_gateway <- function(url, job, instance = NA, ...) {
  if (is.na(instance)) {
    path <- sprintf("/metrics/job/%s", job)
  } else {
    path <- sprintf("/metrics/job/%s/instance/%s", job, instance)
  }
  response <- httr::RETRY("DELETE", url, path = path)
  httr::warn_for_status(response)
  invisible(NULL)
}
