# openmetrics 0.3.0

## Breaking Changes

* Label names should now be passed as the `labels` parameter when creating a
  metric, instead of being inferred from the remaining named arguments. For
  consistency with other clients, there is no longer a notion of a "default"
  label value. This also makes it possible to have labels like `name`, `help`,
  `type`, and so on that conflict with existing parameter names.

* Relatedly, all labels must now be specified when calling methods on a metric
  (such as `inc()` or `set()`), since there is no default to fall back on.

## Other Improvements

* The package now has its own hex logo.

* Only generate the OpenMetrics format when the (preliminary) OpenMetrics
  content-type is passed in the `Accept` header. Otherwise, fall back on the
  Prometheus format and content-type (#3).

* Pushgateway functions will now `stop()` for HTTP errors instead of merely
  issuing a warning. Clients that can tolerate these errors will likely
  `tryCatch()` them anyway, since `httr::RETRY()` can fail for other reasons.

* Requests made to Shiny apps that return HTTP 401 will no longer include the
  rendered metrics anyway.

* The `"`, `\n`, and `\` characters are now escaped in labels and help text, as
  in the OpenMetrics reference implementation (#1).

* Counter metrics can now be incremented by zero.

* Counter, Gauge, and Histogram metrics can now take an optional `unit`
  parameter. The provided unit *must* match the one in the name of the metric
  itself -- e.g. a `received_bytes` metric must have unit "bytes" (#2).

* Counters and Histograms now have a `_created` child metric, as per the
  OpenMetrics draft specification (#2).

# openmetrics 0.2.0

* Add `push_to_gateway()` and `delete_from_gateway()` for manually pushing
  metrics to a [Prometheus Pushgateway](https://prometheus.io/docs/instrumenting/pushing/)
  instance.

* Histogram metrics now have a built-in `time()` method for collecting
  observations on the duration of an expression.

* The built-in `process_cpu_seconds_total` metric is now correctly reported as
  a Counter instead of a Gauge. This fixes an inconsistency with other
  Prometheus clients that would prevent metrics being used with Pushgateway.

# openmetrics 0.1.1

* Fixes inaccurate CPU usage when using `register_default_metrics()`.
  Previously, this would include the total elapsed time for the process (as
  opposed to the CPU time that was actually used).

# openmetrics 0.1.0

* Initial release. **openmetrics** is an opinionated Prometheus and OpenMetrics
  client for R.

* Automatically instrument Plumber apps with `register_plumber_metrics()` and
  Shiny apps with `register_shiny_metrics()`.

* Collect default metrics with `register_default_metrics()`.

* Create custom metrics with `counter_metric()`, `gauge_metric()`, and
  `histogram_metric()` and custom registries with `registry()`.

* Write metrics in the OpenMetrics/Prometheus text format to any location with
  `render_metrics()`.
