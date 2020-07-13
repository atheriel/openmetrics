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
