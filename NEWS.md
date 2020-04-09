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