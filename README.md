
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openmetrics

<!-- badges: start -->

<!-- badges: end -->

**openmetrics** is an opinionated client for
[Prometheus](https://prometheus.io/) and the related
[OpenMetrics](https://openmetrics.io/) project. It makes it possible to
add predefined and custom metrics to any R web application and expose
them on a `/metrics` endpoint, where they can be consumed by Prometheus
services.

The package includes built-in support for
[Plumber](https://www.rplumber.io/) applications, but is highly
extensible.

## Use with Plumber

You can easily wrap an existing Plumber router object to add metrics for
HTTP requests and their duration:

``` r
srv <- plumber::plumb("plumber.R")
srv <- register_plumber_metrics(srv)
srv$run()
```

This will automatically create a `/metrics` endpoint that exposes these
metrics (and any others you have defined).

## Default Process Metrics

You can enable the [default process
metrics](https://prometheus.io/docs/instrumenting/writing_clientlibs/#process-metrics)
(which track CPU time, memory usage, and open files) with

``` r
register_default_metrics()
```

Note that not all metrics are supported (or even meaningful) on all
operating systems.

## Custom Metrics

All required metrics and features of [the informal Prometheus client
specification](https://prometheus.io/docs/instrumenting/writing_clientlibs/)
are supported, so you can create app-specific [counters, gauges, and
histograms](https://prometheus.io/docs/concepts/metric_types/). If you
want to make use of labels, you must pass a default value at the time of
creation.

Here are some examples collected from around the
house:

``` r
meows <- counter_metric("meows", "Heard around the house.", cat = "Unknown")
meows$inc(cat = "Shamus") # Count one meow from Shamus.
meows$inc(3) # Count three meows of unknown origin.

thermostat <- gauge_metric("thermostat", "Thermostat display.")
thermostat$set(21.3) # Read from the display...
thermostat$dec(2) # ... and then turn it down 2 degrees.

temperature <- histogram_metric(
  "temperature", "Ambient room temperature measurements.",
  buckets = c(10, 15, 20, 22, 25), room = "kitchen"
)
set.seed(9090)
# Simulate taking ambient temperature samples.
for (measure in rnorm(20, mean = 21.5)) {
  temperature$observe(measure, room = sample(c("kitchen", "bathroom"), 1))
}
```

All metrics (in fact all exported functions) take a `registry` parameter
in case you want to avoid using the default global registry in some part
of your application. You can construct new registries with `registry()`.

### Performance

Because metrics may be gathered with high frequency, some effort has
been made to ensure that using them is fast – usually around 10
microseconds.

### Rendering Metrics Manually

The `render_metrics()` function exposes the [text-based
format](https://prometheus.io/docs/instrumenting/exposition_formats/#text-based-format)
understood by Prometheus. Here’s what the metrics above look like:

``` r
cat(render_metrics())
```

    # HELP meows Heard around the house.
    # TYPE meows counter
    meows_total{cat="Shamus"} 1
    meows_total{cat="Unknown"} 3
    # HELP thermostat Thermostat display.
    # TYPE thermostat gauge
    thermostat 19.3
    # HELP temperature Ambient room temperature measurements.
    # TYPE temperature histogram
    temperature_bucket{room="bathroom",le="10.0"} 0
    temperature_bucket{room="bathroom",le="15.0"} 0
    temperature_bucket{room="bathroom",le="20.0"} 0
    temperature_bucket{room="bathroom",le="22.0"} 9
    temperature_bucket{room="bathroom",le="25.0"} 11
    temperature_bucket{room="bathroom",le="+Inf"} 11
    temperature_sum{room="bathroom"} 234.387663039796
    temperature_count{room="bathroom"} 11
    temperature_bucket{room="kitchen",le="10.0"} 0
    temperature_bucket{room="kitchen",le="15.0"} 0
    temperature_bucket{room="kitchen",le="20.0"} 1
    temperature_bucket{room="kitchen",le="22.0"} 4
    temperature_bucket{room="kitchen",le="25.0"} 9
    temperature_bucket{room="kitchen",le="+Inf"} 9
    temperature_sum{room="kitchen"} 198.854388891071
    temperature_count{room="kitchen"} 9
    # EOF

You can use this to implement a `/metrics` endpoint in your application
if it is not supported directly. For example, using a raw **httpuv**
server:

``` r
httpuv::runServer(
  "127.0.0.1", 8080,
  list(call = function(req) {
    list(
      status = 200L,
      headers = list("Content-Type" = "text/plain; version=0.0.4"),
      body = render_metrics()
    )
  })
)
```

## Limitations

  - The Summary metric is not available, as it is not required by the
    spec if a Histogram is available. It is also difficult to implement,
    and [could be considered a design
    error](https://www.robustperception.io/how-does-a-prometheus-summary-work).

  - No support yet for the Prometheus
    [Pushgateway](https://prometheus.io/docs/instrumenting/pushing/).

  - Most of the default process metrics are absent on non-Linux systems.
    This is basically because they were designed with Linux in mind, and
    very few Prometheus clients support them for anything but Linux.