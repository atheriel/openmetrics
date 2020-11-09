# Since timestamps are not reproducible, set them to a sigil value.
reset_created <- function(rendered) {
  out <- strsplit(rendered, "\n", fixed = TRUE)[[1]]
  out <- gsub("_created(.*)\\s([0-9\\.]+)$", "_created\\1 1", out)
  out <- paste(out, collapse = "\n")
  if (endsWith(out, "EOF")) out else sprintf("%s\n", out)
}

testthat::test_that("Metrics work as expected", {
  reg <- registry()
  testthat::expect_equal(length(collect_metrics(reg)), 0)

  counter <- counter_metric("count", "Custom counter.", registry = reg)
  testthat::expect_equal(counter$inc(5), 5)
  testthat::expect_equal(counter$inc(), 6)
  counter$reset()
  testthat::expect_equal(counter$inc(), 1)

  # Previously defined metrics are available under the same name.
  testthat::expect_identical(counter, counter_metric("count", registry = reg))

  # Invalid metric names.
  testthat::expect_error(
    counter_metric(".invalid", "Help text.", registry = reg),
    regexp = "Invalid metric name"
  )

  # Unregister/re-register behaviour.
  dropped <- counter_metric("dropped", "Not included.")
  testthat::expect_true(dropped$unregister())
  testthat::expect_false(dropped$unregister())
  dropped <- counter_metric("dropped", "Not included.")
  testthat::expect_true(dropped$unregister())

  gauge <- gauge_metric("value", "Custom gauge.", registry = reg)
  gauge$set(51.7)
  testthat::expect_equal(gauge$dec(), 50.7)
  gauge$reset()
  testthat::expect_equal(gauge$inc(), 1)

  hist <- histogram_metric(
    "dist", "Custom histogram.", buckets = 2 ^ (0:11), registry = reg
  )
  testthat::expect_equal(hist$observe(51.7), 51.7)
  testthat::expect_equal(hist$observe(10), 61.7)
  testthat::expect_equal(hist$observe(600), 661.7)

  out <- collect_metrics(reg)
  testthat::expect_equal(length(out), 3)
  testthat::expect_equal(
    reset_created(render_metrics(reg)),
    '# HELP count Custom counter.
# TYPE count counter
count_total 1
count_created 1
# HELP value Custom gauge.
# TYPE value gauge
value 1
# HELP dist Custom histogram.
# TYPE dist histogram
dist_bucket{le="1.0"} 0
dist_bucket{le="2.0"} 0
dist_bucket{le="4.0"} 0
dist_bucket{le="8.0"} 0
dist_bucket{le="16.0"} 1
dist_bucket{le="32.0"} 1
dist_bucket{le="64.0"} 2
dist_bucket{le="128.0"} 2
dist_bucket{le="256.0"} 2
dist_bucket{le="512.0"} 2
dist_bucket{le="1024.0"} 3
dist_bucket{le="2048.0"} 3
dist_bucket{le="+Inf"} 3
dist_sum 661.7
dist_count 3
dist_created 1
# EOF'
  )

  testthat::expect_silent(reg$reset_all())
})

testthat::test_that("Metrics with labels work as expected", {
  reg <- registry()
  testthat::expect_equal(length(collect_metrics(reg)), 0)

  counter <- counter_metric(
    "count", "Custom counter.", labels = c("method", "endpoint"),
    registry = reg
  )
  testthat::expect_equal(counter$inc(5, method = "GET", endpoint = "/"), 5)
  testthat::expect_equal(
    counter$inc(method = "GET", endpoint = "/", ignored = "value"), 6
  )
  testthat::expect_equal(counter$inc(method = "POST", endpoint = "/"), 1)

  gauge <- gauge_metric(
    "value", "Custom gauge.", labels = c("method", "endpoint"), registry = reg
  )
  testthat::expect_equal(gauge$set(5, method = "GET", endpoint = "/"), 5)
  testthat::expect_equal(
    gauge$set(10, method = "GET", endpoint = "/", ignored = "value"), 10
  )
  testthat::expect_equal(gauge$inc(method = "POST", endpoint = "/"), 1)

  hist <- histogram_metric(
    "dist", "Custom histogram.", labels = c("method", "endpoint"),
    registry = reg
  )
  testthat::expect_equal(
    hist$observe(51.7, method = "GET", endpoint = "/"), 51.7
  )
  testthat::expect_equal(
    hist$observe(10, method = "GET", endpoint = "/",ignored = "value"), 61.7
  )
  testthat::expect_equal(
    hist$observe(100, method = "POST", endpoint = "/"), 100
  )

  # Invalid metric labels.
  testthat::expect_error(
    counter_metric("name", "Help text.", ".invalid" = "0", registry = reg),
    regexp = "One or more invalid metric labels"
  )

  testthat::expect_equal(
    reset_created(render_metrics(reg)),
    '# HELP count Custom counter.
# TYPE count counter
count_total{method="GET",endpoint="/"} 6
count_created{method="GET",endpoint="/"} 1
count_total{method="POST",endpoint="/"} 1
count_created{method="POST",endpoint="/"} 1
# HELP value Custom gauge.
# TYPE value gauge
value{method="GET",endpoint="/"} 10
value{method="POST",endpoint="/"} 1
# HELP dist Custom histogram.
# TYPE dist histogram
dist_bucket{method="GET",endpoint="/",le="0.005"} 0
dist_bucket{method="GET",endpoint="/",le="0.01"} 0
dist_bucket{method="GET",endpoint="/",le="0.025"} 0
dist_bucket{method="GET",endpoint="/",le="0.05"} 0
dist_bucket{method="GET",endpoint="/",le="0.1"} 0
dist_bucket{method="GET",endpoint="/",le="0.25"} 0
dist_bucket{method="GET",endpoint="/",le="0.5"} 0
dist_bucket{method="GET",endpoint="/",le="1.0"} 0
dist_bucket{method="GET",endpoint="/",le="2.5"} 0
dist_bucket{method="GET",endpoint="/",le="5.0"} 0
dist_bucket{method="GET",endpoint="/",le="10.0"} 1
dist_bucket{method="GET",endpoint="/",le="+Inf"} 2
dist_sum{method="GET",endpoint="/"} 61.7
dist_count{method="GET",endpoint="/"} 2
dist_created{method="GET",endpoint="/"} 1
dist_bucket{method="POST",endpoint="/",le="0.005"} 0
dist_bucket{method="POST",endpoint="/",le="0.01"} 0
dist_bucket{method="POST",endpoint="/",le="0.025"} 0
dist_bucket{method="POST",endpoint="/",le="0.05"} 0
dist_bucket{method="POST",endpoint="/",le="0.1"} 0
dist_bucket{method="POST",endpoint="/",le="0.25"} 0
dist_bucket{method="POST",endpoint="/",le="0.5"} 0
dist_bucket{method="POST",endpoint="/",le="1.0"} 0
dist_bucket{method="POST",endpoint="/",le="2.5"} 0
dist_bucket{method="POST",endpoint="/",le="5.0"} 0
dist_bucket{method="POST",endpoint="/",le="10.0"} 0
dist_bucket{method="POST",endpoint="/",le="+Inf"} 1
dist_sum{method="POST",endpoint="/"} 100
dist_count{method="POST",endpoint="/"} 1
dist_created{method="POST",endpoint="/"} 1
# EOF'
  )
})

testthat::test_that("Default process metrics work as expected", {
  supported <- if(grepl("linux", R.version$os, fixed = TRUE)) 6 else 1
  reg <- registry()
  collector <- register_default_metrics(reg)
  testthat::expect_equal(length(collect_metrics(reg)), supported)
  testthat::expect_identical(register_default_metrics(reg), collector)

  # Unregister support, not yet exported.
  testthat::expect_equal(collector$unregister(), supported)
  testthat::expect_equal(length(collect_metrics(reg)), 0)
})

testthat::test_that("Counter metrics render correctly in legacy format", {
  reg <- registry()

  counter <- counter_metric(
    "count", "Custom counter.", registry = reg
  )
  counter$inc()

  counter <- counter_metric(
    "count2", "Custom counter.", entity = "", registry = reg
  )
  counter$inc(entity = "framework")

  testthat::expect_equal(
    reset_created(reg$render_all(format = "legacy")),
    '# HELP count_total Custom counter.
# TYPE count_total counter
count_total 1
# TYPE count_total_created gauge
count_total_created 1
# HELP count2_total Custom counter.
# TYPE count2_total counter
count2_total{entity="framework"} 1
# TYPE count2_total_created gauge
count2_total_created{entity="framework"} 1
'
  )
})

testthat::test_that("Histogram metrics render correctly in legacy format", {
  reg <- registry()

  hist <- histogram_metric(
    "dist", "Custom histogram.", labels = c("method", "endpoint"),
    registry = reg
  )
  hist$observe(51.7, method = "GET", endpoint = "/")
  hist$observe(10, method = "GET", endpoint = "/",ignored = "value")
  hist$observe(100, method = "POST", endpoint = "/")

  testthat::expect_equal(
    reset_created(reg$render_all(format = "legacy")),
    '# HELP dist Custom histogram.
# TYPE dist histogram
dist_bucket{method="GET",endpoint="/",le="0.005"} 0
dist_bucket{method="GET",endpoint="/",le="0.01"} 0
dist_bucket{method="GET",endpoint="/",le="0.025"} 0
dist_bucket{method="GET",endpoint="/",le="0.05"} 0
dist_bucket{method="GET",endpoint="/",le="0.1"} 0
dist_bucket{method="GET",endpoint="/",le="0.25"} 0
dist_bucket{method="GET",endpoint="/",le="0.5"} 0
dist_bucket{method="GET",endpoint="/",le="1.0"} 0
dist_bucket{method="GET",endpoint="/",le="2.5"} 0
dist_bucket{method="GET",endpoint="/",le="5.0"} 0
dist_bucket{method="GET",endpoint="/",le="10.0"} 1
dist_bucket{method="GET",endpoint="/",le="+Inf"} 2
dist_sum{method="GET",endpoint="/"} 61.7
dist_count{method="GET",endpoint="/"} 2
dist_bucket{method="POST",endpoint="/",le="0.005"} 0
dist_bucket{method="POST",endpoint="/",le="0.01"} 0
dist_bucket{method="POST",endpoint="/",le="0.025"} 0
dist_bucket{method="POST",endpoint="/",le="0.05"} 0
dist_bucket{method="POST",endpoint="/",le="0.1"} 0
dist_bucket{method="POST",endpoint="/",le="0.25"} 0
dist_bucket{method="POST",endpoint="/",le="0.5"} 0
dist_bucket{method="POST",endpoint="/",le="1.0"} 0
dist_bucket{method="POST",endpoint="/",le="2.5"} 0
dist_bucket{method="POST",endpoint="/",le="5.0"} 0
dist_bucket{method="POST",endpoint="/",le="10.0"} 0
dist_bucket{method="POST",endpoint="/",le="+Inf"} 1
dist_sum{method="POST",endpoint="/"} 100
dist_count{method="POST",endpoint="/"} 1
# TYPE dist_created gauge
dist_created{method="GET",endpoint="/"} 1
dist_created{method="POST",endpoint="/"} 1
'
  )
})

testthat::test_that("Histogram metrics with no samples render correctly", {
  reg <- registry()

  hist <- histogram_metric(
    "dist", "Custom histogram.", labels = c("method", "endpoint"),
    registry = reg
  )

  testthat::expect_equal(reg$render_all(), "# EOF")
  testthat::expect_equal(reg$render_all(format = "legacy"), "")
})

testthat::test_that("Metric units work as expected", {
  reg <- registry()

  testthat::expect_error(
    counter_metric(
      "count_bytes", "Custom counter.", unit = "seconds", registry = reg
    ),
    regexp = "Metric name does not match unit"
  )

  counter <- counter_metric(
    "count_seconds", "Custom counter.", unit = "seconds", registry = reg
  )
  testthat::expect_equal(
    reset_created(render_metrics(reg)),
    '# HELP count_seconds Custom counter.
# TYPE count_seconds counter
# UNIT count_seconds seconds
count_seconds_total 0
count_seconds_created 1
# EOF'
  )
})
