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
    render_metrics(reg),
    '# HELP count Custom counter.
# TYPE count counter
count_total 1
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
# EOF'
  )

  testthat::expect_silent(reg$reset_all())
})

testthat::test_that("Metrics with labels work as expected", {
  reg <- registry()
  testthat::expect_equal(length(collect_metrics(reg)), 0)

  counter <- counter_metric(
    "count", "Custom counter.", method = "GET", endpoint = "/", registry = reg
  )
  testthat::expect_equal(counter$inc(5), 5)
  testthat::expect_equal(counter$inc(ignored = "value"), 6)
  testthat::expect_equal(counter$inc(method = "POST"), 1)

  gauge <- gauge_metric(
    "value", "Custom gauge.", method = "GET", endpoint = "/", registry = reg
  )
  testthat::expect_equal(gauge$set(5), 5)
  testthat::expect_equal(gauge$set(10, ignored = "value"), 10)
  testthat::expect_equal(gauge$inc(method = "POST"), 1)

  hist <- histogram_metric(
    "dist", "Custom histogram.", method = "GET", endpoint = "/", registry = reg
  )
  testthat::expect_equal(hist$observe(51.7), 51.7)
  testthat::expect_equal(hist$observe(10, ignored = "value"), 61.7)
  testthat::expect_equal(hist$observe(100, method = "POST"), 100)

  # Invalid metric labels.
  testthat::expect_error(
    counter_metric("name", "Help text.", ".invalid" = "0", registry = reg),
    regexp = "One or more invalid metric labels"
  )

  testthat::expect_equal(
    render_metrics(reg),
    '# HELP count Custom counter.
# TYPE count counter
count_total{method="GET",endpoint="/"} 6
count_total{method="POST",endpoint="/"} 1
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
