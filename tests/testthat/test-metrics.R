testthat::test_that("Metrics work as expected", {
  reg <- registry()
  testthat::expect_equal(length(collect_metrics(reg)), 0)

  counter <- counter_metric("custom", "Custom counter.", registry = reg)
  testthat::expect_equal(counter$inc(5), 5)
  testthat::expect_equal(counter$inc(), 6)
  counter$reset()
  testthat::expect_equal(counter$inc(), 1)

  # Previously defined metrics are available under the same name.
  testthat::expect_identical(counter, counter_metric("custom", registry = reg))

  # Invalid metric names.
  testthat::expect_error(
    counter_metric(".invalid", "Help text.", registry = reg),
    regexp = "Invalid metric name"
  )

  dropped <- counter_metric("dropped", "Not included.")
  dropped$unregister()

  gauge <- gauge_metric("custom", "Custom gauge.", registry = reg)
  gauge$set(51.7)
  testthat::expect_equal(gauge$dec(), 50.7)
  gauge$reset()
  testthat::expect_equal(gauge$inc(), 1)

  hist <- histogram_metric("dist", "Custom histogram.", registry = reg)
  testthat::expect_equal(hist$observe(51.7), 51.7)
  testthat::expect_equal(hist$observe(10), 61.7)
  testthat::expect_equal(hist$observe(600), 661.7)

  out <- collect_metrics(reg)
  testthat::expect_equal(length(out), 3)
  testthat::expect_equal(
    reg$render_all(),
    '# HELP custom Custom counter.
# TYPE custom counter
custom_total 1
# HELP custom Custom gauge.
# TYPE custom gauge
custom 1
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
    "custom", "Custom counter.", method = "GET", endpoint = "/", registry = reg
  )
  testthat::expect_equal(counter$inc(5), 5)
  testthat::expect_equal(counter$inc(ignored = "value"), 6)
  testthat::expect_equal(counter$inc(method = "POST"), 1)

  gauge <- gauge_metric(
    "custom", "Custom gauge.", method = "GET", endpoint = "/", registry = reg
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
    reg$render_all(),
    '# HELP custom Custom counter.
# TYPE custom counter
custom_total{method="GET",endpoint="/"} 6
custom_total{method="POST",endpoint="/"} 1
# HELP custom Custom gauge.
# TYPE custom gauge
custom{method="GET",endpoint="/"} 10
custom{method="POST",endpoint="/"} 1
# HELP dist Custom histogram.
# TYPE dist histogram
dist_bucket{method="GET",endpoint="/",le="1.0"} 0
dist_bucket{method="GET",endpoint="/",le="2.0"} 0
dist_bucket{method="GET",endpoint="/",le="4.0"} 0
dist_bucket{method="GET",endpoint="/",le="8.0"} 0
dist_bucket{method="GET",endpoint="/",le="16.0"} 1
dist_bucket{method="GET",endpoint="/",le="32.0"} 1
dist_bucket{method="GET",endpoint="/",le="64.0"} 2
dist_bucket{method="GET",endpoint="/",le="128.0"} 2
dist_bucket{method="GET",endpoint="/",le="256.0"} 2
dist_bucket{method="GET",endpoint="/",le="512.0"} 2
dist_bucket{method="GET",endpoint="/",le="1024.0"} 2
dist_bucket{method="GET",endpoint="/",le="2048.0"} 2
dist_bucket{method="GET",endpoint="/",le="+Inf"} 2
dist_sum{method="GET",endpoint="/"} 61.7
dist_count{method="GET",endpoint="/"} 2
dist_bucket{method="POST",endpoint="/",le="1.0"} 0
dist_bucket{method="POST",endpoint="/",le="2.0"} 0
dist_bucket{method="POST",endpoint="/",le="4.0"} 0
dist_bucket{method="POST",endpoint="/",le="8.0"} 0
dist_bucket{method="POST",endpoint="/",le="16.0"} 0
dist_bucket{method="POST",endpoint="/",le="32.0"} 0
dist_bucket{method="POST",endpoint="/",le="64.0"} 0
dist_bucket{method="POST",endpoint="/",le="128.0"} 1
dist_bucket{method="POST",endpoint="/",le="256.0"} 1
dist_bucket{method="POST",endpoint="/",le="512.0"} 1
dist_bucket{method="POST",endpoint="/",le="1024.0"} 1
dist_bucket{method="POST",endpoint="/",le="2048.0"} 1
dist_bucket{method="POST",endpoint="/",le="+Inf"} 1
dist_sum{method="POST",endpoint="/"} 100
dist_count{method="POST",endpoint="/"} 1
# EOF'
  )
})
