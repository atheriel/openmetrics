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

  out <- collect_metrics(reg)
  testthat::expect_equal(length(out), 2)
  testthat::expect_equal(
    reg$render_all(),
    "# HELP custom Custom counter.
# TYPE custom counter
custom_total 1

# HELP custom Custom gauge.
# TYPE custom gauge
custom 1
"
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
'
  )
})
