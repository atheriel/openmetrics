testthat::test_that("Pushgateway interaction works as expected", {
  reg <- registry()
  register_default_metrics(reg)

  counter <- counter_metric("count", "Custom counter.", registry = reg)
  counter$inc(5)

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  testthat::expect_silent(
    push_to_gateway(
      "http://localhost:9091", job = "openmetrics-tests-1", registry = reg
    )
  )
  testthat::expect_silent(
    push_to_gateway(
      "http://localhost:9091", job = "openmetrics-tests-1", instance = "2",
      pod = Sys.info()["nodename"], registry = reg
    )
  )
  testthat::expect_silent(
    delete_from_gateway(
      "http://localhost:9091", job = "openmetrics-tests-1", instance = "2",
      pod = Sys.info()["nodename"]
    )
  )
  testthat::expect_silent(
    delete_from_gateway("http://localhost:9091", job = "openmetrics-tests-1")
  )
})
