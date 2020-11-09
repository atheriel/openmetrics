testthat::test_that("Pushgateway interaction works as expected", {
  reg <- registry()
  register_default_metrics(reg)

  counter <- counter_metric(
    "count_bytes", "Custom counter.", unit = "bytes", registry = reg
  )
  counter$inc(5)
  hist <- histogram_metric(
    "dist", "Custom histogram.", labels = c("method", "endpoint"),
    registry = reg
  )
  hist$observe(51.7, method = "GET", endpoint = "/")
  hist$observe(10, method = "GET", endpoint = "/",ignored = "value")
  hist$observe(100, method = "POST", endpoint = "/")

  testthat::skip_on_cran()
  testthat::skip_on_ci()

  tryCatch({
    response <- httr::GET("http://localhost:9091", path = "/-/healthy")
    httr::stop_for_status(response)
  }, error = function(e) {
    testthat::skip(
      sprintf("No local Pushgateway server detected: %s.", e$message)
    )
  })

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
