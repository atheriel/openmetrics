testthat::test_that("Labels work as expected", {
  valid <- list(colour = "red", month = "aug")
  available <- c("colour", "month")

  # Ingesting valid labels.
  testthat::expect_equal(parse_labels(list()), NULL)
  testthat::expect_equal(parse_labels(valid), valid)

  # Catching invalid labels.
  testthat::expect_error(
    parse_labels(list("red", "aug")),
    regexp = "All labels must be named"
  )
  testthat::expect_error(
    parse_labels(list(colour = "red", "aug")),
    regexp = "All labels must be named"
  )
  testthat::expect_error(
    parse_labels(list(colour = c("red", "blue"))),
    regexp = "All labels must be strings"
  )
  testthat::expect_error(
    parse_labels(list(colour = 1)),
    regexp = "All labels must be strings"
  )

  # Validating labels.
  testthat::expect_equal(validate_labels(valid, available), valid)
  testthat::expect_error(
    validate_labels(NULL, available),
    regexp = "One or more missing metric labels"
  )
  testthat::expect_error(
    validate_labels(list(colour = "yellow"), available),
    regexp = "One or more missing metric labels"
  )

  # Encoding.
  testthat::expect_equal(encode_labels(valid), 'colour="red",month="aug"')
})

testthat::test_that("Escaping works as expected", {
  misbehaved <- list(unescaped = "cats\n a\\nd \"dogs\"\n", fine = "text")
  testthat::expect_equal(
    encode_labels(misbehaved),
    'unescaped="cats\\n a\\\\nd \\\"dogs\\\"\\n",fine="text"'
  )
})
