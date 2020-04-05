testthat::test_that("Labels work as expected", {
  valid <- list(colour = "red", month = "aug")
  default <- list(colour = "blue", month = "may")

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

  # Merging labels.
  testthat::expect_equal(merge_labels(valid, default), valid)
  testthat::expect_equal(merge_labels(NULL, default), default)
  testthat::expect_equal(
    merge_labels(list(colour = "yellow"), default),
    list(colour = "yellow", month = "may")
  )
  testthat::expect_equal(
    merge_labels(list(new = "present"), default),
    default
  )
  testthat::expect_equal(
    merge_labels(list(colour = "yellow", new = "present"), default),
    list(colour = "yellow", month = "may")
  )

  # Encoding.
  testthat::expect_equal(encode_labels(valid), 'colour="red",month="aug"')
})
