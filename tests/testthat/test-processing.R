################################################################################
##### unit and integration tests for data processing functions
##### main files: R/processing.R, R/processing_misc.R

################################################################################
##### divisor
testthat::test_that("divisor", {
  # expect no error with integer
  testthat::expect_no_error(
    divs_100 <- beethoven::divisor(100)
  )
  testthat::expect_true(is.integer(divs_100))
  testthat::expect_length(divs_100, 9)

  # expect error with character
  testthat::expect_error(
    beethoven::divisor("abc")
  )
})

################################################################################
##### process_counties
testthat::test_that("process_counties", {
  # expect no error
  testthat::expect_no_error(
    ct <- beethoven::process_counties()
  )
  testthat::expect_true(methods::is(ct, "sf"))
})
