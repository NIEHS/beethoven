#' @author Kyle P Messier
#' @description
#' unit testing for point data geographic covariates
#'
#'
#'
testthat::test_that("xgboost model is valid", {
  testthat::skip_on_ci()

  aqs_sftime <- sf::st_read("../testdata/aqs-test-data.gpkg") |>
    sftime::st_as_sftime()

  # fit the xgboost model

  # update with the needed tests
  a <- 1
  b <- 1
  testthat::expect_equal(a, b)
})
