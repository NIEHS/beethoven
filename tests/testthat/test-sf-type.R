#' @author Kyle P Messier
#' @description
#' unit testing that the dependent variable is read and converted
#'  to the expected sf/sftime class
#'
#'
testthat::test_that("dependent variable object is an sf class", {
  aqs_sftime <- sf::st_read("../testdata/aqs-test-data.gpkg") |>
    sftime::st_as_sftime()
  testthat::expect_s3_class(aqs_sftime, "sf")
})
