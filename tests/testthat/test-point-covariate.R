#' @author Kyle P Messier
#' @description
#' unit testing for point data geographic covariates
#' 
#' 
#'
test_that("point covariate is not NA", {

  aqs.sftime <- sf::st_read("../testdata/aqs-test-data.gpkg") |>
    sftime::st_as_sftime()
  
  # Calculate the sum of exponentially decaying contributions
  point.var <- calc_SEDC_covariate(aqs.sftime)
  
  expect_equal(sum(is.na(point.var))==0, TRUE)
})
