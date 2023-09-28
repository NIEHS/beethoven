#' @author Kyle P Messier
#' @description
#' unit testing for point data geographic covariates
#' 
#' 
#'
test_that("xgboost model is valid", {
  skip_on_ci()
  
  aqs.sftime <- sf::st_read("../testdata/aqs-test-data.gpkg") |>
    sftime::st_as_sftime()
  
  # fit the xgboost model
  #mdl <- xgboost::xgboost(aqs.sftime,covariate)
  
  # update with the needed tests
  a=1
  b=1
  expect_equal(a,b)
})
