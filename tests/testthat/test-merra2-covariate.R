#' @author Kyle P Messier
#' @description
#' unit testing for MERRA2 covariates
#' test that the covariate calculation does not have unexpected NA
#' 
#' 
#'
test_that("MERRA2 covariate is not NA", {
  aqs.sftime <- sf::st_read("../testdata/aqs-test-data.gpkg") |>
    st_as_sftime()
  
  MERRA2.var <- calc_MERRA2_covariate(aqs.sftime)
  
  expect_equal(sum(is.na(MERRA2.var))==0, TRUE)
})
