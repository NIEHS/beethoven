#' @author Mitchell Manware
#' @description unit test for checking covariate, base learner and meta learner 
#' temporal range
#' 
testthat::test_that("covariates are within temporal range.", {
  withr::local_package("terra")
  # use NCEP-NCAR Reanalysis 1 data as example
  paths <- list.files("../../input/Rinput/data/NCEP-NCAR-Reanalysis-1/",
                      full.names = TRUE)
  iswithin <- NULL
  for(p in seq_along(path)) {
    iswithin <- check_temporal_range(data = path[p],
                                     start_range = "2018-01-01",
                                     end_range = "2022-12-31",
                                     data_type = "Raster")
    iswithin <- rbind(iswithin, iswithin)
  }
  # expect all elements equal true
  expect_equal(any(iswithin), TRUE)
})

testthat::test_that("test_nc_output.nc is within temporal range.", {
  withr::local_package("terra")
  # assumes that test_nc_output.nc is point data
  path <- "../testdata/test_nc_output.nc"
  iswithin <- check_temporal_range(data = path,
                                   start_range = "2018-01-01",
                                   end_range = "2022-12-31",
                                   data_type = "Point")
  # expect all elements equal true
  expect_equal(any(iswithin), TRUE)
})