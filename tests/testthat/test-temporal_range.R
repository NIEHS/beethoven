#' @author Mitchell Manware
#' @description unit test for checking covariate, base learner and meta learner
#' temporal range
#'
testthat::test_that("covariates are within temporal range.", {
  withr::local_package("terra")
  # use NCEP-NCAR Reanalysis 1 data as example
  paths <- list.files("../../input/data/NCEP-NCAR-Reanalysis-1/",
                      full.names = TRUE)
  iswithin <- NULL
  for (p in seq_along(paths)) {
    iswithin <- check_temporal_range(data = paths[p],
                                     data_type = "Raster")
    iswithin <- rbind(iswithin, iswithin)
  }
  # expect all elements equal true
  expect_equal(all(iswithin), TRUE)
})

testthat::test_that("test_nc_output.nc is within temporal range.", {
  withr::local_package("terra")
  # assumes that test_nc_output.nc is point data
  path <- "../testdata/test_nc_output.nc"
  iswithin <- check_temporal_range(data = path,
                                   data_type = "Point")
  # expect all elements equal true
  expect_equal(all(iswithin), TRUE)
})
