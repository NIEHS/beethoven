#' @author Mitchell Manware
#' @description
#' unit tests for covariates and model data within desired temporal range
#' (2018 - 2022 inclusive).

test_that("test_nc_output.nc is within temporal range.", {
  # run check_temporal_range() function
  iswithin <- check_temporal_range(data = "../testdata/test_nc_output.nc",
                                   start_range = "2018-01-01",
                                   end_range = "2022-12-31",
                                   data_type = "Point")
  # expect all elements == TRUE
  expect_equal(any(iswithin), TRUE)
})
