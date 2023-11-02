#' @author Mitchell Manware
#' @description
#' unit test for model output is within desired temporal range
#' (2018 - 2022 inclusive).
#' assumes `test_nc_output.nc` is point data.
test_that("Output times are within temporal range", {
  withr::local_package("terra")
  withr::local_package("lubridate")
  # 1. import model output
  path_results <- "../testdata/test_nc_output.nc"
  model_results <- terra::vect(path_results)
  # 2. function
  check_temporal_range <- function(
    model_output = model_results,
    start_range = "2018-01-01",
    end_range = "2022-12-31"
  ) {
    # change character inputs to dates
    start_date <- lubridate::as_datetime(start_range)
    end_date <- lubridate::as_datetime(end_range)
    # create sequence of dates
    range <- seq(start_date, end_date, 86400)
    # assign date values to variable
    dates <- lubridate::as_datetime(model_results$Date)
    # check that dates are within range
    checked <- as.vector(dates %in% range)
    return(checked)
  }
  iswithin <- check_temporal_range()
  # expect all elements == TRUE
  expect_equal(any(iswithin), TRUE)
})
