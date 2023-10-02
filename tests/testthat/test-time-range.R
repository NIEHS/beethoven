#' @author Mitchell Manware
#' @description
#' unit test for model output is within desired temporal range (2018 - 2022 inclusive)
#' 
#' 
test_that("Output times are within temporal range", {
  
  library(terra)
  library(lubridate)

  # 1. import model output
  path_results = "./output/model_output.nc"
  model_results = terra::rast(path_results)
  
  # 2. function
  check_temporal_range = function(
    model_output = model_results,
    start_range = "2018-01-01",
    end_range = "2022-12-31"
  ){
    
    # check that data has time values
    if(model_output@ptr$hasTime == FALSE){
      stop("Input should have time values.\n")
    }
    
    # change character inputs to dates
    start_date = as_datetime(start_range)
    end_date = as_datetime(end_range)
    
    # create sequence of dates
    range = seq(start_date, end_date, 86400)
    
    # assign date values to variable
    dates = as_datetime(model_output@ptr$time)
    
    # check dates and ranges are same class
    if(any(class(range) == class(dates)) == FALSE){
      stop("Data dates and test range have different classes.\n")
    }
    
    # check that dates are within range
    checked = as.vector(dates %in% range)
    return(checked)

  }
  
  iswithin = check_temporal_range()
  
  # expect all elements == TRUE
  expect_equal(any(iswithin), TRUE)
  
})
