# Function to test temporal range of various data sets throughout pipeline
# Date created: 2023-11-03
# Mitchell Manware

#' Check if input data is within user defined temporal range
#' 
#' @param data file path to data
#' @param start_range beginning of temporal range
#' @param end_range end of temporal range
#' @param data_type Point, Polygon, or Raster data
#' @return A logical vector reporting whether time observations in data are
#' within defined temporal range
#' @author Mitchell Manware
#' @export
check_temporal_range <- function(
  data = NULL,
  start_range = "2018-01-01",
  end_range = "2022-12-31",
  data_type = NULL
) {
  #### 1. create sequence of dates
  start_date <- lubridate::as_datetime(start_range)
  end_date <- lubridate::as_datetime(end_range)
  range <- seq(start_date, end_date, 86400)
  #### 2. import data and define `dates` vector
  if (data_type %in% c("Point", "Polygon")) {
    data <- terra::vect(data)
    dates <- lubridate::as_datetime(data$Date)
  } else if (data_type == "Raster") {
    data <- terra::rast(data)
    dates <- lubridate::as_datetime(data@ptr$time)
  }
  #### 3. check `dates` are within `range`
  checked <- data.table::data.table(dates %in% range)
  #### 4. return logical vector
  return(checked)
}
