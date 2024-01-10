################################################################################
# Date modified: 2024-01-08
# Packages required: terra
################################################################################

################################################################################
#' process_hms: download daily wildfire smoke plume data from 
#' NOAA Hazard Mapping System Fire and Smoke Product
#'
#' @param date_start character(1). length of 10. Start date of downloaded data. 
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date of downloaded data. 
#' Format YYYY-MM-DD (ex. September 10, 2023 = "2023-09-10").
#' @param data_format character(1). "Shapefile" or "KML". Format of downloaded 
#' data.
#' @param density_level character(1). "Light", "Medium", "Heavy" or "all" 
#' wildfire smoke plumes. (Default = "all").
#' @param directory_with_downloaded_data character(1). Directory storing
#' downloaded ".shp" or ".kml" files.
#' @author Mitchell Manware.
#' @return a data.frame object;
#' @importFrom terra vect
#' @importFrom terra aggregate
#' @export
process_hms <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-02",
    data_format = "Shapefile",
    density_level = "all",
    directory_with_data = "./input/noaa_hms/raw/") {
  #### 0.
  if (data_format == "KML") {
    paste0("KML processes under construction")
    return(NULL)
  }
  #### 1. directory setup
  directory_with_data <- download_sanitize_path(directory_with_data)
  #### 2. check for variable
  check_for_null_parameters(mget(ls()))
  #### 3. define date sequence in character format
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 4. define list of data file names
  if (data_format == "Shapefile") {
    data_files <- list.files(
      path = directory_with_data,
      pattern = "hms_smoke",
      full.names = TRUE
    )
    data_files <- data_files[grep(".shp", data_files)]
    data_files <- data_files[grepl(paste0(
      date_sequence,
      collapse = "|"
    ),
    data_files)]
  } else if (data_format == "KML") {
    data_files <- list.files(
      path = directory_with_data,
      pattern = "hms_smoke_KML",
      full.names = TRUE
    )
  }
  #### 6. check for matching dates
  data_file_dates <- stringr::str_split_i(
    stringr::str_split_i(
      data_files,
      "hms_smoke",
      2
    ),
    ".shp",
    1
  )
  if (!(identical(data_file_dates, date_sequence))) {
    cat(paste0("Data for requested dates does not match available files.\n"))
    return(NULL)
  }
  #### 6. process data
  data <- terra::vect()
  if (data_format == "Shapefile") {
    for (d in seq_along(data_files)) {
      data_date <- terra::vect(data_files[d])
      data_date_aggregate <- terra::aggregate(
        data_date,
        by = "Density",
        dissolve = TRUE
      )
      data_date_aggregate$Density <- factor(
        data_date_aggregate$Density,
        levels = c("Light", "Medium", "Heavy")
      )
      data_date_aggregate$Date <- paste0(date_sequence[d])
      data <- rbind(data, data_date_aggregate)
    }
  }
  #### 7. subset data
  if (density_level %in% c("Light", "Medium", "Heavy")) {
    data_return <- data[
      data$Density == density_level,
    ]
  } else if (density_level == "all"){
    data_return <- data
  }
  #### 8. return SpatVector
  return(data_return)
}
