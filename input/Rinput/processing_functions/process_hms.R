################################################################################
# Date modified: 2024-01-08
# Packages required: terra
################################################################################

################################################################################
#' process_hms: import and aggregate wildfire smoke plume data based on smoke
#' density level.
#'
#' @param date_start character(1). length of 10. Start date of downloaded data. 
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date of downloaded data. 
#' Format YYYY-MM-DD (ex. September 10, 2023 = "2023-09-10").
#' @param data_format character(1). "Shapefile" or "KML". Format of downloaded 
#' data.
#' @param density_level character(1). "Light", "Medium", or "Heavy".
#' @param directory_with_data character(1). Directory storing
#' downloaded ".shp" or ".kml" files.
#' @author Mitchell Manware.
#' @return a SpatVector object;
#' @importFrom terra vect
#' @importFrom terra aggregate
#' @importFrom terra subset
#' @export
process_hms <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-02",
    data_format = "Shapefile",
    density_level = c("Light", "Medium", "Heavy"),
    directory_with_data = "./input/noaa_hms/raw/") {
  #### 0.
  if (data_format == "KML") {
    message(paste0("KML processes under construction"))
    return(NULL)
  }
  #### 1. directory setup
  directory_with_data <- download_sanitize_path(directory_with_data)
  #### 2. check for variable
  check_for_null_parameters(mget(ls()))
  #### 3. define date sequence
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
  #### 5. check for matching dates
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
    message(paste0(
      "Data for requested dates does not match available files.\n"
      ))
    return(NULL)
  }
  #### 6. process data
  data <- terra::vect()
  if (data_format == "Shapefile") {
    for (d in seq_along(data_files)) {
      cat(paste0(
        "Processing file ",
        data_files[d],
        "...\n"
      ))
      data_date <- terra::vect(data_files[d])
      data_date_p <- terra::project(data_date, "EPSG:4326")
      #### 7. absent polygons (ie. December 31, 2018)
      if (nrow(data_date) == 0) {
        cat(paste0(
          "Smoke plume polygons absent for date: ",
          date_sequence[d],
          ". Returning empty SpatVector.\n"))
        data_missing <- data_date_p
        data_missing$Density <- ""
        data_missing$Date <- ""
        data_return <- rbind(data, data_missing)
      } else {
        #### 8. zero buffer to avoid self-intersecting geometry error
        data_date_b <- terra::buffer(
          data_date_p,
          width = 0
        )
        #### 9. aggregate density-specific polygons
        data_date_aggregate <- terra::aggregate(
          data_date_b,
          by = "Density",
          dissolve = TRUE
        )
        #### 10. factorize
        data_date_aggregate$Density <- factor(
          data_date_aggregate$Density,
          levels = c("Light", "Medium", "Heavy")
        )
        data_date_aggregate$Date <- paste0(date_sequence[d])
        data <- rbind(data, data_date_aggregate)
      }
    }
  }
  #### 11. select "Density" and "Date"
  data <- data[1:nrow(data), c("Density", "Date")]
  #### 12. subset to density level
  data_return <- data[data$Density == density_level,]
  #### 13. return SpatVector
  return(data_return)
}
