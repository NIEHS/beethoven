################################################################################
# Date created: 2023-09-19
# Packages required: None
################################################################################

################################################################################
#' download_noaa_hms_smoke_data: download daily wildfire smoke plume data from
#' NOAA Hazard Mapping System Fire and Smoke Product
#' @description
#' The `download_noaa_hms_smoke_data()` function accesses and downloads wildfire
#' smoke plume coverage data from the National Oceanic and Atmospheric
#' Administration's (NOAA) [Hazard Mapping System Fire and Smoke Product]
#' (https://www.ospo.noaa.gov/Products/land/hms.html#0).
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 10, 2023 = "2023-09-10").
#' @param data_format character(1). "Shapefile" or "KML".
#' @param directory_to_download character(1). Directory to download zip files
#' from NOAA Hazard Mapping System Fire and Smoke Product. (Ignored if
#' `data_format = "KML"`.)
#' @param directory_to_save character(1). Directory to save unzipped shapefiles
#' and KML files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the user
#' acknowledge that the data downloaded using this function may be very large
#' and use lots of machine storage and memory.
#' @param download
#' @param unzip logical(1). Unzip zip files. Default = `TRUE`. (Ignored if
#' `data_format = "KML"`.)
#' @param remove_zip logical(1). Remove zip files from
#' directory_to_download. Default = `FALSE`. (Ignored if `data_format = "KML"`.)
#' @param remove_command
#' @importFrom utils head
#' @importFrom utils tail
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_noaa_hms_smoke_data <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    data_format = "Shapefile",
    directory_to_download = "./input/data/noaa_hms/",
    directory_to_save = "./input/data/noaa_hms/",
    data_download_acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE,
    unzip = TRUE,
    remove_zip = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)

  #### 3. check for unzip == FALSE && remove_zip == TRUE
  if (unzip == FALSE && remove_zip == TRUE) {
    stop(paste0(
      "Arguments `unzip = FALSE` and `remove_zip = TRUE` are not ",
      "acceptable together. Please change one.\n"
    ))
  }
  #### 4. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 5. define URL base
  base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"
  #### 6. initiate "..._curl_commands.txt"
  commands_txt <- paste0(
    directory_to_download,
    "hms_smoke_",
    utils::head(date_sequence, n = 1),
    "_",
    utils::tail(date_sequence, n = 1),
    "_curl_commands.txt"
  )
  download_sink(commands_txt)
  #### 7. concatenate and print download commands to "..._curl_commands.txt"
  download_names <- NULL
  for (f in seq_along(date_sequence)) {
    year <- substr(date_sequence[f], 1, 4)
    month <- substr(date_sequence[f], 5, 6)
    if (data_format == "Shapefile") {
      suffix <- ".zip"
      directory_to_cat <- directory_to_download
    } else if (data_format == "KML") {
      suffix <- ".kml"
      directory_to_cat <- directory_to_save
    }
    url <- paste0(
      base,
      data_format,
      "/",
      year,
      "/",
      month,
      "/hms_smoke",
      date_sequence[f],
      suffix
    )
    destfile <- paste0(
      directory_to_cat,
      "hms_smoke_",
      data_format,
      "_",
      date_sequence[f],
      suffix
    )
    download_names <- c(download_names, destfile)
    command <- paste0(
      "curl -s -o ",
      destfile,
      " --url ",
      url,
      "\n"
    )
    cat(command)
  }
  #### 8. finish "..._curl_commands.txt"
  sink()
  #### 9. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 10. download data
  download_run(
    download = download,
    system_command = system_command,
    commands_txt = commands_txt
  )
  #### 11. remove command file
  download_remove_command(
    remove = remove_command,
    commands_txt = commands_txt
  )
  #### 12. end if data_format == "KML"
  if (data_format == "KML") {
    return(cat(paste0("KML files cannot be unzipped.\n")))
  }
  #### 13. unzip downloaded zip files
  for (d in seq_along(download_names)) {
    download_unzip(
      file_name = download_names[d],
      directory_to_unzip = directory_to_save,
      unzip = unzip
    )
  }
  #### 14. remove zip files
  download_remove_zips(
    remove = remove_zip,
    download_name = download_names
  )
}
