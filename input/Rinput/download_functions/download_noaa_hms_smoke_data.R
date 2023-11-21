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
#' @param unzip logical(1). Unzip zip files. Default = `TRUE`. (Ignored if
#' `data_format = "KML"`.)
#' @param remove_zip logical(1). Remove zip files from
#' directory_to_download. Default = `FALSE`. (Ignored if `data_format = "KML"`.)
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_noaa_hms_smoke_data <- function(
  date_start = "2023-09-01",
  date_end = "2023-09-01",
  data_format = "Shapefile",
  directory_to_download = "./input/noaa_hms/raw/",
  directory_to_save = "./input/noaa_hms/raw/",
  data_download_acknowledgement = FALSE,
  unzip = TRUE,
  remove_zip = FALSE
) {
  # nocov start
  #### 1. directory setup
  chars_dir_download <- nchar(directory_to_download)
  chars_dir_save <- nchar(directory_to_save)
  if (substr(directory_to_download,
             chars_dir_download,
             chars_dir_download) != "/") {
    directory_to_download <- paste(directory_to_download, "/", sep = "")
  }
  if (substr(directory_to_save, chars_dir_save, chars_dir_save) != "/") {
    directory_to_save <- paste(directory_to_save, "/", sep = "")
  }
  if (dir.exists(directory_to_download) == FALSE) {
    dir.create(directory_to_download)
  }
  if (dir.exists(directory_to_save) == FALSE) {
    dir.create(directory_to_save)
  }
  #### 2. check for data download acknowledgement
  if (data_download_acknowledgement == FALSE) {
    stop(paste0(
      "Data download acknowledgement is set to FALSE. Please ",
      "acknowledge that the data downloaded using this function may ",
      "be very large and use lots of machine storage and memory.\n"
    ))
  }
  #### 3. check for unzip == FALSE && remove_zip == TRUE
  if (unzip == FALSE && remove_zip == TRUE) {
    stop(paste0("Arguments `unzip = FALSE` and `remove_zip = TRUE` are not ",
                "acceptable together. Please change one.\n"))
  }
  #### 4. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 5. define URL base
  base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"
  #### 6. define empty vectors
  file_urls <- NULL
  download_names <- NULL
  #### 7. add download URLs and file names to empty vectors
  for (f in seq_along(date_sequence)) {
    year <- substr(date_sequence[f], 1, 4)
    month <- substr(date_sequence[f], 5, 6)
    if (data_format == "Shapefile") {
      file_urls_add <- paste0(
        base,
        data_format,
        "/",
        year,
        "/",
        month,
        "/hms_smoke",
        date_sequence[f],
        ".zip"
      )
      file_urls <- c(file_urls, file_urls_add)
      download_names_add <- paste0(
        directory_to_download,
        "hms_smoke_",
        data_format,
        "_",
        date_sequence[f],
        ".zip"
      )
      download_names <- c(download_names, download_names_add)
    } else if (data_format == "KML") {
      file_urls_add <- paste0(
        base,
        data_format,
        "/",
        year,
        "/",
        month,
        "/hms_smoke",
        date_sequence[f],
        ".kml"
      )
      file_urls <- c(file_urls, file_urls_add)
      download_names_add <- paste0(
        directory_to_save,
        "hms_smoke_",
        data_format,
        "_",
        date_sequence[f],
        ".kml"
      )
      download_names <- c(download_names, download_names_add)
    }
  }
  #### 8. download data
  if (!any(file.exists(download_names))) {
    cat(paste0("Downloading requested files...\n"))
    download.file(file_urls, download_names, method = "libcurl", quiet = TRUE)
    cat(paste0("Requested files downloaded.\n"))
  }
  #### 9. end if unzip == FALSE
  if (unzip == FALSE && data_format == "Shapefile") {
    return(cat(paste0("Downloaded files will not be unzipped.\n")))
  }
  #### 10. unzip files
  if (unzip == TRUE && data_format == "Shapefile") {
    cat(paste0("Unzipping zip files to ", directory_to_save, "...\n"))
    for (u in seq_along(download_names)) {
      unzip(download_names[u], exdir = directory_to_save)
    }
    cat(paste0("Zip files unzipped.\n"))
  }
  #### 11. remove zip files
  if (remove_zip == TRUE && data_format == "Shapefile") {
    cat(paste0("Removing zip files...\n"))
    for (z in seq_along(download_names)) {
      file.remove(download_names[z])
    }
    cat(paste0("Zip files removed\n"))
  }
  # nocov end
}
