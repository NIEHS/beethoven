################################################################################
# Date created: 2023-10-17
# Packages required: none
################################################################################

################################################################################
#' download_sedac_groads_data: download Global Roads Open Access Data
#' Set (gROADS), v1 (1980-2010) data from NASA Socioeconomic Data and
#' Applications Center (SEDAC).
#' @description
#' The `download_sedac_groads_data()` function accesses and downloads
#' roads data from the National Aeronautics and Space
#' Administration's (NASA) [Global Roads Open Access Data Set]
#' (https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-
#' v1/data-download).
#' @param data_format character(1). Data can be downloaded as "Shapefile" or
#' "Geodatabase". (Only "Geodatabase" available for "Global" region).
#' @param data_region character(1). Data can be downloaded for "Global",
#' "Africa", "Asia", "Europe", "Americas", "Oceania East", and "Oceania West".
#' @param directory_to_download character(1). Directory to download zip files
#' from NASA UN WPP-Adjusted Population Density, v4.11.
#' @param directory_to_save character(1). Directory to decompress zip files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the user
#' acknowledge that the data downloaded using this function may be very large
#' and use lots of machine storage and memory.
#' @param unzip logical(1). Unzip zip files. Default = `TRUE`.
#' @param remove_zip logical(1). Remove zip files from directory_to_download.
#' Default = `FALSE`.
#' @author Mitchell Manware
#' @return NULL;
#' @export
download_sedac_groads_data <- function(
  data_format = "Shapefile",
  data_region = "Americas",
  directory_to_download = "./input/sedac_groads/raw/",
  directory_to_save = "./input/sedac_groads/raw/",
  data_download_acknowledgement = FALSE,
  unzip = TRUE,
  remove_zip = FALSE
) {
  #### 1. directory setup
  chars_dir_download <- nchar(directory_to_download)
  chars_dir_save <- nchar(directory_to_save)
  if (substr(directory_to_download,
             chars_dir_download,
             chars_dir_download) != "/") {
    directory_to_download <- paste(directory_to_download,
                                   "/",
                                   sep = "")
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
    stop(paste0("Data download acknowledgement is set to FALSE.",
                "Please acknowledge that the data downloaded using this",
                "function may be very large and use lots of machine storage",
                "and memory."))
  }
  #### 3. check for region
  if (is.null(data_region) == TRUE) {
    stop(paste0("Please select a data region.\n"))
  }
  #### 4. check if region is valid
  regions <- c("Global", "Africa", "Asia", "Europe",
               "Americas", "Oceania East", "Oceania West")
  if (!(data_region %in% regions)) {
    stop(paste0("Requested region not recognized.\n"))
  }
  #### 5. check for data format
  formats <- c("Shapefile", "Geodatabase")
  if (!(data_format %in% formats)) {
    stop(paste0("Requested data format not recognized.\n"))
  }
  #### 6. define URL base
  base <- paste0("https://sedac.ciesin.columbia.edu/downloads/data/groads/",
                 "groads-global-roads-open-access-v1/",
                 "groads-v1-")
  #### 7. define data format
  if (data_format == "Shapefile") {
    format <- "shp"
  } else if (data_format == "Geodatabase") {
    format <- "gbd"
  }
  #### 8. coerce region to lower case
  region <- tolower(data_region)
  #### 9. build download URL
  download_url <- paste0(base,
                         region,
                         "-",
                         format,
                         ".zip")
  #### 10. build download file name
  download_name <- paste0(directory_to_download,
                          "groads_v1_",
                          region,
                          "_",
                          format,
                          ".zip")
  #### 11. build system command
  system_command <- paste0("curl -n -c ~/.urs_cookies -b ~/.urs_cookies -LJ",
                           " -o ",
                           download_name,
                           " --url ",
                           download_url,
                           "\n")
  #### 12. download data
  cat(paste0("Downloading requested file...\n"))
  system(command = system_command)
  cat(paste0("Requested file downloaded.\n"))
  #### 13. end if unzip == FALSE
  if (unzip == FALSE) {
    return(cat(paste0("Downloaded files will not be unzipped.\n")))
  }
  #### 14. unzip downloaded data
  cat(paste0("Unzipping files...\n"))
  unzip(download_name, exdir = directory_to_save)
  cat(paste0("Files unzipped and saved in ",
             directory_to_save,
             ".\n"))
  #### 14. remove zip file
  if (remove_zip == TRUE) {
    cat(paste0("Removing downloaded zip file...\n"))
    file.remove(download_name)
    cat(paste0("Downloaded zip files deleted.\n"))
  }
}
