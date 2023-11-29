################################################################################
# Date created: 2023-10-10
# Packages required: None
################################################################################

################################################################################
#' download_sedac_population_data: download UN WPP-Adjusted population density
#' data from NASA Socioeconomic Data and Applications Center (SEDAC).
#' @description
#' The `download_sedac_population_data()` function accesses and downloads
#' population density data from the National Aeronatuics and Space
#' Administration's (NASA) [UN WPP-Adjusted Population Density, v4.11]
#' (https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-adjuste
#' d-to-2015-unwpp-country-totals-rev11).
#' @param year character(1). Available years are 2000, 2005, 2010, 2015, and
#' 2020, or "all" for all years.
#' @param data_format character(1). Individual year data can be downloaded as
#' "ASCII" or "GeoTIFF". "all" years is downloaded as "netCDF".
#' @param data_resolution character(1). Available resolutions are 30 second
#' (~1km), 2.5 minute (~5km), 15 minute (~30km), 30 minute (~55km), and 60
#' minute (~110km).
#' @param directory_to_download character(1). Directory to download zip files
#' from NASA UN WPP-Adjusted Population Density, v4.11.
#' @param directory_to_save character(1). Directory to decompress zip files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the user
#' acknowledge that the data downloaded using this function may be very large
#' and use lots of machine storage and memory.
#' @param unzip logical(1). Unzip zip files. Default = `TRUE`.
#' @param remove_zip logical(1). Remove zip files from directory_to_download.
#' Default = `FALSE`.
#' @param download logical(1).
#' @author Mitchell Manware
#' @return NULL;
#' @export
download_sedac_population_data <- function(
  year = "2020",
  data_format = "GeoTIFF",
  data_resolution = "60 minute",
  directory_to_download = "../../data/covariates/sedac_population/",
  directory_to_save = "../../data/covariates/sedac_population/",
  data_download_acknowledgement = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  download = FALSE
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
  #### 2. check for data acknowledgement
  if (data_download_acknowledgement == FALSE) {
    stop(paste0("Data download acknowledgement is set to FALSE.",
                "Please acknowledge that the data downloaded using this",
                "function may be very large and use lots of machine storage",
                "and memory."))
  }
  #### 3. check for data format
  if (is.null(data_format)) {
    stop(paste0("Please select a data format.\n"))
  }
  #### 4. check for data resolution
  if (is.null(data_resolution)) {
    stop(paste0("Please select a data resolution.\n"))
  }
  #### 5. define URL base
  base <- paste0("https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/")
  #### 6. define year
  if (year == "all") {
    year <- "totpop"
  } else {
    year <- as.character(year)
  }
  #### 7. define data resolution
  if (data_resolution == "60 minute") {
    resolution <- "1_deg"
  } else if (data_resolution == "30 second") {
    resolution <- "30_sec"
  } else if (data_resolution == "2.5 minute") {
    resolution <- "2pt5_min"
  } else if (data_resolution == "15 minute") {
    resolution <- "15_min"
  } else if (data_resolution == "30 minute") {
    resolution <- "30_min"
  }
  #### 8. define data format
  if (data_format == "GeoTIFF" && year != "totpop") {
    format <- "tif"
  } else if (data_format == "GeoTIFF" && year == "totpop") {
    format <- "nc"
    cat(paste0("Data for all years is only available in netCDF format. ",
               "Data will be downloaded as netCDF.\n"))
  } else if (data_format == "ASCII" && year != "totpop") {
    format <- "asc"
  } else if (data_format == "ASCII" && year == "totpop") {
    cat(paste0("Data for all years is only available in netCDF format. ",
               "Data will be downloaded as netCDF.\n"))
  } else if (data_format == "netCDF") {
    format <- "nc"
  }
  #### 9. build download URL
  download_url <- paste0(base,
                         "gpw-v4-population-density-adjusted-to-2015-unwpp-",
                         "country-totals-rev11/",
                         "gpw-v4-population-density-adjusted-to-2015-unwpp-",
                         "country-totals-rev11_",
                         year,
                         "_",
                         resolution,
                         "_",
                         format,
                         ".zip")
  #### 10. build download file name
  download_name <- paste0(directory_to_download,
                          "gpw_v4_population_density_adjusted_to_2015_unwpp_",
                          "country_totals_rev11_",
                          year,
                          "_",
                          resolution,
                          "_",
                          format,
                          ".zip")
  #### 11. build system command
  download_command <- paste0("curl -n -c ~/.urs_cookies -b ~/.urs_cookies -LJ",
                             " -o ",
                             download_name,
                             " --url ",
                             download_url,
                             "\n")
  #### 12. download data
  if (download == TRUE) {
    cat(paste0("Downloading requested file...\n"))
    system(command = download_command)
    cat(paste0("Requested file downloaded.\n"))
  } else if (download == FALSE) {
    return(cat(paste0("Skipping data download.\n")))
  }
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
  #### 14. remove zip file from download directory
  if (remove_zip == TRUE) {
    cat(paste0("Deleting downloaded zip files...\n"))
    file.remove(download_name)
    cat(paste0("Downloaded zip files deleted.\n"))
  }
}
