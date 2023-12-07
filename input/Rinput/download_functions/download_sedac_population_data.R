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
#' @param download logical(1). `= FALSE` will generate a `.txt` file containing
#' all download commands. By setting `= TRUE` the function will download all of
#' the requested data files.
#' @param unzip logical(1). Unzip zip files. Default = `TRUE`.
#' @param remove_zip logical(1). Remove zip files from directory_to_download.
#' Default = `FALSE`.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_sedac_population_data <- function(
  year = "2020",
  data_format = "GeoTIFF",
  data_resolution = "60 minute",
  directory_to_download = "../../data/covariates/sedac_population/",
  directory_to_save = "../../data/covariates/sedac_population/",
  data_download_acknowledgement = FALSE,
  download = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  remove_command = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)

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
  #### 12. initiate "..._curl_command.txt"
  commands_txt <- paste0(directory_to_download,
                         "sedac_population_",
                         year,
                         "_",
                         resolution,
                         "_",
                         Sys.Date(),
                         "_curl_commands.txt")
  download_sink(commands_txt)
  #### 13. concatenate and print download command to "..._curl_commands.txt"
  cat(download_command)
  #### 14. finish "..._curl_commands.txt" file
  sink()
  #### 15. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 16. download data
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
  #### 17. end if unzip == FALSE
  download_unzip(file_name = download_name,
                 directory_to_unzip = directory_to_save,
                 unzip = unzip)
  #### 18. Remove command file
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)
  #### 19. remove zip files
  download_remove_zips(remove = remove_zip,
                       download_name = download_name)
}
