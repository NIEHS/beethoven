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
#' @param download logical(1). `= FALSE` will generate a `.txt` file containing
#' all download commands. By setting `= TRUE` the function will download all of
#' the requested data files.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_sedac_groads_data <- function(
  data_format = c("Shapefile", "Geodatabase"),
  data_region = c("Americas", "Global", "Africa", "Asia",
                  "Europe", "Oceania East", "Oceania West"),
  directory_to_download = "./input/data/sedac_groads/",
  directory_to_save = "./input/data/sedac_groads/",
  data_download_acknowledgement = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  download = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)

  #### 3. check for region
  if (is.null(data_region)) {
    stop(paste0("Please select a data region.\n"))
  }
  #### 4. check if region is valid
  regions <- c("Global", "Africa", "Asia", "Europe",
               "Americas", "Oceania East", "Oceania West")
  data_region <- match.arg(data_region)
  #### 5. check for data format
  # formats <- c("Shapefile", "Geodatabase")
  # if (!(data_format %in% formats)) {
  #   stop(paste0("Requested data format not recognized.\n"))
  # }
  formats <- match.arg(formats)
  #### 6. define URL base
  base <- paste0("https://sedac.ciesin.columbia.edu/downloads/data/groads/",
                 "groads-global-roads-open-access-v1/",
                 "groads-v1-")
  #### 7. define data format
  if (data_format == "Shapefile") {
    format <- "shp"
  } else if (data_format == "Geodatabase") {
    format <- "gdb"
  }
  #### 8. coerce region to lower case
  region <- tolower(data_region)
  #### 9. build download URL
  download_url <- paste0(base,
                         gsub(" ", "-", region),
                         "-",
                         format,
                         ".zip")
  #### 10. build download file name
  download_name <- paste0(directory_to_download,
                          "groads_v1_",
                          gsub(" ", "-", region),
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
  #### 12. initiate "..._curl_commands.txt"
  commands_txt <- paste0(directory_to_download,
                         "sedac_groads_",
                         gsub(" ", "_", region),
                         "_",
                         Sys.Date(),
                         "_curl_command.txt")
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
