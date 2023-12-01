################################################################################
# Date created: 2023-10-24
# Packages required: none
################################################################################

################################################################################
#' download_gmted_data: download global elevation data from the Global Multi-
#' resolution Terrain Elevation Data (GMTED2010).
#' @description
#' The `download_gmted_data()` function acesses and downloads Global
#' Multi-resolution Terrain Elevation Data (GMTED2010) from
#' [U.S. Geological Survey and National Geospatial-Intelligence Agency]
#' (https://www.usgs.gov/coastal-changes-and-impacts/gmted2010).
#' @param statistic character(1). Available statistics include "Breakline
#' Emphasis", "Systematic Subsample", "Median Statistic", "Minimum Statistic",
#' "Mean Statistic", "Maximum Statistic", and "Standard Deviation Statistic".
#' @param resolution character(1). Available resolutions include "7.5 arc-
#' seconds", "15 arc-seconds", and "30 arc-seconds".
#' @param directory_to_download character(1). Directory to download zip files
#' from Global Multi-resolution Terrain Elevation Data (GMTED2010).
#' @param directory_to_save character(1). Directory to decompress zip files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param unzip logical(1). Unzip zip files. Default = `TRUE`.
#' @param remove_zip logical(1). Remove zip file from directory_to_download.
#' Default = `FALSE`.
#' @param download logical(1). `= FALSE` will generate a `.txt` file containing
#' all download commands. By setting `= TRUE` the function will download all of
#' the requested data files.
#' @author Mitchell Manware
#' @return NULL;
#' @export
download_gmted_data <- function(
  statistic = NULL,
  resolution = NULL,
  directory_to_download = "../../data/covariates/gmted/",
  directory_to_save = "../../data/covariates/gmted/",
  data_download_acknowledgement = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  download = FALSE
) {
  # nolint start: cyclocomp_linter
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
  #### 3. check for statistic
  if (is.null(statistic) == TRUE) {
    stop(paste0("Please select a GMTED2010 statistic.\n"))
  }
  #### 4. check for valid statistic
  valid_statistics <- c("Breakline Emphasis", "Systematic Subsample",
                        "Median Statistic", "Minimum Statistic",
                        "Mean Statistic", "Maximum Statistic",
                        "Standard Deviation Statistic")
  if (!(statistic %in% valid_statistics)) {
    stop(paste0("Requested statistic is not recognized.\n"))
  }
  #### 5. check for resolution
  if (is.null(resolution) == TRUE) {
    stop(paste0("Please select a data resolution.\n"))
  }
  #### 6. check for valid resolution
  valid_resolutions <- c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds")
  if (!(resolution %in% valid_resolutions)) {
    stop(paste0("Requested resolution is not recognized.\n"))
  }
  #### 7. define URL base
  base <- paste0("https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo",
                 "/downloads/GMTED/Grid_ZipFiles/")
  #### 8. define URL statistic code
  statistics <- c("Breakline Emphasis", "Systematic Subsample",
                  "Median Statistic", "Minimum Statistic",
                  "Mean Statistic", "Maximum Statistic",
                  "Standard Deviation Statistic")
  statistic_codes <- c("be", "ds", "md", "mi", "mn", "mx", "sd")
  statistic_codes <- cbind(statistics, statistic_codes)
  statistic_code <- subset(statistic_codes, statistics == statistic)[2]
  #### 9. define URL resolution code
  resolutions <- c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds")
  resolution_codes <- c("75", "15", "30")
  resolution_codes <- cbind(resolutions, resolution_codes)
  resolution_code <- subset(resolution_codes, resolutions == resolution)[2]
  #### 10. build url
  download_url <- paste0(base,
                         statistic_code,
                         resolution_code,
                         "_grd.zip")
  #### 11. build download file name
  download_name <- paste0(directory_to_download,
                          "gmted2010_",
                          statistic_code,
                          resolution_code,
                          "_grd.zip")
  #### 12. build download command
  download_command <- paste0("curl -s -o ",
                             download_name,
                             " --url ",
                             download_url,
                             "\n")
  #### 13. initiate "..._curl_commands.txt"
  commands_txt <- paste0(directory_to_download,
                         "gmted_",
                         gsub(" ", "", statistic),
                         "_",
                         gsub(" ", "", resolution),
                         "_curl_command.txt")
  sink(commands_txt)
  #### 14. concatenate and print download command to "..._curl_commands.txt"
  cat(download_command)
  #### 15. finish "..._curl_commands.txt" file
  sink()
  #### 16. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 17 download data
  if (download == TRUE) {
    cat(paste0("Downloading requested file...\n"))
    system(command = system_command)
    Sys.sleep(5L)
    cat(paste0("Requested file downloaded.\n"))
  } else if (download == FALSE) {
    return(cat(paste0("Skipping data download.\n")))
  }
  #### 18. end if unzip == FALSE
  if (unzip == FALSE) {
    return(cat(paste0("Downloaded files will not be unzipped.\n")))
  }
  #### 19 unzip downloaded data
  cat(paste0("Unzipping files...\n"))
  unzip(download_name,
        exdir = directory_to_save)
  cat(paste0("Files unzipped and saved in ",
             directory_to_save,
             ".\n"))
  #### 20. remove zip files
  if (remove_zip == TRUE) {
    cat(paste0("Removing download files...\n"))
    file.remove(download_name)
    cat(paste0("Download files removed.\n"))
  }
  # nolint end
}
