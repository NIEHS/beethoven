################################################################################
# Date created: September 29, 2023
# Packages required: None
################################################################################

################################################################################
#' download_narr_monolevel_data: download daily monolevel meteorological
#' data from NOAA NCEP North American Regional Reanalysis
#' @description
#' The `download_narr_monolevel_data` function accesses and downloads
#' monolevel meteorological data.
#' @param year_start integer(1). length of 4. Start of year range for
#' downloading data.
#' @param year_end integer(1). length of 4. End of year range for downloading
#' data.
#' @param variables character(1). Variable code(s) that should be downloaded.
#' For full list of variables and variable codes see ***.
#' @param directory_to_save character(1). Directory(s) to save downloaded data
#' files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the user
#' acknowledge that the data downloaded using this function may be very large
#' and use lots of machine storage and memory.
#' @author Mitchell Manware
#' @return NULL; NCEP North American Regional Reanalysis monolevel
#' meteorological data will be returned to the designated saving directory.
#' @export
download_narr_monolevel_data <- function(
  year_start = 2022,
  year_end = 2022,
  variables = NULL,
  directory_to_save = "./input/ncep_narr_monolevel/raw/",
  data_download_acknowledgement = FALSE
) {
  #### 1. directory setup
  chars_dir_save <- nchar(directory_to_save)
  if (substr(directory_to_save, chars_dir_save, chars_dir_save) != "/") {
    directory_to_save <- paste(directory_to_save,
                               "/",
                               sep = "")
  }
  #### 2. check for data download acknowledgement
  if (data_download_acknowledgement == FALSE) {
    cat(paste0("Data download acknowledgement is set to FALSE. Please",
               "acknowledge that the data downloaded using this function may",
               "be very large and use lots of machine storage and memory."))
    stop()
  }
  #### 3. check for variables
  if (is.null(variables) == TRUE) {
    cat(paste0("Please select an NCEP-NARR variable.\n"))
    stop()
  }
  #### 4. define years sequence
  years <- seq(year_start, year_end, 1)
  #### 5. define variables
  variables_list <- as.vector(variables)
  #### 6. define URL base
  base <- "https://downloads.psl.noaa.gov//Datasets/NARR/Dailies/monolevel/"
  #### 7. initiate "..._curl_commands.txt"
  commands_txt <- paste0(directory_to_save,
                         "narr_monolevel_curl_commands.txt")
  sink(commands_txt)
  #### 8. concatenate and print download commands to "..._curl_commands.txt"
  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")
    if (!(file.exists(folder))) {
      dir.create(folder)
    }
    for (y in seq_along(years)) {
      year <- years[y]
      url <- paste0(base,
                    variable,
                    ".",
                    year,
                    ".nc")
      destfile <- paste0(directory_to_save,
                         variable,
                         "/",
                         variable,
                         ".",
                         year,
                         ".nc")
      command <- paste0("curl -s -o ",
                        destfile,
                        " --url ",
                        url,
                        "\n")
      cat(command)
    }
  }
  #### 9. finish "..._curl_commands.txt"
  sink()
  cat(paste0("Downloading requested files...\n"))
  #### 10. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 11. download data
  system(command = system_command)
  cat(paste0("Requested files have been downloaded.\n"))
  #### 12. remove "..._curl_commands.txt"
  Sys.sleep(5L)
  file.remove(commands_txt)
}
