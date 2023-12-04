################################################################################
# Date created: September 29, 2023
# Packages required: None
################################################################################

################################################################################
#' download_narr_monolevel_data: download monolevel meteorological data from
#' NOAA NCEP North American Regional Reanalysis (NARR) model.
#' @description
#' The `download_narr_monolevel_data` function accesses and downloads
#' monolevel meteorological data from [NOAA NCEP North American Regional]
#' [Reanalysis (NARR)](https://psl.noaa.gov/data/gridded/data.narr.html).
#' @param year_start integer(1). length of 4. Start of year range for
#' downloading data.
#' @param year_end integer(1). length of 4. End of year range for downloading
#' data.
#' @param variables character(1). Variable(s) name acronym.
#' @param directory_to_save character(1). Directory(s) to save downloaded data
#' files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the user
#' acknowledge that the data downloaded using this function may be very large
#' and use lots of machine storage and memory.
#' @param download logical(1). `= FALSE` will generate a `.txt` file containing
#' all download commands. By setting `= TRUE` the function will download all of
#' the requested data files.
#' @author Mitchell Manware
#' @return NULL;
#' @export
download_narr_monolevel_data <- function(
  year_start = 2022,
  year_end = 2022,
  variables = NULL,
  directory_to_save = "../../data/covariates/narr/",
  data_download_acknowledgement = FALSE,
  download = FALSE
) {
  #### 1. directory setup
  directory_to_save <- directory_setup(directory_to_save)
  #### 2. check for data download acknowledgement
  if (data_download_acknowledgement == FALSE) {
    stop(paste0("Data download acknowledgement is set to FALSE. Please",
                "acknowledge that the data downloaded using this function may",
                "be very large and use lots of machine storage and memory."))
  }
  #### 3. check for variables
  if (is.null(variables) == TRUE) {
    stop(paste0("Please select an NCEP-NARR variable.\n"))
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
  #### 10. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 11. download data
  execute_download(download = download,
                   system_command = system_command,
                   commands_txt = commands_txt)
}

