################################################################################
# Date created: September 29, 2023
# Packages required: stringr
################################################################################

################################################################################
#' download_narr_p_levels_data: download pressure level meteorological data from
#' NOAA NCEP North American Regional Reanalysis (NARR) model.
#' @description
#' The `download_narr_p_levels_data` function accesses and downloads
#' pressure level meteorological data from [NOAA NCEP North American Regional]
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
#' @importFrom stringr str_pad
#' @export
download_narr_p_levels_data <- function(
  year_start = 2022,
  year_end = 2022,
  variables = NULL,
  directory_to_save = "../../data/covariates/narr/",
  data_download_acknowledgement = FALSE,
  download = FALSE
) {
  #### 1. directory setup
  directory_to_save <- download_setup_dir(directory_to_save)
  #### 2. check for data download acknowledgement
  if (data_download_acknowledgement == FALSE) {
    stop(cat(paste0("Data download acknowledgement is set to FALSE. Please ",
                    "acknowledge that the data downloaded using this function ",
                    "may be very large and use lots of machine storage and ",
                    "memory.")))
  }
  #### 3. check for variables
  if (is.null(variables) == TRUE) {
    stop(paste0("Please select an NCEP-NARR variable.\n"))
  }
  #### 4. define years sequence
  years <- seq(year_start, year_end, 1)
  #### 5. define months sequence
  months <- stringr::str_pad(seq(1, 12, by = 1),
                             width = 2,
                             pad = "0",
                             side = "left")
  #### 6. define variables
  variables_list <- as.vector(variables)
  #### 7. define URL base
  base <- "https://downloads.psl.noaa.gov//Datasets/NARR/Dailies/pressure/"
  #### 8. initiate "..._curl_commands.txt"
  commands_txt <- paste0(directory_to_save,
                         "narr_p_levels_curl_commands.txt")
  sink(commands_txt)
  #### 9. concatenate download commands to "..._curl_commands.txt"
  for (v in seq_along(variables_list)) {
    variable <- variables_list[v]
    folder <- paste0(directory_to_save, variable, "/")
    if (!(file.exists(folder))) {
      dir.create(folder)
    }
    for (y in seq_along(years)) {
      year <- years[y]
      for (m in seq_along(months)) {
        month <- months[m]
        url <- paste0(base,
                      variable,
                      ".",
                      year,
                      month,
                      ".nc")
        destfile <- paste0(directory_to_save,
                           variable,
                           "/",
                           variable,
                           ".",
                           year,
                           month,
                           ".nc")
        command <- paste0("curl -s -o ",
                          destfile,
                          " --url ",
                          url,
                          "\n")
        cat(command)
      }
    }
  }
  #### 10. finish "..._curl_commands.txt"
  sink()
  #### 11. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 12. download data
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
}
