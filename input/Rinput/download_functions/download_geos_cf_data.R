################################################################################
# Date created: 2023-10-16
# Packages required: stringr
################################################################################

################################################################################
#' download_geos_cf_data: download atmospheric composition data from the NASA
#' Global Earth Observing System (GEOS) model.
#' @description
#' The `download_goes_cf_data()` function accesses and downloads various
#' atmospheric composition collections from the [NASA Global Earth Observing]
#' [System (GEOS) model](https://gmao.gsfc.nasa.gov/GEOS_systems/).
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param collection character(1). GEOS-CF data collection file name.
#' @param directory_to_save character(1). Directory to save data.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @author Mitchell Manware
#' @return NULL;
#' @importFrom stringr str_sub
#' @importFrom stringr str_pad
#' @export
download_geos_cf_data <- function(
  date_start = "2023-09-01",
  date_end = "2023-09-01",
  collection = NULL,
  directory_to_save = "./input/geos_cf/raw/",
  data_download_acknowledgement = FALSE
) {
  #### 1. directory setup
  chars_dir_save <- nchar(directory_to_save)
  if (substr(directory_to_save, chars_dir_save, chars_dir_save) != "/") {
    directory_to_save <- paste0(directory_to_save, "/", sep = "")
  }
  #### 2. check for data download acknowledgement
  if (data_download_acknowledgement == FALSE) {
    stop(paste0("Data download acknowledgement is set to FALSE. ",
                "Please acknowledge that the data downloaded using this ",
                "function may be very large and use lots of machine storage ",
                "and memory.\n"))
  }
  #### 2. check for collection
  if (is.null(collection) == TRUE) {
    stop(paste0("Please select a GEOS-CF collection.\n"))
  }
  #### 3. check if collection is valid
  collections <- c("htf_inst_15mn_g1440x721_x1", "aqc_tavg_1hr_g1440x721_v1",
                   "chm_tavg_1hr_g1440x721_v1", "met_tavg_1hr_g1440x721_x1",
                   "xgc_tavg_1hr_g1440x721_x1", "chm_inst_1hr_g1440x721_p23",
                   "met_inst_1hr_g1440x721_p23")
  if (!(collection %in% collections)) {
    stop(paste0("Requested collection is not recognized.\n"))
  }
  #### 4. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 5. define time sequence
  if (stringr::str_sub(collection, -1, -1) == "1") {
    time_sequence <- as.character(seq(from = 30, to = 2330, by = 100))
    time_sequence <- stringr::str_pad(time_sequence,
                                      pad = "0",
                                      width = 4,
                                      side = "left")
  } else if (stringr::str_sub(collection, -1, -1) == "3") {
    time_sequence <- as.character(seq(from = 100, to = 2400, by = 100))
    time_sequence <- stringr::str_pad(time_sequence,
                                      pad = "0",
                                      width = 4,
                                      side = "left")
  }
  #### 6. define URL base
  base <- "https://portal.nccs.nasa.gov/datashare/gmao/geos-cf/v1/ana/"
  #### 7. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(directory_to_save,
                         collection,
                         "_wget_commands.txt")
  sink(commands_txt)
  #### 8. concatenate and print download commands to "..._wget_commands.txt"
  for (d in seq_along(date_sequence)){
    date <- date_sequence[d]
    year <- stringr::str_sub(date, 1, 4)
    month <- stringr::str_sub(date, 5, 6)
    day <- stringr::str_sub(date, 7, 8)
    for (t in seq_along(time_sequence)){
      download_url <- paste0(base,
                             "Y",
                             year,
                             "/M",
                             month,
                             "/D",
                             day,
                             "/GEOS-CF.v01.rpl.",
                             collection,
                             ".",
                             date,
                             "_",
                             time_sequence[t],
                             "z.nc4")
      download_folder <- paste0(directory_to_save,
                                collection)
      download_command <- paste0("wget ",
                                 download_url,
                                 " -P ",
                                 download_folder,
                                 "\n")
      cat(download_command)
    }
  }
  #### 9. finish "..._wget_commands.txt" file
  sink()
  #### 10. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 11. download data
  system(command = system_command)
  #### 12. remove "..._wget_commands.txt" file
  file.remove(commands_txt)
}
