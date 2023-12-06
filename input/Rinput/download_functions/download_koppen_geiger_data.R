################################################################################
# Date created: 2023-10-17
# Date edited: 2023-12-06
# Packages required: none
################################################################################

################################################################################
#' download_koppen_geiger_data: download climate classification data from the
#' Present and future Köppen-Geiger climate classification maps.
#' @description
#' The `download_koppen_geiger_data()` function accesses and downloads climate
#' classification data from the Present and future Köppen-Geiger climate
#' classification maps at 1-km resolution ([link for article]
#' (https://www.nature.com/articles/sdata2018214); [link for data]
#' (https://figshare.com/articles/dataset/Present_and_future_K_ppen-Geiger_
#' climate_classification_maps_at_1-km_resolution/6396959/2)).
#' @param time_period character(1). Available times are "Present" (1980-206) and
#' "Future" (2071-2100). ("Future" classifications are based on scenario
#' RCP8.5).
#' @param data_resolution character(1). Available resolutions are "0.0083"
#' degrees (~1km), "0.083" degrees (~10km), and "0.5" degrees (~50km).
#' @param directory_to_download character(1). Directory to download zip files
#' from Present and future Köppen-Geiger climate classification maps at 1-km
#' resolution.
#' @param directory_to_save character(1). Directory to decompress zip files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
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
download_koppen_geiger_data <- function(
  time_period = c("Present", "Future"),
  data_resolution = c("0.0083", "0.083", "0.5"),
  directory_to_download = "./input/data/koppen_geiger/",
  directory_to_save = "./input/data/koppen_geiger/",
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

  #### 3. check for data resolution
  if (is.null(data_resolution)) {
    stop(paste0("Please select a data resolution.\n"))
  }
  data_resolution <- match.arg(data_resolution)
  #### 4. check for valid time period
  if (is.null(time_period)) {
    stop(paste0("Please select a time period.\n"))
  }
  time_period <- match.arg(time_period)
  #### 6. define time period
  period <- tolower(time_period)
  #### 7. define data resolution
  data_resolution <- gsub("\\.", "p", data_resolution)
  #### 8 define download URL
  download_url <- "https://figshare.com/ndownloader/files/12407516"
  #### 9 build download file name
  download_name <- paste0(directory_to_download,
                          "koppen_geiger_",
                          period,
                          "_",
                          data_resolution,
                          ".zip")
  #### 10. build download command
  download_command <- paste0("wget ",
                             download_url,
                             " -O ",
                             download_name,
                             "\n")
  #### 11. initiate "..._wget_commands.txt"
  commands_txt <- paste0(directory_to_download,
                         "koppen_geiger_",
                         time_period,
                         "_",
                         data_resolution,
                         "_",
                         Sys.Date(),
                         "_wget_command.txt")
  download_sink(commands_txt)
  #### 12. concatenate and print download command to "..._wget_commands.txt"
  cat(download_command)
  #### 13. finish "..._wget_commands.txt" file
  sink()
  #### 14. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 15. download data
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
  #### 16. end if unzip == FALSE
  download_unzip(file_name = download_name,
                 directory_to_unzip = directory_to_save,
                 unzip = unzip)
  if (unzip) {
    #### 17. remove unwanted files
    unwanted_names <- list.files(path = directory_to_save,
                                pattern = "Beck_KG",
                                full.names = TRUE)
    unwanted_names <- as.vector(c(unwanted_names,
                                  paste0(directory_to_save,
                                        "KoppenGeiger.m")))
    tif <- paste0(directory_to_save,
                  "/Beck_KG_V1_",
                  period,
                  "_",
                  data_resolution,
                  ".tif")
    unwanted_names <- unwanted_names[grep(pattern = tif,
                                          unwanted_names,
                                          invert = TRUE)]
    file.remove(unwanted_names)
  }

  #### 18. Remove command file
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)
  #### 19. remove zip files
  download_remove_zips(remove = remove_zip,
                       download_name = download_name)

}
