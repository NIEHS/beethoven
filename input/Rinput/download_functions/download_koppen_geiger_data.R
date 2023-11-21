################################################################################
# Date created: 2023-10-17
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
#' @param remove_download logical(1). Remove download files in
#' directory_to_download.
#' @author Mitchell Manware
#' @return NULL;
#' @export
download_koppen_geiger_data <- function(
  time_period = "Present",
  data_resolution = NULL,
  directory_to_download = "./input/koppen_geiger/raw/",
  directory_to_save = "./input/koppen_geiger/raw/",
  data_download_acknowledgement = FALSE,
  remove_download = TRUE
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
    cat(paste0("Data download acknowledgement is set to FALSE.",
               "Please acknowledge that the data downloaded using this",
               "function may be very large and use lots of machine storage",
               "and memory."))
    stop()
  }
  #### 3. check for data resolution
  if (is.null(data_resolution)) {
    cat(paste0("Please select a data resolution.\n"))
    stop()
  }
  #### 4. check for valid time period
  if (!(time_period %in% c("Present", "Future"))) {
    cat(paste0("Requested time period is not recognized.\n"))
    stop()
  }
  #### 5. check for valid data resolution
  if (!(data_resolution %in% c("0.0083", "0.083", "0.5"))) {
    cat(paste0("Requested time period is not recognized.\n"))
    stop()
  }
  #### 6. define time period
  period <- tolower(time_period)
  #### 7. define data resolution
  if (data_resolution == "0.0083") {
    resolution <- "0p0083"
  } else if (data_resolution == "0.083") {
    resolution <- "0p083"
  } else if (data_resolution == "0.5") {
    resolution <- "0p5"
  }
  #### 8 define download URL
  download_url <- "https://figshare.com/ndownloader/files/12407516"
  #### 9 build download file name
  download_name <- paste0(directory_to_download,
                          "koppen_geiger_",
                          period,
                          "_",
                          resolution,
                          ".zip")
  #### 10. build download command
  download_command <- paste0("wget ",
                             download_url,
                             " -O ",
                             download_name,
                             "\n")
  #### 11. download data
  cat(paste0("Downloading requested file...\n"))
  system(command = download_command)
  Sys.sleep(2L)
  cat(paste0("Requested file downloaded.\n"))
  #### 12. unzip downloaded data
  cat(paste0("Unzipping files...\n"))
  unzip(download_name,
        exdir = directory_to_save)
  cat(paste0("Files unzipped and saved in ",
             directory_to_save,
             ".\n"))
  #### 13. remove unwanted files
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
                resolution,
                ".tif")
  unwanted_names <- unwanted_names[grep(pattern = tif,
                                        unwanted_names,
                                        invert = TRUE)]
  file.remove(unwanted_names)
  #### 14. remove zip files
  if (remove_download == TRUE) {
    cat(paste0("Removing download files...\n"))
    file.remove(download_name)
    cat(paste0("Download files removed.\n"))
  }
}
