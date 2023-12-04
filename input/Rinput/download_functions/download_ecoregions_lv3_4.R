################################################################################
# Date created: 2023-12-04
# Packages required: none
################################################################################

################################################################################
#' download_ecoregions: download Ecoregion levels 3 and 4 data from the
#' EPS.
#' @description
#' The `download_ecoregions()` function accesses and downloads Ecoregions
#' level 4 data, where all pieces of information in the higher levels are
#' included.
#' @param directory_to_download character(1). Directory to download zip file
#' of Ecoregion level 4 shapefiles
#' @param directory_to_save character(1). Directory to decompress zip files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param remove_download logical(1). Remove download files in
#' directory_to_download.
#' @author Insang Song
#' @returns NULL;
#' @export
download_ecoregions <- function(
  directory_to_download = "./input/ecoregions/raw/",
  directory_to_save = "./input/ecoregions/raw/",
  data_download_acknowledgement = FALSE,
  remove_download = TRUE
) {
  #### 2. check for data download acknowledgement
  if (data_download_acknowledgement == FALSE) {
    cat(paste0("Data download acknowledgement is set to FALSE.",
               "Please acknowledge that the data downloaded using this",
               "function may be very large and use lots of machine storage",
               "and memory."))
    stop()
  }
  #### 3. Check the presence of file
  ## This part is hard-coded as the original file appears to
  ## be a misnomer. May need to be modified accordingly in the future.
  path_downloaded_file <- sprintf("%sus_eco_l4.shp", directory_to_save)
  if (file.exists(path_downloaded_file)) {
    message("Requested files are present in the target directory.\n")
    return(NULL)
  }
  # nolint start
  #### 4. define download URL
  download_url <-
    "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4_state_boundaries.zip"
  # nolint end
  #### 5. build download file name
  download_name <- paste0(directory_to_download,
                          "us_eco_l4_state_boundaries.zip")
  #### 6. build download command
  download_command <- paste0("wget ",
                             download_url,
                             " -O ",
                             download_name,
                             "\n")
  #### 7. download data
  cat(paste0("Downloading requested file...\n"))
  system(command = download_command)
  Sys.sleep(2L)
  cat(paste0("Requested file downloaded.\n"))
  #### 8. unzip downloaded data
  cat(paste0("Unzipping files...\n"))
  unzip(download_name,
        exdir = directory_to_save)
  cat(paste0("Files unzipped and saved in ",
             directory_to_save,
             ".\n"))
  #### 9. remove zip files
  if (remove_download == TRUE) {
    cat(paste0("Removing download files...\n"))
    file.remove(download_name)
    cat(paste0("Download files removed.\n"))
  }
}
