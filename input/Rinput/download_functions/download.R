# download.R
# Here all functions for downloading data will be placed
# The wrapper function download_data will switch
# datasets to download data from the sources.
# edited 12/04/2023
# will be fully documented


download_data <-
  function(
    dataset_name = c("aqs", "ecoregion", "geos", "gmted", "koppen", "koppen-geiger", "koppengeiger", "merra2", "merra", "narr", "nlcd", "noaa", "sedac"),
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    parameter_code,
    resolution_temporal,
    variables = NULL,
    collection = NULL,
    data_resolution,
    product = c("MOD09GA", "MOD11A1", "MOD06_L2",
                "MCD19A2", "MOD13A2", "VNP46A2"),
    version = "61",
    horizontal_tiles = c(7, 13),
    vertical_tiles = c(3, 6),
    nasa_earth_data_token = NULL,
    directory_to_save = "./input/modis/raw/",
    data_download_acknowledgement = FALSE,
    write_command_only = FALSE
  ) {

    dataset_name <- match.arg(dataset_name)

    # determine whether the data exist and deter proceeding?
    # common elements ...
    # directory presence, sanity check in path strings
    # identify base URL
    # subdataset conditionals
    # flush wget commands
    #   save commands in txt file
    #   run commands
    # unzip
    # remove unzip (conditional)
  }


#' Check if input directory exists
#' @param directory character(1) directory path
#' @description If directory does not exist, the directory
#' will be created.
#' @returns NULL
#' @export
download_check_dir_exists <-
  function(directory) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }


#' Sanitize path to end with a forward slash
#' @param directory_to_download character(1). Path
#' @param directory_to_save character(1). Path
#' @returns A list of length 2
#' - \code{$directory_to_download}: directory to download data.
#' - \code{$directory_to_save}: directory to save data.
#' @export
download_sanitize_path <-
  function(directory_to_download, directory_to_save) {
    #### 1. directory setup
    chars_dir_download <- nchar(directory_to_download)
    chars_dir_save <- nchar(directory_to_save)
    if (substr(directory_to_download,
               chars_dir_download,
               chars_dir_download) != "/") {
      directory_to_download <-
        paste(directory_to_download,
              "/",
              sep = "")
    }
    if (substr(directory_to_save,
               chars_dir_save,
               chars_dir_save) != "/") {
      directory_to_save <-
        paste(directory_to_save,
              "/",
              sep = "")
    }
    sanitized <-
      list(
        directory_to_download = directory_to_download,
        directory_to_save = directory_to_save
      )
    return(sanitized)
  }



#' Check for data download acknowledgement
#' @param data_download_acknowledgement logical(1). Whether to
#' start downloading
#' @returns NULL
#' @export
download_permit <-
  function(data_download_acknowledgement) {
    if (!data_download_acknowledgement) {
      cat(paste0("Data download acknowledgement is set to FALSE.",
                 "Please acknowledge that the data downloaded using this",
                 "function may be very large and use lots of machine storage",
                 "and memory."))
      stop()
    }
  }



#' Run download commands
#' @param download_command character. Command that can be run in
#' console/terminal.
#' @returns NULL
#' @export
download_run <-
  function(
    download_command
  ) {
  cat(paste0("Downloading requested file...\n"))
  system(command = download_command)
  Sys.sleep(2L)
  cat(paste0("Requested file downloaded.\n"))
  }


#' Unzip downloaded data
#' @param download_name character(1). Full zip file path
#' @param directory_to_save character(1). Directory to unzip
#' data
#' @returns NULL
#' @export
download_unzip <-
  function(
    download_name,
    directory_to_save
  ) {
  #### 8. unzip downloaded data
  cat(paste0("Unzipping files...\n"))
  unzip(download_name,
        exdir = directory_to_save)
  cat(paste0("Files unzipped and saved in ",
             directory_to_save,
             ".\n"))
  }

#' Remove downloaded zip files
#' @param remove_download logical(1). Confirm removal.
#' @param download_name character. Full zip file path
#' @returns NULL
#' @export
download_remove_zips <-
  function(
    remove_download,
    download_name
  ) {
  #### 9. remove zip files
  if (remove_download == TRUE) {
    cat(paste0("Removing download files...\n"))
    file.remove(download_name)
    cat(paste0("Download files removed.\n"))
  }
  }




#' download_ecoregions: download Ecoregion Shapefiles from EPA.
#' @description
#' The \code{download_ecoregions()} function accesses and downloads Ecoregions
#' level 3 data, where all pieces of information in the higher levels are
#' included.
#' @param directory_to_download character(1). Directory to download zip file
#' of Ecoregion level 3 shapefiles
#' @param directory_to_save character(1). Directory to decompress zip files.
#' @param data_download_acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param remove_download logical(1). Remove download files in
#' directory_to_download.
#' @author Insang Song
#' @returns NULL;
#' @export
download_ecoregion <- function(
  directory_to_download = "./input/data/ecoregions/raw/",
  directory_to_save = "./input/data/ecoregions/raw/",
  data_download_acknowledgement = FALSE,
  remove_download = TRUE
) {
  if (!level %in% c(2, 3)) {
    stop("level should be one of 2 or 3.\n")
  }
  download_check_dir_exists(directory_to_save)
  download_check_dir_exists(directory_to_download)

  path_sanitized <-
    download_sanitize_path(directory_to_download = directory_to_download,
                           directory_to_save = directory_to_save)
  directory_to_download <- path_sanitized$directory_to_download
  directory_to_save <- path_sanitized$directory_to_save

  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 3. Check the presence of file
  ## This part is hard-coded as the original file appears to
  ## be a misnomer. May need to be modified accordingly in the future.
  path_downloaded_file <- sprintf("%sus_eco_l3_state_boundaries.shp", directory_to_save)
  if (file.exists(path_downloaded_file)) {
    message("Requested files exist in the target directory.\n")
    return(NULL)
  }
  # nolint start
  #### 4. define download URL
  download_url <-
    "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip"

  #### 5. build download file name
  download_name <- sprintf("%sus_eco_l3_state_boundaries.zip",
                           directory_to_download)
  #### 6. build download command
  download_command <- paste0("wget --no-check-certificate ",
                             download_url,
                             " -O ",
                             download_name,
                             "\n")
  #### 7. download data

  download_run(download_command = download_command)

  download_unzip(download_name = download_name,
    directory_to_save = directory_to_save)
  #### 9. remove zip files
  download_remove_zips(
    remove_download = remove_download,
    download_name = download_name
  )
}


# test
# download_ecoregions(data_download_acknowledgement = TRUE, remove_download = FALSE)
