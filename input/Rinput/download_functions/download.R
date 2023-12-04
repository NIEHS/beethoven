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



#### 2. check for data download acknowledgement
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




download_run <-
  function(
    download_command
  ) {
  cat(paste0("Downloading requested file...\n"))
  system(command = download_command)
  Sys.sleep(2L)
  cat(paste0("Requested file downloaded.\n"))
  }


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


