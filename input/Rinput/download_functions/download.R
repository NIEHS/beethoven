# download.R
#' download_data: a wrapper function for download covariate sources
#' @param dataset_name character(1). Dataset to download.
#' @param directory_to_save character(1). Directory to save / unzip
#'  (if zip files are downloaded) data.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param ... Arguments passed to each download function.
#' @note
#' - All download function names are in \code{download_*_data} formats
#' @author Insang Song
#' @seealso
#' - [download_aqs_data()]: "aqs"
#' - [download_ecoregion_data()]: "ecoregion"
#' - [download_geos_cf_data()]: "geos"
#' - [download_gmted_data()]: "gmted"
#' - [download_koppen_geiger_data()]: "koppen", "koppengeiger"
#' - [download_merra2_data()]: "merra2", "merra"
#' - [download_narr_monolevel_data()]: "narr_monolevel", "monolevel"
#' - [download_narr_p_levels_data()]: "narr_p_levels", "p_levels", "plevels"
#' - [download_nlcd_data()]: "nlcd",
#' - [download_noaa_hms_smoke_data()]: "noaa", "smoke", "hms"
#' - [download_sedac_groads_data()]: "sedac_groads", "groads"
#' - [download_sedac_population_data()]: "sedac_population", "population"
#' @returns NULL
#' @export
download_data <-
  function(
    dataset_name = c("aqs", "ecoregion", "geos", "gmted", "koppen",
    "koppengeiger", "merra2", "merra", "narr_monolevel",
    "narr_p_levels", "nlcd", "noaa", "sedac_groads", "sedac_population",
    "groads", "population", "plevels", "p_levels", "monolevel", "hms", "smoke"),
    directory_to_save = NULL,
    data_download_acknowledgement = FALSE,
    ...
  ) {

    dataset_name <- tolower(dataset_name)
    dataset_name <- match.arg(dataset_name)

    # determine whether the data exist and deter proceeding?
    what_to_run <- switch(dataset_name,
      aqs = download_aqs_data,
      ecoregion = download_ecoregion_data,
      geos = download_geos_cf_data,
      gmted = download_gmted_data,
      koppen = download_koppen_geiger_data,
      koppengeiger = download_koppen_geiger_data,
      merra2 = download_merra2_data,
      merra = download_merra2_data,
      narr_monolevel = download_narr_monolevel_data,
      monolevel = download_narr_monolevel_data,
      narr_p_levels = download_narr_p_levels_data,
      p_levels = download_narr_p_levels_data,
      plevels = download_narr_p_levels_data,
      nlcd = download_nlcd_data,
      noaa = download_noaa_hms_smoke_data,
      smoke = download_noaa_hms_smoke_data,
      hms = download_noaa_hms_smoke_data,
      sedac_groads = download_sedac_groads_data,
      groads = download_sedac_groads_data,
      sedac_population = download_sedac_population_data,
      population = download_sedac_population_data
    )

    tryCatch({
      what_to_run(
        directory_to_save = directory_to_save,
        data_download_acknowledgement = data_download_acknowledgement,
        ...
      )
    }, error = function(e) {
      print(e)
      print(args(what_to_run))
      message("Please refer to the argument list and the error message
      above to rectify the error.\n")
      return(NULL)
    })
  }


#' download_aqs_data: download daily data from AQS datamart
#' @param parameter_code integer(1). length of 5.
#'  EPA pollutant parameter code.
#'  For details, please refer to
#'  https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html
#' @param year_start integer(1). length of 4.
#'  Start year for downloading data.
#' @param year_end integer(1). length of 4.
#'  End year for downloading data.
#' @param resolution_temporal character(1).
#'  Name of column containing POC values.
#'  Currently, no value other than "daily" works.
#' @param directory_to_download character(1).
#'  Directory to download zip files from AQS data mart.
#' @param directory_to_save character(1).
#'  Directory to decompress zip files.
#' @param url_aqs_download character(1).
#'  URL to the AQS pre-generated datasets.
#' @param data_download_acknowledgement logical(1). By setting \code{TRUE} the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param remove_zips logical(1).
#'  Remove zip files in directory_to_download.
#' @author Mariana Kassien, Insang Song, Mitchell Manware
#' @returns NULL; Separate comma-separated value (CSV) files of
#'  monitors and the daily representative values
#'  will be stored in directory_to_save.
#' @export
download_aqs_data <-
  function(
      parameter_code = 88101,
      year_start = 2018,
      year_end = 2022,
      resolution_temporal = "daily",
      directory_to_download = "./input/data/aqs/",
      directory_to_save = "./input/data/aqs/",
      url_aqs_download = "https://aqs.epa.gov/aqsweb/airdata/",
      data_download_acknowledgement = FALSE,
      remove_zips = FALSE) {

    #### 1. directory setup
    download_permit(data_download_acknowledgement = data_download_acknowledgement)

    directory_to_download <- download_sanitize_path(directory_to_download)
    directory_to_save <- download_sanitize_path(directory_to_save)

    download_setup_dir(directory_to_download)
    download_setup_dir(directory_to_save)

    #### 2. define measurement data paths
    year_sequence <- seq(year_start, year_end, 1)
    file_urls <- sprintf(paste(url_aqs_download,
                              resolution_temporal,
                              "_",
                              parameter_code,
                              "_%.0f.zip",
                              sep = ""),
                        year_sequence)
    download_names <- sprintf(paste(directory_to_download,
                                    "download_output_%.0f.zip",
                                    sep = ""),
                              year_sequence)
    #### 3. Downloading data

    # Download zip files from website
    if (!any(file.exists(download_names))) {
      download.file(file_urls, download_names, method = "libcurl")
    }
    # Construct string with unzipped file names
    csv_names <- sprintf(paste(directory_to_download,
                              resolution_temporal,
                              "_",
                              parameter_code,
                              "_%.0f.csv",
                              sep = ""),
                        year_sequence)
    #### 4. Processing data
    # Unzip and read in .csv files, process and join in one dataframe.
    # The unique site identifier "ID.Monitor" is a string with the structure
    # State-County-Site-Parameter-POC
    for (n in seq(1, length(file_urls))) {
      unzip(download_names[n], exdir = directory_to_save)
      # Read in dataframe
      cat(paste("reading and processing file: ", csv_names[n], "...\n"))
      data <- read.csv(csv_names[n], stringsAsFactors = FALSE)
      # Make unique site identifier: State-County-Site-Parameter-POC
      data$ID.Monitor <- sprintf(
        "%02d-%03d-%04d-%05d-%02d",
        data$State.Code,
        data$County.Code,
        data$Site.Num,
        data$Parameter.Code,
        data$POC
      )
      # Concatenate with other years
      if (n == 1) {
        data_all <- data
      } else {
        data_all <- rbind(data_all, data)
      }
    }

    cat(paste("Downloading monitor metadata...\n"))
    #### 4. Downloading monitor metadata file and filter for relevant sites
    # Download monitors file
    dest_monitors <- paste(directory_to_download, "aqs_monitors.zip", sep = "")
    if (!file.exists(dest_monitors)) {
      download.file(
                    sprintf(
                            "%saqs_monitors.zip",
                            url_aqs_download),
                    dest_monitors)
    }

    # Unzip and read in
    unzip(dest_monitors, exdir = directory_to_save)
    monitors <-
      read.csv(sprintf("%saqs_monitors.csv",
                       directory_to_save),
               stringsAsFactors = FALSE)

    # Create site identifier
    monitors$State.Code <- as.numeric(monitors$State.Code)
    # Convert from string to numeric to get rid of
    # leading zeros, the NAs introduced are from
    # monitors in Canada with site number="CC"
    monitors$ID.Monitor <-
      sprintf("%02d-%03d-%04d-%05d-%02d",
              monitors$State.Code,
              monitors$County.Code,
              monitors$Site.Num,
              monitors$Parameter.Code,
              monitors$POC)
    # Filter monitors file to include only monitors in our csv
    monitors_filter <-
      monitors[which(monitors$ID.Monitor %in% data_all$ID.Monitor), ]
    #### 5. Uploading data to desired folder
    cat(paste("All requested files were downloaded. Write the cleaned data to ",
              directory_to_save, "...\n", sep = ""))

    write.csv(data_all,
              paste(directory_to_save,
                    resolution_temporal, "_",
                    parameter_code, "_",
                    year_start, "-",
                    year_end, ".csv", sep = ""))
    write.csv(monitors_filter,
              paste(directory_to_save,
                    "monitors_",
                    parameter_code, "_",
                    year_start, "-",
                    year_end, ".csv", sep = ""))

    if (remove_zips) {
      cat(paste("Delete zip files ... \n"))
      path_zips <- list.files(pattern = ".(zip|ZIP)$",
                              path = directory_to_download,
                              full.names = TRUE)
      file.remove(path_zips)
      cat(paste("Zip files were deleted. \n"))
    }
  }




#' download_ecoregion_data: download Ecoregion Shapefiles from EPA.
#' @description
#' The \code{download_ecoregion_data()} function accesses and downloads Ecoregions
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
download_ecoregion_data <- function(
  directory_to_download = "./input/data/ecoregions/raw/",
  directory_to_save = "./input/data/ecoregions/raw/",
  data_download_acknowledgement = FALSE,
  remove_download = TRUE
) {
  if (!level %in% c(2, 3)) {
    stop("level should be one of 2 or 3.\n")
  }
  download_setup_dir(directory_to_save)
  download_setup_dir(directory_to_download)

  directory_to_download <-
    download_sanitize_path(directory = directory_to_download)
  directory_to_save <-
    download_sanitize_path(directory = directory_to_save)

  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 3. Check the presence of file
  ## This part is hard-coded as the original file appears to
  ## be a misnomer. May need to be modified accordingly in the future.
  path_downloaded_file <- sprintf("%sus_eco_l3_state_boundaries.shp", directory_to_save)
  if (file.exists(path_downloaded_file)) {
    message("Requested files exist in the target directory.\n")
    return(NULL)
  }
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

  download_unzip(file_name = download_name,
    directory_to_unzip = directory_to_save)
  #### 9. remove zip files
  download_remove_zips(
    remove_download = remove_download,
    download_name = download_name
  )
}

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
#' @param download logical(1). `= FALSE` will generate a `.txt` file containing
#' all download commands. By setting `= TRUE` the function will download all of
#' the requested data files.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_geos_cf_data <- function(
  date_start = "2023-09-01",
  date_end = "2023-09-01",
  collection = c("aqc_tavg_1hr_g1440x721_v1", "chm_tavg_1hr_g1440x721_v1",
                   "met_tavg_1hr_g1440x721_x1", "xgc_tavg_1hr_g1440x721_x1",
                   "chm_inst_1hr_g1440x721_p23", "met_inst_1hr_g1440x721_p23"),
  directory_to_save = "./input/data/geos_cf/",
  data_download_acknowledgement = FALSE,
  download = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 3. check for collection
  if (is.null(collection) == TRUE) {
    stop(paste0("Please select a GEOS-CF collection.\n"))
  }
  #### 4. check if collection is valid
  collections <- c("aqc_tavg_1hr_g1440x721_v1", "chm_tavg_1hr_g1440x721_v1",
                   "met_tavg_1hr_g1440x721_x1", "xgc_tavg_1hr_g1440x721_x1",
                   "chm_inst_1hr_g1440x721_p23", "met_inst_1hr_g1440x721_p23")
  collection <- match.arg(collection)
  if (!(collection %in% collections)) {
    stop(paste0("Requested collection is not recognized.\n"))
  }
  #### 5. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 6. define time sequence
  collection_end <- substr(collection, nchar(collection), nchar(collection))
  if (collection_end == "1") {
    time_sequence <- as.character(seq(from = 30, to = 2330, by = 100))
  } else if (collection_end == "3") {
    time_sequence <- as.character(seq(from = 0, to = 2300, by = 100))
  }
  time_sequence <- sprintf("%04d", time_sequence)
  #### 7. define URL base
  base <- "https://portal.nccs.nasa.gov/datashare/gmao/geos-cf/v1/ana/"
  #### 8. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(directory_to_save,
                         collection,
                         "_",
                         date_start,
                         "_",
                         date_end,
                         "_wget_commands.txt")

  download_sink(commands_txt)
  #### 9. concatenate and print download commands to "..._wget_commands.txt"
  for (d in seq_along(date_sequence)){
    date <- date_sequence[d]
    year <- substr(date, 1, 4)
    month <- substr(date, 5, 6)
    day <- substr(date, 7, 8)
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
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)
}

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
#' the requested data files. Default is FALSE.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands. Default is FALSE.
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_gmted_data <- function(
  statistic = NULL,
  resolution = NULL,
  directory_to_download = "./input/data/gmted/",
  directory_to_save = "./input/data/gmted/",
  data_download_acknowledgement = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  download = FALSE,
  remove_command = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)
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
                         "_",
                         Sys.Date(),
                         "_curl_command.txt")
  download_sink(commands_txt)
  # sink(commands_txt)
  #### 14. concatenate and print download command to "..._curl_commands.txt"
  cat(download_command)
  #### 15. finish "..._curl_commands.txt" file
  sink()
  #### 16. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 17 download data
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
  #### 18. end if unzip == FALSE
  download_unzip(file_name = download_name,
                 directory_to_unzip = directory_to_save,
                 unzip = unzip)
  #### 19. Remove command file
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)
  #### 20. remove zip files
  download_remove_zips(remove = remove_zip,
                       download_name = download_name)
}


#' download_merra2_data: download meteorological and atmospheric data from the
#' Modern-Era Retrospective analysis for Research and Applications, Version 2
#' (MERRA-2) model.
#' @description
#' The `download_merra2_data()` function accesses and downloads various
#' meteorological and atmospheric collections from the [Modern-Era]
#' [Retrospective analysis for Research and Applications, Version 2 (MERRA-2)]
#' (https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/).
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param collection character(1). MERRA-2 data collection file name.
#' @param directory_to_save character(1). Directory to save data.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). `= FALSE` will generate a `.txt` file containing
#' all download commands. By setting `= TRUE` the function will download all of
#' the requested data files.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_merra2_data <- function(
  date_start = "2023-09-01",
  date_end = "2023-09-01",
  collection = NULL,
  directory_to_save = "../../data/covariates/merra2/",
  data_download_acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)

  #### 3. check for collection
  if (is.null(collection) == TRUE) {
    stop(paste0("Please select a MERRA2 collection.\n"))
  }
  #### 4. check if collection is recognized
  identifiers <- c("inst1_2d_asm_Nx M2I1NXASM 10.5067/3Z173KIE2TPD",
                   "inst1_2d_int_Nx M2I1NXINT 10.5067/G0U6NGQ3BLE0",
                   "inst1_2d_lfo_Nx M2I1NXLFO 10.5067/RCMZA6TL70BG",
                   "inst3_3d_asm_Np M2I3NPASM 10.5067/QBZ6MG944HW0",
                   "inst3_3d_aer_Nv M2I3NVAER 10.5067/LTVB4GPCOTK2",
                   "inst3_3d_asm_Nv M2I3NVASM 10.5067/WWQSXQ8IVFW8",
                   "inst3_3d_chm_Nv M2I3NVCHM 10.5067/HO9OVZWF3KW2",
                   "inst3_3d_gas_Nv M2I3NVGAS 10.5067/96BUID8HGGX5",
                   "inst3_2d_gas_Nx M2I3NXGAS 10.5067/HNGA0EWW0R09",
                   "inst6_3d_ana_Np M2I6NPANA 10.5067/A7S6XP56VZWS",
                   "inst6_3d_ana_Nv M2I6NVANA 10.5067/IUUF4WB9FT4W",
                   "statD_2d_slv_Nx M2SDNXSLV 10.5067/9SC1VNTWGWV3",
                   "tavg1_2d_adg_Nx M2T1NXADG 10.5067/HM00OHQBHKTP",
                   "tavg1_2d_aer_Nx M2T1NXAER 10.5067/KLICLTZ8EM9D",
                   "tavg1_2d_chm_Nx M2T1NXCHM 10.5067/3RQ5YS674DGQ",
                   "tavg1_2d_csp_Nx M2T1NXCSP 10.5067/H0VVAD8F6MX5",
                   "tavg1_2d_flx_Nx M2T1NXFLX 10.5067/7MCPBJ41Y0K6",
                   "tavg1_2d_int_Nx M2T1NXINT 10.5067/Q5GVUVUIVGO7",
                   "tavg1_2d_lfo_Nx M2T1NXLFO 10.5067/L0T5GEG1NYFA",
                   "tavg1_2d_lnd_Nx M2T1NXLND 10.5067/RKPHT8KC1Y1T",
                   "tavg1_2d_ocn_Nx M2T1NXOCN 10.5067/Y67YQ1L3ZZ4R",
                   "tavg1_2d_rad_Nx M2T1NXRAD 10.5067/Q9QMY5PBNV1T",
                   "tavg1_2d_slv_Nx M2T1NXSLV 10.5067/VJAFPLI1CSIV",
                   "tavg3_3d_mst_Ne M2T3NEMST 10.5067/JRUZ3SJ3ZJ72",
                   "tavg3_3d_trb_Ne M2T3NETRB 10.5067/4I7ZI35QRH8K",
                   "tavg3_3d_nav_Ne M2T3NENAV 10.5067/N5WAKNS1UYQN",
                   "tavg3_3d_cld_Np M2T3NPCLD 10.5067/TX10URJSKT53",
                   "tavg3_3d_mst_Np M2T3NPMST 10.5067/0TUFO90Q2PMS",
                   "tavg3_3d_rad_Np M2T3NPRAD 10.5067/3UGE8WQXZAOK",
                   "tavg3_3d_tdt_Np M2T3NPTDT 10.5067/9NCR9DDDOPFI",
                   "tavg3_3d_trb_Np M2T3NPTRB 10.5067/ZRRJPGWL8AVL",
                   "tavg3_3d_udt_Np M2T3NPUDT 10.5067/CWV0G3PPPWFW",
                   "tavg3_3d_odt_Np M2T3NPODT 10.5067/S0LYTK57786Z",
                   "tavg3_3d_qdt_Np M2T3NPQDT 10.5067/A9KWADY78YHQ",
                   "tavg3_3d_asm_Nv M2T3NVASM 10.5067/SUOQESM06LPK",
                   "tavg3_3d_cld_Nv M2T3NVCLD 10.5067/F9353J0FAHIH",
                   "tavg3_3d_mst_Nv M2T3NVMST 10.5067/ZXTJ28TQR1TR",
                   "tavg3_3d_rad_Nv M2T3NVRAD 10.5067/7GFQKO1T43RW",
                   "tavg3_2d_glc_Nx M2T3NXGLC 10.5067/9ETB4TT5J6US")
  identifiers <- lapply(identifiers, strsplit, split = " ")
  identifiers <- lapply(identifiers, function(x) matrix(x[[1]], nrow = 1))
  identifiers <- do.call(rbind, identifiers)
  identifiers_df <- as.data.frame(identifiers)
  colnames(identifiers_df) <- c("collection_id", "estd_name", "DOI")

  if (!(collection %in% identifiers_df$collection_id)) {
    print(identifiers_df)
    stop(paste0("Requested collection is not recognized.\n
    Please refer to the table above to find a proper collection.\n"))
  }
  #### 4. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 5. define ESDT name and DOI
  identifiers_df_requested <- subset(identifiers_df,
                                     subset =
                                       identifiers_df$collection_id ==
                                       collection)
  esdt_name <- identifiers_df_requested[, 2]
  cat(paste0("Collection: ",
             collection,
             " | ESDT Name: ",
             esdt_name,
             " | DOI: ",
             identifiers_df_requested[, 3],
             "\n"))
  #### 6. define URL base
  #### NOTE: sorted and defined manually according to
  ####       https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/ &
  ####       https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/
  esdt_name_4 <- c("M2I1NXASM", "M2I1NXINT", "M2I1NXLFO", "M2I3NXGAS",
                   "M2SDNXSLV", "M2T1NXADG", "M2T1NXAER", "M2T1NXCHM",
                   "M2T1NXCSP", "M2T1NXFLX", "M2T1NXINT", "M2T1NXLFO",
                   "M2T1NXLND", "M2T1NXOCN", "M2T1NXRAD", "M2T1NXSLV",
                   "M2T3NXGLC")
  esdt_name_5 <- c("M2I3NPASM", "M2I3NVAER", "M2I3NVASM", "M2I3NVCHM",
                   "M2I3NVGAS", "M2I6NPANA", "M2I6NVANA", "M2T3NEMST",
                   "M2T3NENAV", "M2T3NETRB", "M2T3NPCLD", "M2T3NPMST",
                   "M2T3NPODT", "M2T3NPQDT", "M2T3NPRAD", "M2T3NPTDT",
                   "M2T3NPTRB", "M2T3NPUDT", "M2T3NVASM", "M2T3NVCLD",
                   "M2T3NVMST", "M2T3NVRAD")
  if (esdt_name %in% esdt_name_4) {
    base <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/"
  } else if (esdt_name %in% esdt_name_5) {
    base <- "https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/"
  }
  #### 7. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(directory_to_save,
                         collection,
                         "_",
                         date_start,
                         "_",
                         date_end,
                         "_wget_commands.txt")
  download_sink(commands_txt)
  #### 8. concatenate and print download commands to "..._wget_commands.txt"
  for (d in seq_along(date_sequence)) {
    date <- date_sequence[d]
    year <- as.character(substr(date, 1, 4))
    month <- as.character(substr(date, 5, 6))
    year_num <- as.numeric(year)
    #### 8.1 define Stream and Version number
    #### NOTE: sorted and defined manually according
    ####       MERRA2 File Specification
    ####       https://gmao.gsfc.nasa.gov/pubs/docs/Bosilovich785.pdf
    if (year_num > 2010) {
      hundreds <- "400"
    } else if (year_num <= 2010 && year_num > 2001) {
      hundreds <- "300"
    } else if (year_num <= 2001 && year_num > 1991) {
      hundreds <- "200"
    } else if (year_num <= 1991) {
      hundreds <- "100"
    }
    download_url <- paste0(base,
                           esdt_name,
                           ".5.12.4/",
                           year,
                           "/",
                           month,
                           "/MERRA2_",
                           hundreds,
                           ".",
                           collection,
                           ".",
                           date,
                           ".nc4")
    download_folder <- paste0(directory_to_save,
                              collection)
    download_command <- paste0("wget ",
                               download_url,
                               " -P ",
                               download_folder,
                               "\n")
    cat(download_command)
    download_url_metadata <- paste0(base,
                                    esdt_name,
                                    ".5.12.4/",
                                    year,
                                    "/",
                                    month,
                                    "/MERRA2_",
                                    hundreds,
                                    ".",
                                    collection,
                                    ".",
                                    date,
                                    ".nc4.xml")
    download_folder_metadata <- paste0(directory_to_save,
                                       collection,
                                       "/metadata/")
    download_command_metadata <- paste0("wget ",
                                        download_url_metadata,
                                        " -P ",
                                        download_folder_metadata,
                                        "\n")
    cat(download_command_metadata)
  }
  #### 9. finish "..._wget_commands.txt"
  sink()
  #### 10. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")

  #### 11. download data
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
  #### 18. Remove command file
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)

}



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
#' @param variables character. Variable(s) name acronym.
#' @param directory_to_save character(1). Directory(s) to save downloaded data
#' files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the user
#' acknowledge that the data downloaded using this function may be very large
#' and use lots of machine storage and memory.
#' @param download logical(1). `= FALSE` will generate a `.txt` file containing
#' all download commands. By setting `= TRUE` the function will download all of
#' the requested data files.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_narr_monolevel_data <- function(
  year_start = 2022,
  year_end = 2022,
  variables = NULL,
  directory_to_save = "./input/data/narr/",
  data_download_acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 3. check for variables
  if (is.null(variables)) {
    stop(paste0("Please select an NCEP-NARR variable.\n"))
  }
  #### 4. define years sequence
  if (any(nchar(year_start) != 4, nchar(year_end) != 4)) {
    stop("year_start and year_end should be 4-digit integers.\n")
  }
  years <- seq(year_start, year_end, 1)
  #### 5. define variables
  variables_list <- as.vector(variables)
  #### 6. define URL base
  base <- "https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/"
  #### 7. initiate "..._curl_commands.txt"
  commands_txt <- paste0(directory_to_save,
                         "narr_monolevel_",
                         year_start, "_", year_end,
                         "_curl_commands.txt")
  download_sink(commands_txt)
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
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
}


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
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_narr_p_levels_data <- function(
  year_start = 2022,
  year_end = 2022,
  variables = NULL,
  directory_to_save = "../../data/covariates/narr/",
  data_download_acknowledgement = FALSE,
  download = FALSE,
  remove_command = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)

  #### 2. directory setup
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)

  #### 3. check for variables
  if (is.null(variables) == TRUE) {
    stop(paste0("Please select an NCEP-NARR variable.\n"))
  }
  #### 4. define years sequence
  years <- seq(year_start, year_end, 1)
  #### 5. define months sequence
  months <- sprintf("%02d", seq(1, 12, by = 1))

  #### 6. define variables
  variables_list <- as.vector(variables)
  #### 7. define URL base
  base <- "https://downloads.psl.noaa.gov//Datasets/NARR/Dailies/pressure/"
  #### 8. initiate "..._curl_commands.txt"
  commands_txt <- paste0(directory_to_save,
                         "narr_p_levels_",
                         year_start,
                         "_",
                         year_end,
                         "_curl_commands.txt")
  download_sink(commands_txt)  
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
  #### 13. Remove command file
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)

}



#' download_nlcd_data: download land cover data from the National Land Cover
#' Database Science Research Products.
#' @description
#' The `download_nlcd_data()` function accesses and downloads land cover data
#' from the [NLCD Science Research Products](https://www.mrlc.gov/data)
#' data base.
#' @param year integer(1). Available years for Coterminous United States include
#' 2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, and 2021. Available years for
#' Alaska include 2001, 2011, and 2016.
#' @param collection character(1). "Coterminous United States" or "Alaska".
#' @param directory_to_download character(1). Directory to download zip files
#' from National Land Cover Database Science Research Products.
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
#' @returns NULL;
#' @export
download_nlcd_data <- function(
  year = 2021,
  collection = "Coterminous United States",
  directory_to_download = "../../data/covariates/nlcd/",
  directory_to_save = "../../data/covariates/nlcd/",
  data_download_acknowledgement = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  download = FALSE,
  remove_command = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)

  #### 2. check for valid years
  valid_years <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, 2021)
  if (!(year %in% valid_years)) {
    stop(paste0("Requested year is not recognized.\n"))
  }
  #### 3. define URL base
  base <- "https://s3-us-west-2.amazonaws.com/mrlc/"
  #### 4. define collection code
  if (collection == "Coterminous United States") {
    collection_code <- paste0("nlcd_",
                              as.character(year),
                              "_land_cover_l48_")
  } else if (collection == "Alaska") {
    collection_code <- paste0("NLCD_",
                              as.character(year),
                              "_Land_Cover_AK_")
  }
  #### 5. define release date
  #### NOTE: release dates identified by inspecting URLs on from
  ####       https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover
  if (year == 2021 && collection == "Coterminous United States") {
    release_date <- "20230630"
  } else if (!(year == 2021) && collection == "Coterminous United States") {
    release_date <- "20210604"
  } else if (collection == "Alaska") {
    release_date <- "20200724"
  }
  #### 6. build URL
  download_url <- paste0(base,
                         collection_code,
                         release_date,
                         ".zip")
  #### 7. build download file name
  download_name <- paste0(directory_to_download,
                          collection_code,
                          release_date,
                          ".zip")
  #### 8. build system command
  download_command <- paste0("curl -o ",
                             download_name,
                             " --url ",
                             download_url,
                             "\n")
  #### 9. initiate "..._curl_command.txt"
  commands_txt <- paste0(directory_to_download,
                         collection_code,
                         "_",
                         Sys.Date(),
                         "_curl_command.txt")
  download_sink(commands_txt)
  #### 10. concatenate and print download command to "..._curl_commands.txt"
  cat(download_command)
  #### 11. finish "..._curl_command.txt"
  sink()
  #### 12. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 13. download data
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
  #### 14. end if unzip == FALSE
  download_unzip(file_name = download_name,
                 directory_to_unzip = directory_to_save,
                 unzip = unzip)
  #### 15. unzip downloaded data
  cat(paste0("Unzipping files...\n"))
  unzip(download_name,
        exdir = directory_to_save)
  cat(paste0("Files unzipped and saved in ",
             directory_to_save,
             ".\n"))
  #### 16. remove zip files
  download_remove_zips(remove = remove_zip,
                       download_name = download_name)
}


#' download_sedac_groads_data: download Global Roads Open Access Data
#' Set (gROADS), v1 (1980-2010) data from NASA Socioeconomic Data and
#' Applications Center (SEDAC).
#' @description
#' The `download_sedac_groads_data()` function accesses and downloads
#' roads data from the National Aeronautics and Space
#' Administration's (NASA) [Global Roads Open Access Data Set]
#' (https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-
#' v1/data-download).
#' @param data_format character(1). Data can be downloaded as "Shapefile" or
#' "Geodatabase". (Only "Geodatabase" available for "Global" region).
#' @param data_region character(1). Data can be downloaded for "Global",
#' "Africa", "Asia", "Europe", "Americas", "Oceania East", and "Oceania West".
#' @param directory_to_download character(1). Directory to download zip files
#' from NASA UN WPP-Adjusted Population Density, v4.11.
#' @param directory_to_save character(1). Directory to decompress zip files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the user
#' acknowledge that the data downloaded using this function may be very large
#' and use lots of machine storage and memory.
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
download_sedac_groads_data <- function(
  data_format = c("Shapefile", "Geodatabase"),
  data_region = c("Americas", "Global", "Africa", "Asia",
                  "Europe", "Oceania East", "Oceania West"),
  directory_to_download = "./input/data/sedac_groads/",
  directory_to_save = "./input/data/sedac_groads/",
  data_download_acknowledgement = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  download = FALSE
) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)

  #### 3. check for region
  if (is.null(data_region)) {
    stop(paste0("Please select a data region.\n"))
  }
  #### 4. check if region is valid
  regions <- c("Global", "Africa", "Asia", "Europe",
               "Americas", "Oceania East", "Oceania West")
  data_region <- match.arg(data_region)
  #### 5. check for data format
  # formats <- c("Shapefile", "Geodatabase")
  # if (!(data_format %in% formats)) {
  #   stop(paste0("Requested data format not recognized.\n"))
  # }
  formats <- match.arg(formats)
  #### 6. define URL base
  base <- paste0("https://sedac.ciesin.columbia.edu/downloads/data/groads/",
                 "groads-global-roads-open-access-v1/",
                 "groads-v1-")
  #### 7. define data format
  if (data_format == "Shapefile") {
    format <- "shp"
  } else if (data_format == "Geodatabase") {
    format <- "gdb"
  }
  #### 8. coerce region to lower case
  region <- tolower(data_region)
  #### 9. build download URL
  download_url <- paste0(base,
                         gsub(" ", "-", region),
                         "-",
                         format,
                         ".zip")
  #### 10. build download file name
  download_name <- paste0(directory_to_download,
                          "groads_v1_",
                          gsub(" ", "-", region),
                          "_",
                          format,
                          ".zip")
  #### 11. build system command
  download_command <- paste0("curl -n -c ~/.urs_cookies -b ~/.urs_cookies -LJ",
                             " -o ",
                             download_name,
                             " --url ",
                             download_url,
                             "\n")
  #### 12. initiate "..._curl_commands.txt"
  commands_txt <- paste0(directory_to_download,
                         "sedac_groads_",
                         gsub(" ", "_", region),
                         "_",
                         Sys.Date(),
                         "_curl_command.txt")
  download_sink(commands_txt)
  #### 13. concatenate and print download command to "..._curl_commands.txt"
  cat(download_command)
  #### 14. finish "..._curl_commands.txt" file
  sink()
  #### 15. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 16. download data
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
  #### 17. end if unzip == FALSE
  download_unzip(file_name = download_name,
                 directory_to_unzip = directory_to_save,
                 unzip = unzip)

  #### 18. Remove command file
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)
  #### 19. remove zip files
  download_remove_zips(remove = remove_zip,
                       download_name = download_name)

}


#' download_sedac_population_data: download UN WPP-Adjusted population density
#' data from NASA Socioeconomic Data and Applications Center (SEDAC).
#' @description
#' The `download_sedac_population_data()` function accesses and downloads
#' population density data from the National Aeronatuics and Space
#' Administration's (NASA) [UN WPP-Adjusted Population Density, v4.11]
#' (https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-adjuste
#' d-to-2015-unwpp-country-totals-rev11).
#' @param year character(1). Available years are 2000, 2005, 2010, 2015, and
#' 2020, or "all" for all years.
#' @param data_format character(1). Individual year data can be downloaded as
#' "ASCII" or "GeoTIFF". "all" years is downloaded as "netCDF".
#' @param data_resolution character(1). Available resolutions are 30 second
#' (~1km), 2.5 minute (~5km), 15 minute (~30km), 30 minute (~55km), and 60
#' minute (~110km).
#' @param directory_to_download character(1). Directory to download zip files
#' from NASA UN WPP-Adjusted Population Density, v4.11.
#' @param directory_to_save character(1). Directory to decompress zip files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the user
#' acknowledge that the data downloaded using this function may be very large
#' and use lots of machine storage and memory.
#' @param download logical(1). `= FALSE` will generate a `.txt` file containing
#' all download commands. By setting `= TRUE` the function will download all of
#' the requested data files.
#' @param unzip logical(1). Unzip zip files. Default = `TRUE`.
#' @param remove_zip logical(1). Remove zip files from directory_to_download.
#' Default = `FALSE`.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_sedac_population_data <- function(
  year = "2020",
  data_format = "GeoTIFF",
  data_resolution = "60 minute",
  directory_to_download = "../../data/covariates/sedac_population/",
  directory_to_save = "../../data/covariates/sedac_population/",
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

  #### 3. check for data format
  if (is.null(data_format)) {
    stop(paste0("Please select a data format.\n"))
  }
  #### 4. check for data resolution
  if (is.null(data_resolution)) {
    stop(paste0("Please select a data resolution.\n"))
  }
  #### 5. define URL base
  base <- paste0("https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/")
  #### 6. define year
  if (year == "all") {
    year <- "totpop"
  } else {
    year <- as.character(year)
  }
  #### 7. define data resolution
  if (data_resolution == "60 minute") {
    resolution <- "1_deg"
  } else if (data_resolution == "30 second") {
    resolution <- "30_sec"
  } else if (data_resolution == "2.5 minute") {
    resolution <- "2pt5_min"
  } else if (data_resolution == "15 minute") {
    resolution <- "15_min"
  } else if (data_resolution == "30 minute") {
    resolution <- "30_min"
  }
  #### 8. define data format
  if (data_format == "GeoTIFF" && year != "totpop") {
    format <- "tif"
  } else if (data_format == "GeoTIFF" && year == "totpop") {
    format <- "nc"
    cat(paste0("Data for all years is only available in netCDF format. ",
               "Data will be downloaded as netCDF.\n"))
  } else if (data_format == "ASCII" && year != "totpop") {
    format <- "asc"
  } else if (data_format == "ASCII" && year == "totpop") {
    cat(paste0("Data for all years is only available in netCDF format. ",
               "Data will be downloaded as netCDF.\n"))
  } else if (data_format == "netCDF") {
    format <- "nc"
  }
  #### 9. build download URL
  download_url <- paste0(base,
                         "gpw-v4-population-density-adjusted-to-2015-unwpp-",
                         "country-totals-rev11/",
                         "gpw-v4-population-density-adjusted-to-2015-unwpp-",
                         "country-totals-rev11_",
                         year,
                         "_",
                         resolution,
                         "_",
                         format,
                         ".zip")
  #### 10. build download file name
  download_name <- paste0(directory_to_download,
                          "gpw_v4_population_density_adjusted_to_2015_unwpp_",
                          "country_totals_rev11_",
                          year,
                          "_",
                          resolution,
                          "_",
                          format,
                          ".zip")
  #### 11. build system command
  download_command <- paste0("curl -n -c ~/.urs_cookies -b ~/.urs_cookies -LJ",
                             " -o ",
                             download_name,
                             " --url ",
                             download_url,
                             "\n")
  #### 12. initiate "..._curl_command.txt"
  commands_txt <- paste0(directory_to_download,
                         "sedac_population_",
                         year,
                         "_",
                         resolution,
                         "_",
                         Sys.Date(),
                         "_curl_commands.txt")
  download_sink(commands_txt)
  #### 13. concatenate and print download command to "..._curl_commands.txt"
  cat(download_command)
  #### 14. finish "..._curl_commands.txt" file
  sink()
  #### 15. build system command
  system_command <- paste0(". ",
                           commands_txt,
                           "\n")
  #### 16. download data
  download_run(download = download,
               system_command = system_command,
               commands_txt = commands_txt)
  #### 17. end if unzip == FALSE
  download_unzip(file_name = download_name,
                 directory_to_unzip = directory_to_save,
                 unzip = unzip)
  #### 18. Remove command file
  download_remove_command(commands_txt = commands_txt,
                          remove = remove_command)
  #### 19. remove zip files
  download_remove_zips(remove = remove_zip,
                       download_name = download_name)
}


#' download_noaa_hms_smoke_data: download daily wildfire smoke plume data from
#' NOAA Hazard Mapping System Fire and Smoke Product
#' @description
#' The `download_noaa_hms_smoke_data()` function accesses and downloads wildfire
#' smoke plume coverage data from the National Oceanic and Atmospheric
#' Administration's (NOAA) [Hazard Mapping System Fire and Smoke Product]
#' (https://www.ospo.noaa.gov/Products/land/hms.html#0).
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 10, 2023 = "2023-09-10").
#' @param data_format character(1). "Shapefile" or "KML".
#' @param directory_to_download character(1). Directory to download zip files
#' from NOAA Hazard Mapping System Fire and Smoke Product. (Ignored if
#' `data_format = "KML"`.)
#' @param directory_to_save character(1). Directory to save unzipped shapefiles
#' and KML files.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the user
#' acknowledge that the data downloaded using this function may be very large
#' and use lots of machine storage and memory.
#' @param unzip logical(1). Unzip zip files. Default = `TRUE`. (Ignored if
#' `data_format = "KML"`.)
#' @param remove_zip logical(1). Remove zip files from
#' directory_to_download. Default = `FALSE`. (Ignored if `data_format = "KML"`.)
#' @author Mitchell Manware, Insang Song
#' @return NULL;
#' @export
download_noaa_hms_smoke_data <- function(
  date_start = "2023-09-01",
  date_end = "2023-09-01",
  data_format = "Shapefile",
  directory_to_download = "./input/data/noaa_hms/",
  directory_to_save = "./input/data/noaa_hms/",
  data_download_acknowledgement = FALSE,
  download = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  remove_command = FALSE
) {
  # nocov start
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. directory setup
  download_setup_dir(directory_to_download)
  download_setup_dir(directory_to_save)
  directory_to_download <- download_sanitize_path(directory_to_download)
  directory_to_save <- download_sanitize_path(directory_to_save)

  #### 3. check for unzip == FALSE && remove_zip == TRUE
  if (unzip == FALSE && remove_zip == TRUE) {
    stop(paste0("Arguments `unzip = FALSE` and `remove_zip = TRUE` are not ",
                "acceptable together. Please change one.\n"))
  }
  #### 4. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 5. define URL base
  base <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/"
  #### 6. define empty vectors
  file_urls <- NULL
  download_names <- NULL
  #### 7. add download URLs and file names to empty vectors
  for (f in seq_along(date_sequence)) {
    year <- substr(date_sequence[f], 1, 4)
    month <- substr(date_sequence[f], 5, 6)
    if (data_format == "Shapefile") {
      file_urls_add <- paste0(
        base,
        data_format,
        "/",
        year,
        "/",
        month,
        "/hms_smoke",
        date_sequence[f],
        ".zip"
      )
      file_urls <- c(file_urls, file_urls_add)
      download_names_add <- paste0(
        directory_to_download,
        "hms_smoke_",
        data_format,
        "_",
        date_sequence[f],
        ".zip"
      )
      download_names <- c(download_names, download_names_add)
    } else if (data_format == "KML") {
      file_urls_add <- paste0(
        base,
        data_format,
        "/",
        year,
        "/",
        month,
        "/hms_smoke",
        date_sequence[f],
        ".kml"
      )
      file_urls <- c(file_urls, file_urls_add)
      download_names_add <- paste0(
        directory_to_save,
        "hms_smoke_",
        data_format,
        "_",
        date_sequence[f],
        ".kml"
      )
      download_names <- c(download_names, download_names_add)
    }
  }
  #### 8. download data
  if (!any(file.exists(download_names))) {
    cat(paste0("Downloading requested files...\n"))
    download.file(file_urls, download_names, method = "libcurl", quiet = TRUE)
    cat(paste0("Requested files downloaded.\n"))
  }
  #### 9. end if unzip == FALSE
  if (unzip == FALSE && data_format == "Shapefile") {
    return(cat(paste0("Downloaded files will not be unzipped.\n")))
  }
  #### 10. unzip files
  if (unzip == TRUE && data_format == "Shapefile") {
    cat(paste0("Unzipping zip files to ", directory_to_save, "...\n"))
    for (u in seq_along(download_names)) {
      unzip(download_names[u], exdir = directory_to_save)
    }
    cat(paste0("Zip files unzipped.\n"))
  }
  #### 11. remove zip files
  if (remove_zip == TRUE && data_format == "Shapefile") {
    cat(paste0("Removing zip files...\n"))
    for (z in seq_along(download_names)) {
      file.remove(download_names[z])
    }
    cat(paste0("Zip files removed\n"))
  }
  # nocov end
}


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
