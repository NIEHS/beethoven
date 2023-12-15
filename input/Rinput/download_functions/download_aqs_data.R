################################################################################
# Date modified: 2023-12-01
# Packages required: None
################################################################################

################################################################################
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
      directory_to_download = "../input/data/aqs/",
      directory_to_save = "../input/data/aqs/",
      url_aqs_download = "https://aqs.epa.gov/aqsweb/airdata/",
      data_download_acknowledgement = FALSE,
      unzip = TRUE,
      remove_zip = FALSE,
      download = FALSE,
      remove_command = FALSE) {
    #### 1. check for data download acknowledgement
    download_permit(
      data_download_acknowledgement = data_download_acknowledgement)
    #### 2. check for null parameteres
    check_for_null_parameters(mget(ls()))
    #### 3. directory setup
    directory_to_download <- download_sanitize_path(directory_to_download)
    directory_to_save <- download_sanitize_path(directory_to_save)
    download_setup_dir(directory_to_download)
    download_setup_dir(directory_to_save)
    #### 4. define year sequence
    year_sequence <- seq(year_start, year_end, 1)
    #### 5. build URLs
    download_urls <- sprintf(
      paste(url_aqs_download,
        resolution_temporal,
        "_",
        parameter_code,
        "_%.0f.zip",
        sep = ""
      ),
      year_sequence
    )
    #### 5. build download file name
    download_names <- sprintf(
      paste(directory_to_download,
        "aqs_",
        resolution_temporal,
        "_",
        parameter_code,
        "_%.0f.zip",
        sep = ""
      ),
      year_sequence
    )
    #### 6. build download command
    download_commands <- paste0("curl ",
                                download_urls,
                                " --output ",
                                download_names,
                                "\n")
    #### 7. initiate "..._curl_commands.txt"
    commands_txt <- paste0(
      directory_to_download,
      "aqs_",
      parameter_code,
      "_",
      year_start, "_", year_end,
      "_",
      resolution_temporal,
      "_curl_commands.txt"
    )
    download_sink(commands_txt)
    #### 8. concatenate and print download commands to "..._curl_commands.txt"
    writeLines(download_commands)
    #### 9. finish "..._curl_commands.txt" file
    sink()
    #### 10. build system command
    system_command <- paste0(
      ". ",
      commands_txt,
      "\n"
    )
    #### 11. download data
    if (!any(file.exists(download_names))) {
      download_run(download = download,
                   system_command = system_command)
    }
    #### 12. Construct string with unzipped file names
    csv_names <- sprintf(
      paste(directory_to_download,
        resolution_temporal,
        "_",
        parameter_code,
        "_%.0f.csv",
        sep = ""
      ),
      year_sequence
    )
    #### 13. unzip data
    for (n in seq_along(download_names)) {
      download_unzip(
        file_name = download_names[n],
        directory_to_unzip = directory_to_save,
        unzip = unzip
      )
    }
    #### 14. remove command file
    download_remove_command(
      commands_txt = commands_txt,
      remove = remove_command
    )
    #### 15. remove zip files
    for (d in seq_along(download_names)) {
      download_remove_zips(
        remove = remove_zip,
        download_name = download_names[d]
      )
    }
  }
