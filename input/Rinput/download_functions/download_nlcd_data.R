################################################################################
# Date created: 2023-10-17
# Packages required: none
################################################################################

################################################################################
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
