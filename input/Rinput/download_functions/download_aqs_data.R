################################################################################
# Date modified: 2023-09-13
# Packages required: None
################################################################################

################################################################################
#' download_aqs_data: download daily data from AQS datamart
#' @param parameter_code integer(1). length of 5. EPA pollutant parameter code.
#' For details, please refer to
#' https://aqs.epa.gov/aqsweb/documents/codetables/parameters.html
#' @param year_start integer(1). length of 4. Start year for downloading data.
#' @param year_end integer(1). length of 4. End year for downloading data.
#' @param resolution_temporal character(1). Name of column containing POC
#' values. Currently, no value other than "daily" works.
#' @param directory_to_download character(1). Directory to download zip files
#' from AQS data mart.
#' @param directory_to_save character(1). Directory to decompress zip files
#' @param url_aqs_download character(1). URL to the AQS pre-generated datasets.
#' @param remove_zips logical(1). remove zip files in directory_to_download.
#' @author Mariana Kassien, Insang Song
#' @return NULL; Separate comma-separated value (CSV) files of monitors and the
#' daily representative values will be stored in directory_to_save.
#' @export
download_aqs_data <- function(
  parameter_code = 88101,
  year_start = 2018,
  year_end = 2022,
  resolution_temporal = "daily",
  directory_to_download = "./input/aqs/",
  directory_to_save = "./input/aqs/",
  url_aqs_download = "https://aqs.epa.gov/aqsweb/airdata/",
  remove_zips = FALSE
) {
  # nocov start
  #### 1. directory setup
  chars_dir_download <- nchar(directory_to_download)
  chars_dir_save <- nchar(directory_to_save)
  if (substr(directory_to_download,
             chars_dir_download,
             chars_dir_download) != "/") {
    directory_to_download <- paste(directory_to_download, "/", sep = "")
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
    # ISong: Some POCs are two digits, so here I changed POC slot to zero-padded
    # two digits.
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
  #### 5. Downloading monitor metadata file and filter for relevant sites
  # Download monitors file
  dest_monitors <- paste(directory_to_download, "aqs_monitors.zip", sep = "")
  if (!file.exists(dest_monitors)) {
    download.file(sprintf("%saqs_monitors.zip",
                          url_aqs_download),
                  dest_monitors)
  }
  # Unzip and read in
  unzip(dest_monitors, exdir = directory_to_save)
  monitors <- read.csv(sprintf("%saqs_monitors.csv",
                               directory_to_save),
                       stringsAsFactors = FALSE)
  # Create site identifier
  monitors$State.Code <- as.numeric(monitors$State.Code)
  # Convert from string to numeric to get rid of leading zeros,
  # the NAs introduced are from monitors in Canada with site number="CC"
  monitors$ID.Monitor <- sprintf(
    "%02d-%03d-%04d-%05d-%02d",
    monitors$State.Code,
    monitors$County.Code,
    monitors$Site.Num,
    monitors$Parameter.Code,
    monitors$POC
  )
  # Filter monitors file to include only monitors in our csv
  monitors_filter <-
    monitors[which(monitors$ID.Monitor %in% data_all$ID.Monitor), ]
  #### 5. Uploading data to desired folder
  cat(paste("All requested files were downloaded. Write the cleaned data to ",
            directory_to_save, "...\n", sep = ""))
  write.csv(data_all, paste(directory_to_save,
                            resolution_temporal,
                            "_",
                            parameter_code,
                            "_",
                            year_start,
                            "-",
                            year_end,
                            ".csv",
                            sep = ""))
  write.csv(monitors_filter, paste(directory_to_save,
                                   "monitors_",
                                   parameter_code,
                                   "_",
                                   year_start,
                                   "-",
                                   year_end,
                                   ".csv",
                                   sep = ""))
  if (remove_zips) {
    cat(paste("Delete zip files ... \n"))
    path_zips <- list.files(
      pattern = ".(zip|ZIP)$",
      path = directory_to_download,
      full.names = TRUE
    )
    for (zipfile in path_zips) {
      file.remove(zipfile)
    }
  }
  # nocov end
}
