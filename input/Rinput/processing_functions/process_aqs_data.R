################################################################################
# Date modified: 2023-12-15
# Packages required: None
################################################################################

#' process_aqs_data: process daily data from AQS datamart
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
#' @param directory_with_data character(1).
#'  Directory with data from decompressed zip files.
#' @param directory_to_save character(1).
#'  Directory to save combined CSV files with unique monitor ID
#' @param remove_zip logical(1). Remove metadata zip file from
#' directory_with_data Default = `FALSE`.
#' @author Mariana Kassien, Insang Song, Mitchell Manware
#' @returns NULL; Separate comma-separated value (CSV) files of
#' monitors and the daily representative values
#' will be stored in directory_to_save.
#' @export
process_aqs_data <- function(
    parameter_code = 88101,
    year_start = 2018,
    year_end = 2022,
    resolution_temporal = "daily",
    directory_with_data = "../input/data/aqs/",
    directory_to_save = "../input/data/aqs/",
    remove_zip = FALSE
) {
  #### 1. directory setup
  download_setup_dir(directory_with_data)
  download_setup_dir(directory_to_save)
  directory_with_data <- download_sanitize_path(directory_with_data)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. define year sequence
  year_sequence <- seq(year_start, year_end, 1)
  #### 4. define unzipped csv names
  csv_names <- sprintf(
    paste(directory_with_data,
          resolution_temporal,
          "_",
          parameter_code,
          "_%.0f.csv",
          sep = ""
    ),
    year_sequence
  )
  data_all <- NULL
  for (c in seq_along(csv_names)) {
    #### 5. Read in dataframe
    cat(paste("reading and processing file: ", csv_names[c], "...\n"))
    data <- read.csv(csv_names[c], stringsAsFactors = FALSE)
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
    data_all <- rbind(data_all, data)
  }
  cat(paste("Downloading monitor metadata...\n"))
  #### 4. Downloading monitor metadata file and filter for relevant sites
  # Download monitors file
  dest_monitors <- paste(directory_with_data, "aqs_monitors.zip", sep = "")
  url_aqs_download <- "https://aqs.epa.gov/aqsweb/airdata/"
  if (!file.exists(dest_monitors)) {
    download.file(
      sprintf(
        "%saqs_monitors.zip",
        url_aqs_download
      ),
      dest_monitors
    )
  }
  # Unzip and read in
  unzip(dest_monitors, exdir = directory_to_save)
  monitors <-
    read.csv(
      sprintf(
        "%saqs_monitors.csv",
        directory_to_save
      ),
      stringsAsFactors = FALSE
    )
  # Create site identifier
  monitors$State.Code <- as.numeric(monitors$State.Code)
  # Convert from string to numeric to get rid of
  # leading zeros, the NAs introduced are from
  # monitors in Canada with site number="CC"
  monitors$ID.Monitor <-
    sprintf(
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
            directory_to_save, "...\n",
            sep = ""
  ))
  write.csv(
    data_all,
    paste(directory_to_save,
          resolution_temporal, "_",
          parameter_code, "_",
          year_start, "-",
          year_end, ".csv",
          sep = ""
    )
  )
  write.csv(
    monitors_filter,
    paste(directory_to_save,
          "monitors_",
          parameter_code, "_",
          year_start, "-",
          year_end, ".csv",
          sep = ""
    )
  )
  download_remove_zips(
    remove = remove_zip,
    download_name = dest_monitors
  )
}