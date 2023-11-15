################################################################################
# Date modified: 2023-11-15
# Packages required: None
################################################################################

################################################################################
#' download_tri_data: download daily data from EPA toxic release inventory
#'
#' @param year_start integer(1). length of 4. Start year for downloading data.
#' @param year_end integer(1). length of 4. End year for downloading data.
#' @param directory_to_save character(1). Directory to download files.
#' @param urls_download character(1). URL to the TRI pre-generated datasets.
#' @author Mariana Kassien
#' @return NULL; Yearly comma-separated value (CSV) raw files for each year and
#' a combined csv file with all years and some preprocessing (tbd)
#' @export
download_tri_data <- function(
    year_start = 2018,
    year_end = 2022,
    directory_to_save = "./input/tri/raw",
    url_download = "https://data.epa.gov/efservice/downloads/tri/
    mv_tri_basic_download/") {
  # nocov start
  chars_dir_save <- nchar(directory_to_save)

  if (substr(directory_to_save, chars_dir_save, chars_dir_save) != "/") {
    directory_to_save <- paste(directory_to_save, "/", sep = "")
  }

  #### 1. define measurement data paths
  year_sequence <- seq(year_start, year_end, 1)
  file_urls <- sprintf(paste(url_download, "%.0f", "_US/csv", sep = ""),
                       year_sequence)
  download_names <- sprintf(paste(directory_to_save, "tri_raw_%.0f.csv",
                                  sep = ""), year_sequence)

  #### 2. Downloading data
  # Download zip files from website
  if (!any(file.exists(download_names))) {
    download.file(file_urls, download_names, method = "libcurl")
  }

  #### 3. Concatenate with other years
  for (n in seq(1, length(download_names))) {
    data <- read.csv(download_names[n], stringsAsFactors = FALSE)
    if (n == 1) {
      data_all <- data
    } else {
      data_all <- rbind(data_all, data)
    }
  }
  #### 4. Placeholder for extra processing
  # (will be updated depending on what group decides)

  #### 5. Save processed data
  write.csv(data_all, paste(directory_to_save, "tri_processed_", year_start,
                            "-", year_end, ".csv", sep = ""))
  # nocov end
}
