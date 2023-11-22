################################################################################
# Date modified: 2023-11-15
# Packages required: None
################################################################################

################################################################################
#' download_tri_data: download data from EPA toxic release inventory
#'
#' @param year_start integer(1). length of 4. Start year for downloading data.
#' @param year_end integer(1). length of 4. End year for downloading data.
#' @param directory_to_save character(1). Directory to download files.
#' @param test_mode logical(1). Turn off download to allow for function testing.
#' @author Mariana Kassien
#' @return NULL; Yearly comma-separated value (CSV) raw files for each year
#' @export
download_tri_data <- function(
    year_start = 2018,
    year_end = 2022,
    directory_to_save = "./input/data/covariates/",
    test_mode = FALSE
    )
    {
  # nocov start
  chars_dir_save <- nchar(directory_to_save)

  if (substr(directory_to_save, chars_dir_save, chars_dir_save) != "/") {
    directory_to_save <- paste(directory_to_save, "/", sep = "")
  }

  #### 1. define measurement data paths
  url_download = "https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/"
  year_sequence <- seq(year_start, year_end, 1)
  file_urls <- sprintf(
    paste(url_download, "%.0f", "_US/csv", sep = ""),
    year_sequence
  )
  download_names <- sprintf(paste(directory_to_save, "tri_raw_%.0f.csv",
    sep = ""
  ), year_sequence)

  #### 2. test mode
  if (test_mode == FALSE) {
    #### 3. Downloading data
    # Download zip files from website
    if (!any(file.exists(download_names))) {
      download.file(file_urls, download_names, method = "libcurl")
    }
  }
  # nocov end
}


