################################################################################
# Date created: 2023-10-12
# Date modified: 2023-11-24
################################################################################

################################################################################
#' download_modis_data:
#' @description Need maintenance for the directory path change
#' in NASA EOSDIS. This function first retrieves the all hdf download links
#' on a certain day, then only selects the relevant tiles from the retrieved
#' links. Download is only done at the queried horizontal-vertical tile number
#' combinations. An exception is MOD06_L2 product, which is produced
#' every five minutes every day.
#' @note \code{date_start} and \code{date_end} should be in the same year.
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param product character(1). One of c("MOD09GA", "MOD11A1", "MOD06_L2",
#'      "MCD19A2", "MOD13A2", "VNP46A2").
#' @param version character(1). Default is "61", meaning v061.
#' @param horizontal_tiles integer(2).
#' @param vertical_tiles integer(2).
#' @param nasa_earth_data_token character(1).
#'  Token for downloading data from NASA.
#' @param directory_to_save character(1). Directory to save data.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param write_command logical(1). Only return a text file
#' with wget commands instead of running it
#' @author Mitchell Manware, Insang Song
#' @import rvest
#' @return NULL;
#' @export
download_modis_data <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    product = c("MOD09GA", "MOD11A1", "MOD06_L2",
      "MCD19A2", "MOD13A2", "VNP46A2"),
    version = "61",
    horizontal_tiles = c(7, 13),
    vertical_tiles = c(3, 6),
    nasa_earth_data_token = NULL,
    directory_to_save = "./input/modis/raw/",
    data_download_acknowledgement = FALSE,
    write_command = FALSE) {
  #### 1. directory setup
  chars_dir_save <- nchar(directory_to_save)
  if (substr(directory_to_save, chars_dir_save, chars_dir_save) != "/") {
    directory_to_save <- paste0(directory_to_save, "/", sep = "")
  }
  #### 2. check for data download acknowledgement
  if (data_download_acknowledgement == FALSE) {
    stop(paste0(
      "Data download acknowledgement is set to FALSE. ",
      "Please acknowledge that the data downloaded using this ",
      "function may be very large and use lots of machine storage ",
      "and memory.\n"))
  }
  #### 3. check for NASA earth data token
  if (is.null(nasa_earth_data_token)) {
    stop("Please provide NASA EarthData Login token.\n")
  }
  #### 4. check for product
  product <- match.arg(product)
  ismod13 <- startsWith(product, "MOD13")
  # if (is.null(product) == TRUE) {
  #   stop("Please select a MODIS product.\n")
  # }
  #### 5. check for version
  if (is.null(version) == TRUE) {
    stop("Please select a data version.\n")
  }
  #### 6. check for valid horizontal tiles
  for (h in seq_along(horizontal_tiles)) {
    if (horizontal_tiles[h] < 0 || horizontal_tiles[h] > 35) {
      cat(paste0("Horizontal tiles invalid.\n"))
      stop()
    }
  }
  #### 7. check for valid vertical tiles
  for (v in seq_along(vertical_tiles)) {
    if (vertical_tiles[v] < 0 || vertical_tiles[v] > 17) {
      cat(paste0("Vertical tiles invalid.\n"))
      stop()
    }
  }

  #### 8. Reuse ladsweb home url
  ladsurl <- "https://ladsweb.modaps.eosdis.nasa.gov/"
  version <- ifelse(startsWith(product, "VNP"), "5000", version)

  #### 9. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  if (ismod13) {
    date_start_yearday <- as.numeric(strftime(date_start_date_format, "%j"))
    date_end_yearday <- as.numeric(strftime(date_end_date_format, "%j"))
    date_sequence <- seq(1, 366, 16)
    date_sequence <-
      date_sequence[date_sequence >= date_start_yearday &
        date_sequence <= date_end_yearday]
  }


  #### 10. define horizontal tiles
  tiles_horizontal <- seq(horizontal_tiles[1],
    horizontal_tiles[2],
    by = 1
  )
  tiles_horizontal <- paste0(
    "h",
    sprintf("%02d", tiles_horizontal)
  )

  #### 11. define vertical tiles
  tiles_vertical <- seq(vertical_tiles[1],
    vertical_tiles[2],
    by = 1
  )
  tiles_vertical <- paste0(
    "v",
    sprintf("%02d", tiles_vertical)
  )
  #### 12. define requested tiles
  tiles_df <- expand.grid(
    h = tiles_horizontal,
    v = tiles_vertical
  )
  tiles_requested <-
    paste0(tiles_df$h, tiles_df$v)


  #### 13. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(
    directory_to_save,
    product,
    date_start,
    "_",
    date_end,
    "_",
    product,
    "_wget_commands.txt"
  )
  sink(commands_txt)

  #### 14. append download commands to text file
  for (d in seq_along(date_sequence)) {
    date <- date_sequence[d]
    year <- as.character(substr(date, 1, 4))
    day <- ifelse(ismod13, sprintf("%03d", d), strftime(date, "%j"))
    filedir_url <- paste0(
        ladsurl,
        "archive/allData/",
        version,
        "/",
        product,
        "/",
        year,
        "/",
        day)

    filelist <-
      rvest::read_html(filedir_url) |>
      rvest::html_elements("tr") |>
      rvest::html_attr("data-path")

    filelist_sub <- grep(paste("(", paste(tiles_requested, collapse = "|"), ")"),
        filelist, value = TRUE)
    if (ismod13) {
      filelist_sub <- filelist
    }
    download_url <- sprintf("%s%s", ladsurl, filelist_sub)

    # Main wget run
    download_command <- paste0(
      "wget -e robots=off -m -np -R .html,.tmp ",
      "-nH --cut-dirs=3 \"",
      download_url,
      "\" --header \"Authorization: Bearer ",
      nasa_earth_data_token,
      "\" -P ",
      directory_to_save,
      "\n"
    )
    #### 15. concatenate and print download commands to "..._wget_commands.txt"
    cat(download_command)
  }


  #### 16. finish "..._wget_commands.txt"
  sink(file = NULL)

  if (!write_command) {
    #### 17. build system command
    system_command <- paste0(
      ". ",
      commands_txt,
      "\n"
    )
    #### 18. download data
    system(command = system_command)
  }

  message("Requests were processed.\n")

}
