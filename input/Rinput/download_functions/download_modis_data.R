################################################################################
# Date created: 2023-10-12
# Date modified: 2023-12-01
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
#'  Directory structure looks like
#'  input/modis/raw/[version]/[product]/[year]/[day_of_year]
#' download_modis_data:
#' @description
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param product character(1). One of c("MOD09GA", "MOD11A1", "MOD06_L2",
#'      "MCD19A2", "MOD13A2", "VNP46A2").
#' @param version character(1). Default is "61", meaning v061.
#' @param horizontal_tiles integer(2). Horizontal tile numbers
#' c([start], [end]). Default is c(7, 13).
#' @param vertical_tiles integer(2). Vertical tile numbers
#' c([start], [end]). Default is c(3, 6).
#' @param nasa_earth_data_token character(1).
#'  Token for downloading data from NASA. Should be set before
#'  trying running the function.
#' @param directory_to_save character(1). Directory to save data.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @param download logical(1). Download data or only save wget commands.
#' @param remove_command logical(1). Remove (\code{TRUE}) or keep (\code{FALSE})
#' the text file containing download commands.
#' @author Mitchell Manware, Insang Song
#' @import rvest
#' @returns NULL;
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
    download = TRUE,
    remove_command = FALSE) {
  #### 1. check for data download acknowledgement
  download_permit(data_download_acknowledgement = data_download_acknowledgement)
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 2. directory setup
  download_setup_dir(directory_to_save)
  directory_to_save <- download_sanitize_path(directory_to_save)
  #### 4. check for product
  product <- match.arg(product)
  ismod13 <- startsWith(product, "MOD13")
  ismod06 <- startsWith(product, "MOD06")
  #### 5. value years
  if (substr(date_start, 1, 4) != substr(date_end, 1, 4)) {
    stop("date_start and date_end should be in the same year.\n")
  }
  #### 6. check for valid horizontal tiles
  if (!all(horizontal_tiles %in% seq(0, 35))) {
    stop("Horizontal tiles are not in the proper range [0, 35].\n")
  }
  if (!all(vertical_tiles %in% seq(0, 17))) {
    stop("Vertical tiles are not in the proper range [0, 17].\n")
  }
  #### 8. Reuse ladsweb home url
  ladsurl <- "https://ladsweb.modaps.eosdis.nasa.gov/"
  version <- ifelse(startsWith(product, "VNP"), "5000", version)
  #### 9. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  # if (ismod13) {
  #   date_start_yearday <- as.numeric(strftime(date_start_date_format, "%j"))
  #   date_end_yearday <- as.numeric(strftime(date_end_date_format, "%j"))
  #   year_mod13 <- strftime(date_start_date_format, "%Y")
  #   date_sequence <- seq(1, 366, 16)
  #   date_sequence <-
  #     date_sequence[date_sequence >= date_start_yearday &
  #       date_sequence <= date_end_yearday]
  # }

  # In a certain year, list all available dates
  year <- as.character(substr(date_start, 1, 4))
  # year <- ifelse(ismod13, year_mod13, as.character(substr(date_start, 1, 4)))
  filedir_year_url <-
    paste0(
          ladsurl,
          "archive/allData/",
          version,
          "/",
          product,
          "/",
          year)

  list_available_d <-
    rvest::read_html(filedir_year_url) |>
    rvest::html_elements("tr") |>
    rvest::html_attr("data-name")
  # no conditional assignment at this moment.

  # remove NAs
  # Queried year's available days
  date_sequence <- list_available_d[!is.na(list_available_d)]
  date_sequence_i <- as.integer(date_sequence)
  # Queried dates to integer range
  date_start_i <- as.integer(strftime(date_start, "%j"))
  date_end_i <- as.integer(strftime(date_end, "%j"))
  date_range_julian <- seq(date_start_i, date_end_i)
  date_sequence_in <- (date_sequence_i %in% date_range_julian)

  message(sprintf("%d / %d days of data available in the queried dates.\n",
                  sum(date_sequence_in), length(date_range_julian)))
  date_sequence <- date_sequence[date_sequence_in]


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
    "_",
    date_start,
    "_",
    date_end,
    "_wget_commands.txt"
  )

  # avoid any possible errors by removing existing command files
  download_sink(commands_txt)
  #### 14. append download commands to text file
  for (d in seq_along(date_sequence)) {
    day <- date_sequence[d]
    filedir_url <-
      paste0(
             filedir_year_url,
             "/",
             day)

    filelist <-
      rvest::read_html(filedir_url) |>
      rvest::html_elements("tr") |>
      rvest::html_attr("data-path")

    filelist_sub <-
      grep(
           paste("(", paste(tiles_requested, collapse = "|"), ")"),
           filelist, value = TRUE)
    if (ismod06) {
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

  if (download) {
    #### 17. build system command
    system_command <- paste0(
      ". ",
      commands_txt,
      "\n"
    )
    #### 18. download data
    download_run(download = download,
                 system_command = system_command,
                 commands_txt = commands_txt)
  }

  message("Requests were processed.\n")

}
