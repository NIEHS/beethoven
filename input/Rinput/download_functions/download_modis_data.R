################################################################################
# Date created: 2023-10-12
# Date modified: 2023-11-22
################################################################################

################################################################################
#' download_modis_data:
#' @description Need maintenance for the directory path change
#' in NASA EOSDIS. This function first retrieves the all hdf download links
#' on a certain day, then only selects the relevant tiles from the retrieved
#' links. Download is only done at the queried horizontal-vertical tile number
#' combinations.
#' @param date_start character(1). length of 10. Start date for downloading
#' data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for downloading data.
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param product character(1).
#' @param version character(1).
#' @param horizontal_tiles integer(2).
#' @param vertical_tiles integer(2).
#' @param nasa_earth_data_token character(1).
#'  Token for downloading data from NASA.
#' @param directory_to_save character(1). Directory to save data.
#' @param data_download_acknowledgement logical(1). By setting `= TRUE` the
#' user acknowledge that the data downloaded using this function may be very
#' large and use lots of machine storage and memory.
#' @author Mitchell Manware, Insang Song
#' @import rvest
#' @return NULL;
#' @export
download_modis_data <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    product = NULL,
    version = "61",
    horizontal_tiles = c(7, 13),
    vertical_tiles = c(3, 6),
    nasa_earth_data_token = NULL,
    directory_to_save = "./input/modis/raw/",
    data_download_acknowledgement = FALSE) {
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
  if (is.null(product) == TRUE) {
    stop("Please select a MODIS product.\n")
  }
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

  #### 9. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")

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

  #### 13. download
  for (d in seq_along(date_sequence)) {
    date <- date_sequence[d]
    year <- as.character(substr(date, 1, 4))
    day <- strftime(date, "%j")
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

    filelist <- grep(paste("(", paste(tiles_requested, collapse = "|"), ")"),
        filelist, value = TRUE)
    download_url <- sprintf("%s%s", ladsurl, filelist)
    # download_url <- paste0(
    #   "https://ladsweb.modaps.eosdis.nasa.gov/",
    #   "archive/allData/",
    #   version,
    #   "/",
    #   product,
    #   "/",
    #   year,
    #   "/",
    #   day,
    #   "/"
    # )
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
    cat(download_command)
  }

  

  #### 14. initiate "..._wget_commands.txt" file
  commands_txt <- paste0(
    directory_to_save,
    product,
    "_wget_commands.txt"
  )
  sink(commands_txt)

  #### 15. concatenate and print download commands to "..._wget_commands.txt"

  #### 16. finish "..._wget_commands.txt"
  sink()
  #### 17. build system command
  system_command <- paste0(
    ". ",
    commands_txt,
    "\n"
  )
  #### 18. download data
  system(command = system_command)
  #### xx. remove "..._wget_commands.txt"
  # file.remove(commands_txt)

  # tiles_requested <- as.vector(NULL)
  # for (t in seq_along(tiles_horizontal)) {
  #   tile <- paste0(
  #     tiles_horizontal[t],
  #     tiles_vertical
  #   )
  #   tiles_requested <- c(tiles_requested, tile)
  # }
  #### xx. remove data outside of requested tiles
  # for (s in seq_along(date_sequence)) {
  #   date <- date_sequence[s]
  #   year <- as.character(substr(date, 1, 4))
  #   day <- strftime(date, "%j")
  #   directory_with_data <- paste0(
  #     directory_to_save,
  #     product,
  #     "/",
  #     year,
  #     "/",
  #     day,
  #     "/"
  #   )
  #   data_paths <- list.files(
  #     path = directory_with_data,
  #     full.names = TRUE
  #   )
  #   path_splitter <- paste0(
  #     "A",
  #     year,
  #     day
  #   )
  #   for (p in seq_along(data_paths)) {
  #     path_tiles <- as.data.frame(strsplit(data_paths[p], path_splitter))[2, ]
  #     path_tiles <- substr(path_tiles, 2, 7)
  #     if (!(path_tiles %in% tiles_requested)) {
  #       file.remove(data_paths[p])
  #     }
  #   }
  # }
}
