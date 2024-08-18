# nocov start
# please note that functions here are modified version of
# the original functions in the package amadeus (<0.2.0)


#' Process atmospheric composition data by chunks (v2)
#' @keywords Calculation
#' @description
#' Returning a single `SpatRasterDataset` object.
#' @param date character(2). length of 10. Format "YYYY-MM-DD".
#' @param path character(1). Directory with downloaded netCDF (.nc4) files. or
#' netCDF file paths.
#' @param ... Arguments passed to [`terra::rast`].
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable,
#' pressure level, date
#' Reference duration: 1 day summary, all layers: 115 seconds
#' Superseded by [`calc_geos_strict`].
#' @author Mitchell Manware, Insang Song
#' @return a `SpatRaster` object;
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra crs
#' @importFrom terra subset
#' @export
process_geos_bulk <-
  function(path = NULL,
           date = c("2018-01-01", "2018-01-01"),
           ...) {
    #### directory setup
    if (length(path) == 1) {

      if (dir.exists(path)) {
        path <- amadeus::download_sanitize_path(path)
        paths <- list.files(
          path,
          pattern = "GEOS-CF.v01.rpl",
          full.names = TRUE
        )
        paths <- paths[grep(
          ".nc4",
          paths
        )]
      }
    } else {
      paths <- path
    }
    #### check for variable
    amadeus::check_for_null_parameters(mget(ls()))
    #### identify file paths
    #### identify dates based on user input
    dates_of_interest <- amadeus::generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = TRUE
    )
    #### subset file paths to only dates of interest
    data_paths <- unique(
      grep(
        paste(
          dates_of_interest,
          collapse = "|"
        ),
        paths,
        value = TRUE
      )
    )
    #### identify collection
    collection <- amadeus::process_collection(
      data_paths[1],
      source = "geos",
      collection = TRUE
    )
    cat(
      paste0(
        "Identified collection ",
        collection,
        ".\n"
      )
    )
    if (length(unique(collection)) > 1) {
      warning(
        "Multiple collections detected. Returning data for all collections.\n"
      )
    }

    filename_date <- regmatches(
      data_paths,
      regexpr(
        "20[0-9]{2}(0[1-9]|1[0-2])([0-2][0-9]|3[0-1])",
        data_paths
      )
    )
    if (any(table(filename_date) < 24)) {
      warning(
        "Some dates include less than 24 hours. Check the downloaded files."
      )
    }
    if (length(unique(filename_date)) > 10) {
      message(
        "More than 10 unique dates detected. Try 10-day chunks..."
      )
    }

    # split filename date every 10 days
    filename_date <- as.Date(filename_date, format = "%Y%m%d")
    filename_date_cl <- as.integer(cut(filename_date, "30 days"))

    future_inserted <- split(data_paths, filename_date_cl)
    other_args <- list(...)
    data_variables <- names(terra::rast(data_paths[1]))
    # nolint start
    summary_byvar <- function(x = data_variables, fs) {
      rast_in <- rlang::inject(terra::rast(fs, !!!other_args))
      terra::sds(lapply(
        x,
        function(v) {
          rast_inidx <- grep(v, names(rast_in))
          rast_in <- rast_in[[rast_inidx]]
          rast_summary <- terra::tapp(rast_in, index = "days", fun = "mean")
          names(rast_summary) <-
            paste0(
              rep(v, terra::nlyr(rast_summary)), "_",
              terra::time(rast_summary)
            )
          terra::set.crs(rast_summary, "EPSG:4326")
          return(rast_summary)
        }
      ))
    }
    # nolint end

    # summary by 10 days
    # TODO: dropping furrr?
    rast_10d_summary <-
      furrr::future_map(
        .x = future_inserted,
        .f = ~summary_byvar(fs = .x),
        .options =
        furrr::furrr_options(
          globals = c("other_args", "data_variables")
        )
      )
    rast_10d_summary <- Reduce(c, rast_10d_summary)
    return(rast_10d_summary)

  }




#' Process NARR Data (v2)
#'
#' This function processes NARR2 data based on the specified parameters.
#'
#' @keywords Calculation
#' @param date A character vector specifying the start and end dates
#'   in the format "YYYY-MM-DD".
#' @param variable A character vector specifying the variable of interest.
#' @param path A character vector specifying the path to the data files.
#' @param ... Additional parameters to be passed to other functions.
#'
#' @return A SpatRaster object containing the processed NARR2 data.
#'
#' @details This function performs the following steps:
#'   1. Sets up the directory path.
#'   2. Checks for null parameters.
#'   3. Identifies file paths based on the specified variable.
#'   4. Generates a date sequence based on the specified start and end dates.
#'   5. Filters the file paths to include only dates of interest.
#'   6. Sets up the search abbreviation and target variable.
#'   7. Imports and processes the data for each file path.
#'   8. Subsets the data to include only dates of interest.
#'   9. Returns the processed data as a SpatRaster object.
#'
#' @examples
#' # Process NARR2 data for the variable "PRATE" from
#' # September 1, 2023 to September 1, 2023
#' \dontrun{
#' data <-
#'   process_narr2(
#'     date = c("2023-09-01", "2023-09-01"),
#'     variable = "PRATE",
#'     path = "/path/to/data"
#'   )
#' }
#'
#' @export
process_narr2 <- function(
    date = c("2023-09-01", "2023-09-01"),
    variable = NULL,
    path = NULL,
    ...) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check for variable
  amadeus::check_for_null_parameters(mget(ls()))
  #### identify file paths
  data_paths <- list.files(
    path,
    pattern = variable,
    recursive = TRUE,
    full.names = TRUE
  )
  # data_paths <- grep(
  #   sprintf("%s*.*.nc", variable),
  #   data_paths,
  #   value = TRUE
  # )
  #### define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### path ncar
  ym_from <- regmatches(
    data_paths,
    regexpr(
      "2[0-9]{3,5}",
      data_paths
    )
  )
  ym_of_interest <-
    substr(date_sequence,
           1, ifelse(all(nchar(ym_from) == 6), 6, 4))
  ym_of_interest <- unique(ym_of_interest)
  #### subset file paths to only dates of interest
  data_paths_ym <- unique(
    grep(
      paste(
        ym_of_interest,
        collapse = "|"
      ),
      data_paths,
      value = TRUE
    )
  )

  search_abbr <- list.dirs(path)[-1]
  search_abbr <- sub(paste0(path, "/"), "", search_abbr)
  search_to <- c(
    "ATSFC", "ALBDO", "ACPRC", "DSWRF", "ACEVP",
    "HCLAF", "PLBLH", "LCLAF", "LATHF", "MCLAF",
    "OMEGA", "PRWTR", "PRATE", "PRSFC", "SENHF",
    "SPHUM", "SNWCV", "SLMSC", "CLDCV", "ULWRF",
    "UWIND", "VISIB", "VWIND", "ACSNW"
  )
  search_to <-
    sprintf("MET_%s", search_to[match(variable, search_abbr)])

  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths_ym)) {
    #### import data
    data_year <- terra::rast(data_paths_ym[p])
    data_year_tinfo <- terra::time(data_year)
    time_processed <- as.POSIXlt(data_year_tinfo)
    time_this <- time_processed[1]
    cat(paste0(
      "Cleaning ", variable, " data for ",
      sprintf(
        "%s, %d %s",
        strftime(time_this, "%B"),
        time_this$year + 1900,
        "...\n"
      )
    ))
    #### check for mono or pressure levels
    lvinfo <- regmatches(
      names(data_year),
      regexpr("level=[0-9]{3,4}", names(data_year))
    )
    if (length(lvinfo) == 0) {
      cat("Detected monolevel data...\n")
      names(data_year) <- paste0(
        search_to, "_",
        gsub("-", "", data_year_tinfo)
      )
    } else {
      cat("Detected pressure levels data...\n")
      lvinfo <- sub("level=", "", lvinfo)
      lvinfo <- sprintf("%04d", as.integer(lvinfo))
      lvinfo <- paste0("L", lvinfo)
      terra::time(data_year) <- as.Date(data_year_tinfo)
      names(data_year) <- sprintf(
        "%s_%s_%s",
        search_to,
        lvinfo,
        gsub("-", "", data_year_tinfo)
      )
    }
    data_full <- c(
      data_full,
      data_year,
      warn = FALSE
    )
  }

  #### subset years to dates of interest
  data_full_cn <- names(data_full)
  data_return <- terra::subset(
    data_full,
    which(
      substr(
        data_full_cn,
        nchar(data_full_cn) - 7,
        nchar(data_full_cn)
      ) %in% date_sequence
    )
  )
  cat(paste0(
    "Returning daily ",
    variable,
    " data from ",
    as.Date(date_sequence[1], format = "%Y%m%d"),
    " to ",
    as.Date(
      date_sequence[length(date_sequence)],
      format = "%Y%m%d"
    ),
    ".\n"
  ))
  #### return SpatRaster
  return(data_return)
}



# nocov end
