# nocov start
# not used in the pipeline, but good for educational/development purposes


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




#' Search package functions
#' @keywords Utility
#' @param package character(1). Package name.
#' @param search character(1). Search term.
#' @returns A character vector containing the matching function names.
#' @examples
#' # Search for functions in the `amadeus` package
#' \dontrun{
#' search_function("amadeus", "process_")
#' }
search_function <- function(package, search) {
  library(package, character.only = TRUE)
  grep(search, ls(sprintf("package:%s", package)), value = TRUE)
}

#' Get data.frame of function parameters
#' @keywords Utility
#' @param functions character. Vector of function names.
#' @returns A data.frame containing the parameters of the functions.
#' @importFrom dplyr as_tibble bind_rows
df_params <- function(functions) {
  params <- lapply(functions, function(x) {
    args <-
      dplyr::as_tibble(
        lapply(as.list(formals(get(x))), \(p) list(p)),
        .name_repair = "minimal"
      )
    return(args)
  })
  paramsdf <- Reduce(dplyr::bind_rows, params)
  return(paramsdf)
}


# nocov end
