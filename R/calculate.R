# nocov start


#' Process atmospheric composition data by chunks
#' @keywords Calculation
#' @description
#' Returning a single `SpatRasterDataset` object.
#' Removed `tapp` for performance; impose a strict assumption that
#' there are no missing values
#' @param path character(1). Directory with downloaded netCDF (.nc4) files. or
#' netCDF file paths.
#' @param date character(2). length of 10. Format "YYYY-MM-DD".
#' @param locs Locations to extract.
#' @param locs_id character(1). Location identifier.
#' @param ... Arguments passed to [`terra::rast`].
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable,
#' pressure level, date
#' Reference duration: 1 day summary, all layers: 106 seconds
#' hard-coded subsets for subdataset selection
#' @author Mitchell Manware, Insang Song
#' @return a `SpatRaster` object;
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra crs
#' @importFrom terra subset
#' @importFrom sf st_as_sf
#' @importFrom future.apply future_lapply
#' @importFrom data.table rbindlist
#' @export
calc_geos_strict <-
  function(path = NULL,
           date = c("2018-01-01", "2018-01-01"),
           locs = NULL,
           locs_id = NULL,
           ...) {
    #### directory setup
    if (length(path) == 1) {
      if (dir.exists(path)) {
        # path <- amadeus::download_sanitize_path(path)
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
    # amadeus::check_for_null_parameters(mget(ls()))
    #### identify file paths
    #### identify dates based on user input
    dates_of_interest <- amadeus::generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = TRUE
    )
    dates_of_interest_incl <- amadeus::generate_date_sequence(
      date[1],
      date[2],
      sub_hyphen = FALSE
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
    collection <- regmatches(
      data_paths[1],
      # the pattern accommodates 3-4 characters for the variable name,
      # 3-4 alphanumerics for the temporal resolution,
      # 8-9 alphanumerics for the output dimensions
      # nolint start
      regexpr(
        "GEOS-CF.v01.rpl.(aqc|chm)_[[:alpha:]]{3,4}_[[:alnum:]]{3,4}_[[:alnum:]]{8,9}_v[1-9]",
        data_paths[1]
      )
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

    filename_date <- sort(regmatches(
      data_paths,
      regexpr(
        "20[0-9]{2}(0[1-9]|1[0-2])([0-2][0-9]|3[0-1])",
        data_paths
      )
    ))
    if (any(table(filename_date) < 24)) {
      warning(
        "Some dates include less than 24 hours. Check the downloaded files."
      )
    }
    # nolint end
    # to export locs (pointers are not exportable)
    locs <- sf::st_as_sf(locs)

    # split filename dates daily
    filename_date <- as.Date(filename_date, format = "%Y%m%d")
    filename_date <- filename_date[filename_date %in% dates_of_interest_incl]
    filename_date_cl <- as.integer(as.factor(filename_date))

    future_inserted <- split(data_paths, filename_date_cl)
    other_args <- list(...)
    other_args$nthreads <- NULL
    data_variables <- terra::describe(data_paths[1], sds = TRUE)$var

    search_variables <-
      if (grepl("chm", collection)) {
        c("ACET", "ALD2", "ALK4", "BCPI", "BCPO", "BENZ", "C2H6", "C3H8",
          "CH4", "CO", "DST1", "DST2", "DST3", "DST4", "EOH", "H2O2",
          "HCHO", "HNO3", "HNO4", "ISOP", "MACR", "MEK", "MVK", "N2O5",
          "NH3", "NH4", "NIT", "NO", "NO2", "NOy", "OCPI", "OCPO", "PAN",
          "PM25_RH35_GCC", "PM25_RH35_GOCART", "PM25bc_RH35_GCC",
          "PM25du_RH35_GCC", "PM25ni_RH35_GCC", "PM25oc_RH35_GCC",
          "PM25soa_RH35_GCC", "PM25ss_RH35_GCC", "PM25su_RH35_GCC",
          "PRPE", "RCHO", "SALA", "SALC", "SO2", "SOAP", "SOAS", "TOLU", "XYLE"
        )
      } else {
        c("CO", "NO2", "O3", "SO2")
      }

    # fs is the hourly file paths per day (each element with N=24)
    summary_byvar <- function(x = search_variables, fs) {
      rast_in <- rlang::inject(terra::rast(fs, !!!other_args))
      # strongly assume that we take the single day. no need to filter dates
      # per variable,
      # all files (hourly) are cleaned and processed
      sds_proc <-
        lapply(
          x,
          function(v) {
            rast_inidx <- grep(v, data_variables)
            #rast_in <- mean(rast_in[[rast_inidx]])
            rast_summary <- terra::mean(rast_in[[rast_inidx]])
            rtin <- as.Date(terra::time(rast_in))
            rtin_u <- unique(rtin)
            cat(sprintf("Processing %s, date: %s\n", v, rtin_u))
            # rast_summary <- vector("list", length = length(rtin_u))
            # for (d in seq_along(rtin_u)) {
            #   rast_d <- rast_in[[rtin == rtin_u[d]]]
            #   rast_summary[[d]] <- mean(rast_d)
            # }
            # rast_summary <- do.call(c, rast_summary)

            # the next line is deprecated
            # rast_summary <- terra::tapp(rast_in, index = "days", fun = "mean")
            terra::time(rast_summary) <- rtin_u
            names(rast_summary) <-
              paste0(
                rep(gsub("_lev=.*", "", v), terra::nlyr(rast_summary))
              )
            terra::set.crs(rast_summary, "EPSG:4326")
            return(rast_summary)
          }
        )
      sds_proc <- terra::sds(sds_proc)

      locstr <- terra::vect(locs)
      rast_ext <- terra::extract(sds_proc, locstr, ID = TRUE)
      # rast_ext <- lapply(rast_ext,
      #   function(df) {
      #     df$ID <- unlist(locs[[locs_id]])
      #     return(df)
      #   }
      # )
      rast_ext <-
        Reduce(function(dfa, dfb) dplyr::full_join(dfa, dfb, by = "ID"),
          rast_ext
        )
      rast_ext$time <- unique(as.Date(terra::time(rast_in)))
      rast_ext$ID <- unlist(locs[[locs_id]])[rast_ext$ID]
      names(rast_ext)[names(rast_ext) == "ID"] <- locs_id
      return(rast_ext)

    }
    future::plan(future::multicore, workers = 10)
    rast_summary <-
      future.apply::future_lapply(
        future_inserted,
        function(fs) summary_byvar(fs = fs)
      )
    future::plan(future::sequential)
    rast_summary <- data.table::rbindlist(rast_summary)

    return(rast_summary)

  }


#' Reflown gmted processing
#' @keywords Calculation
#' @param variable character(2). Statistic and resolution.
#' @param path character(1). Directory with downloaded GMTED files.
#' @param locs data.frame/SpatVector/sf. Locations.
#' @param locs_id character(1). Location identifier.
#' @param win numeric(4). Window for the raster.
#' @param radius numeric(1). Radius for the extraction.
#' @param fun character(1). Function to apply.
#' @param ... Additional parameters to be passed to other functions.
#' @returns A data.frame containing the extracted GMTED data.
#' @importFrom terra rast
#' @importFrom terra varnames
#' @importFrom terra extract
#' @export
calc_gmted_direct <- function(
    variable = NULL,
    path = NULL,
    locs = NULL,
    locs_id = NULL,
    win = c(-126, -62, 22, 52),
    radius = 0,
    fun = "mean",
    ...) {
  #### directory setup
  path <- amadeus::download_sanitize_path(path)
  #### check for length of variable
  if (!(length(variable) == 2)) {
    stop(
      paste0(
        "Please provide a vector with the statistic and resolution.\n"
      )
    )
  }
  #### identify statistic and resolution
  statistic <- variable[1]
  statistic_code <- amadeus::process_gmted_codes(
    statistic,
    statistic = TRUE,
    invert = FALSE
  )
  resolution <- variable[2]
  resolution_code <- amadeus::process_gmted_codes(
    resolution,
    resolution = TRUE,
    invert = FALSE
  )
  cat(paste0(
    "Cleaning ",
    statistic,
    " data at ",
    resolution,
    " resolution.\n"
  ))
  statistic_from <- c(
    "Breakline Emphasis", "Systematic Subsample",
    "Median Statistic", "Minimum Statistic",
    "Mean Statistic", "Maximum Statistic",
    "Standard Deviation Statistic"
  )
  statistic_to <- c(
    "BRKL", "SSUB", "MEDN", "MINI", "MEAN", "MAXL", "STDV"
  )
  statistic_to <-
    sprintf("LDU_E%s", statistic_to[match(statistic, statistic_from)])

  #### identify file path
  paths <- list.dirs(
    path,
    full.names = TRUE
  )
  data_path <-
    grep(
      sprintf(
        "%s%s_grd",
        statistic_code,
        as.character(resolution_code)
      ),
      paths, value = TRUE
    )

  #### import data
  data <- terra::rast(data_path, win = win)
  #### layer name
  names(data) <- paste0(
    "elevation_",
    gsub(
      "_grd",
      "",
      names(data)
    )
  )
  #### varnames
  terra::varnames(data) <- paste0(
    "Elevation: ",
    statistic,
    " (",
    resolution,
    ")"
  )
  from <- data
  #return(from)
  #### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs,
    locs_id = locs_id,
    radius = radius
  )
  sites_e <- sites_list[[1]]
  sites_id <- sites_list[[2]]
  #### perform extraction
  sites_extracted <- amadeus::calc_worker(
    dataset = "gmted",
    from = from,
    locs_vector = sites_e,
    locs_df = sites_id,
    radius = radius,
    fun = fun,
    variable = 2,
    time = NULL,
    time_type = "timeless"
  )
  #### convert integer to numeric
  sites_extracted[, 2] <- as.numeric(sites_extracted[, 2])
  #### define column names
  colnames(sites_extracted) <- c(
    locs_id,
    paste0(
      statistic_to, "_", sprintf("%05d", radius)
    )
  )
  #### return data.frame
  return(data.frame(sites_extracted))
}



#' Calculate aggregated values for specified locations
#'
#' This function calculates aggregated values for specified locations from
#'  a raster dataset.
#'
#' @keywords Calculation
#' @param from The raster dataset from which to extract values.
#' @param locs A data frame containing the locations for which
#'  to calculate aggregated values.
#'  It should have a column in `locs_id` value
#'  that contains unique identifiers for each location.
#' @param locs_id An optional column name
#'  in the \code{locs} data frame that contains additional location
#'  identifiers.
#' @param radius The radius within which to include neighboring locations
#'  for aggregation. Default is 0.
#' @param fun The aggregation function to use.
#'  It can be a character string specifying a function name
#' (e.g., "mean", "sum"),
#' or it can be a custom function. Default is "mean".
#' @param ... Additional arguments to be passed to
#'  the aggregation function.
#'
#' @returns A data frame containing the aggregated values for each
#'  location and time point.
#' @export
calc_narr2 <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  ...
) {
  #
  name <- geometry <- value <- NULL
  ### prepare locations list
  sites_list <- amadeus::calc_prepare_locs(
    from = from,
    locs = locs[, "site_id"],
    locs_id = locs_id,
    radius = radius
  )
  sites_e <- sites_list[[1]]
  # sites_id <- sites_list[[2]]
  #### identify pressure level or monolevel data
  time_from <- terra::time(from)
  timetab <- table(time_from)
  if (!all(timetab == 1)) {
    time_split <-
      split(time_from,
            #ceiling(seq_along(time_from) / 29L))
            ceiling(as.integer(as.factor(time_from)) / 14L))
    sites_extracted <- Map(
      function(day) {
        cat(sprintf("Processing %s...\n", paste(day[1], "-", day[length(day)])))
        from_day <- from[[time_from %in% day]]
        sites_extracted_day <- terra::extract(
          from_day,
          sites_e,
          bind = TRUE
        )
        sites_extracted_day <- data.frame(sites_extracted_day)
        if ("geometry" %in% names(sites_extracted_day)) {
          sites_extracted_day <- sites_extracted_day |>
            dplyr::select(-geometry)
        }
        return(sites_extracted_day)
      },
      time_split
    )
    sites_extracted <- reduce_merge(sites_extracted, by = c("site_id"))
  } else {
    sites_extracted <-
      terra::extract(
        from,
        sites_e,
        bind = TRUE
      )
    sites_extracted <- as.data.frame(sites_extracted)
    if ("geometry" %in% names(sites_extracted)) {
      sites_extracted <- sites_extracted |>
        dplyr::select(-geometry)
    }
  }
  sites_extracted <-
    sites_extracted |>
    tidyr::pivot_longer(cols = tidyselect::starts_with("MET_")) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      time =
      regmatches(
        name,
        regexpr(
          "20[0-9]{2,2}[0-1][0-9][0-3][0-9]",
          name
        )
      )
    ) |>
    dplyr::mutate(
      name = sub(paste0("_", time), "", name)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      time = as.character(as.Date(time, format = "%Y%m%d"))
    ) |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = value,
      id_cols = c("site_id", "time")
    )
  sites_extracted <- data.table::as.data.table(sites_extracted)
  names(sites_extracted)[-1:-2] <-
    sprintf("%s_%05d", names(sites_extracted)[-1:-2], radius)

  #### return data.frame
  return(sites_extracted)
}




#' Parallelize NARR feature calculation
#'
#' This function parallelizes the processing and calculation of
#'  NARR data for multiple domains.
#' @keywords Calculation
#' @param domain A character vector specifying the domains to process.
#' @param date A character vector specifying the date of the
#'  NARR data to process.
#' @param locs A data frame specifying the locations to calculate NARR data for.
#' @param nthreads An integer specifying the number of threads
#'  to use for parallel processing. Default is 24.
#'
#' @returns A list of results from the parallel processing.
#' @importFrom future plan multicore sequential
#' @importFrom future.apply future_lapply
#' @export
par_narr <- function(domain, date, locs, nthreads = 24L) {

  future::plan(future::multicore, workers = nthreads)

  res <-
    future.apply::future_lapply(
      domain,
      function(x) {
        from <- process_narr2(
          path = "input/narr",
          variable = x,
          date = date
        )
        calc_narr2(
          from = from,
          locs = locs,
          locs_id = "site_id"
        )
      },
      future.seed = TRUE
    )
  future::plan(future::sequential)
  return(res)

}

# nocov end