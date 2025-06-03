
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
#' @importFrom sf st_as_sf
#' @importFrom data.table rbindlist
#' @importFrom amadeus generate_date_sequence
#' @importFrom terra describe rast time subset crs varnames vect sds extract
#' @importFrom terra nlyr set.crs
#' @importFrom dplyr full_join
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
          full.names = TRUE,
          recursive = TRUE
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
      data_paths,
      # the pattern accommodates 3-4 characters for the variable name,
      # 3-4 alphanumerics for the temporal resolution,
      # 8-9 alphanumerics for the output dimensions
      # nolint start
      regexpr(
        "GEOS-CF.v01.rpl.(aqc|chm)_[[:alpha:]]{3,4}_[[:alnum:]]{3,4}_[[:alnum:]]{8,9}_v[1-9]",
        data_paths # mm-tests-0816 search all data paths for > 1 collection
      )
    )
    collection <- unique(collection)
    cat(
      paste0(
        "Identified collection ",
        collection
      )
    )
    if (length(collection) > 1) {
      stop(
        paste0(
          "Multiple collections detected. Ensure that data files ",
          "for each collection are stored in different directories ",
          "(ie. 'chm' in 'data/chm/' and 'aqc' in 'data/aqc/')."
        )
      )
      # mm-tests-0816 return error for multiple collections in one path
      # instead of warning to retain down-stream functionality
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
    # Process by variable in each day
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
            # Daily mean
            rast_summary <- terra::mean(rast_in[[rast_inidx]])

            # Assign time tag
            rtin <- as.Date(terra::time(rast_in))
            rtin_u <- unique(rtin)
            message(sprintf("Processing %s, date: %s", v, rtin_u))

            terra::time(rast_summary) <-
              rep(rtin_u, terra::nlyr(rast_summary))
            names(rast_summary) <-
              paste0(
                rep(gsub("_lev=.*", "", v), terra::nlyr(rast_summary))
              )
            terra::set.crs(rast_summary, "EPSG:4326")
            return(rast_summary)
          }
        )
      sds_proc <- terra::sds(sds_proc)

      # SpatVector conversion
      locstr <- terra::vect(locs)
      # Raster to point extraction
      rast_ext <- terra::extract(sds_proc, locstr, ID = TRUE)

      # Full join across all variables
      rast_ext <-
        Reduce(function(dfa, dfb) dplyr::full_join(dfa, dfb, by = "ID"),
          rast_ext
        )
      rast_ext$time <- unique(as.Date(terra::time(rast_in)))
      rast_ext$ID <- unlist(locs[[locs_id]])[rast_ext$ID]
      names(rast_ext)[names(rast_ext) == "ID"] <- locs_id
      return(rast_ext)

    }

    rast_summary <-
      lapply(
        future_inserted,
        function(fs) summary_byvar(fs = fs)
      )
    # Daily results are rowbound
    rast_summary <- data.table::rbindlist(rast_summary, fill=TRUE)

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
#' @return A data.frame containing the extracted GMTED data.
#' @importFrom terra rast varnames extract
#' @importFrom amadeus process_gmted_codes calc_prepare_locs calc_worker
#' @importFrom amadeus download_sanitize_path
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
  # mm-tests-0816 replaced "list.dir" with "list.files" to accomodate for
  # spatially subsetted and saved files
  # using "list.dir" does not properly read file path for re-written ESRI ASCII
  # grid file
  paths <- list.files(
    path,
    full.names = TRUE,
    include.dirs = TRUE
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
  # mm-tests-0816 ensures that .prj and .aux.xml files are not read
  # with terra::rast if path points to re-written ASCII file
  data_path <- data_path[!grepl("\\.prj$|\\.aux\\.xml$", data_path)]

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
#' @return A data frame containing the aggregated values for each
#'  location and time point.
#' @importFrom amadeus calc_prepare_locs calc_worker
#' @importFrom terra time
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr rowwise mutate ungroup
#' @importFrom data.table as.data.table
#' @export
calc_narr2 <- function(
  from,
  locs,
  locs_id = NULL,
  radius = 0,
  fun = "mean",
  ...
) {
  # name <- geometry <- value <- NULL
  name <- value <- NULL
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
        # mm-tests-0816 pending removal
        # "geometry" column is dropped during locs = locs[, "site_id"],
        # in line 410
        # if ("geometry" %in% names(sites_extracted_day)) {
        #   sites_extracted_day <- sites_extracted_day |>
        #     dplyr::select(-geometry)
        # }
        return(sites_extracted_day)
      },
      time_split
    )
    sites_extracted <- beethoven::reduce_merge(
      sites_extracted, by = c("site_id")
    )
  } else {
    sites_extracted <-
      terra::extract(
        from,
        sites_e,
        bind = TRUE
      )
    sites_extracted <- as.data.frame(sites_extracted)
    # mm-tests-0816 pending removal
    # "geometry" column is dropped during locs = locs[, "site_id"],
    # in line 410
    # if ("geometry" %in% names(sites_extracted)) {
    #   sites_extracted <- sites_extracted |>
    #     dplyr::select(-geometry)
    # }
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
#' @param path A character vector specifying the path to the NARR data.
#' @param date A character vector specifying the date of the
#'  NARR data to process.
#' @param locs A data frame specifying the locations to calculate NARR data for.
#'
#' @return A list of results from the parallel processing.
#' @export
par_narr <- function(domain, path, date, locs) {

  if (!dir.exists(path)) {
    stop("The specified path does not exist.")
  }

  res <-
    lapply(
      domain,
      function(x) {
        from <- beethoven::process_narr2(
          path = path,
          variable = x,
          date = date
        )
        beethoven::calc_narr2(
          from = from,
          locs = locs,
          locs_id = "site_id"
        )
      }
    )
  return(res)

}

#' Identify MODIS files
#' @description
#' This function identifies the relevant MODIS file paths based on
#' path, list of julian dates, and index. Designed to help set arguments
#' for the `inject_modis_par` function.
#' @keywords Calculation
#' @param path A character vector specifying the path to the MODIS data.
#' @param list A list of julian dates.
#' @param index An integer specifying the index of the julian date to use.
#' @return A character vector of MODIS file paths.
#' @export
query_modis_files <- function(path, list, index) {
  grep_files <- list.files(
    path,
    full.names = TRUE,
    recursive = TRUE
  ) |> grep(
    pattern = paste0(
      "A", list[[index]], collapse = "|"
    ),
    value = TRUE
  )
  return(grep_files)
}



#' Calculate MODIS product covariates in multiple CPU threads
#' @keywords Calculation
#' @param from character. List of paths to MODIS/VIIRS files.
#' @param locs sf/SpatVector object. Unique locs where covariates
#' will be calculated.
#' @param locs_id character(1). Site identifier. Default is `"site_id"`
#' @param radius numeric. Radii to calculate covariates.
#' Default is `c(0, 1000, 10000, 50000)`.
#' @param preprocess function. Function to handle HDF files.
#' @param name_covariates character. Name header of covariates.
#' e.g., `"MOD_NDVIF_0_"`.
#' The calculated covariate names will have a form of
#' "\code{\{name_covariates\}\{zero-padded buffer radius in meters\}}",
#' e.g., 'MOD_NDVIF_0_50000' where 50 km radius circular buffer
#' was used to calculate mean NDVI value.
#' @param subdataset Indices, names, or search patterns for subdatasets.
#' Find detail usage of the argument in notes.
#' @param fun_summary character or function. Function to summarize
#'  extracted raster values.
#' @param package_list_add character. A vector with package names to load
#'  these in each thread. Note that `sf`, `terra`, `exactextractr`,
#' `doParallel`, `parallelly` and `dplyr` are the default packages to be
#' loaded.
#' @param export_list_add character. A vector with object names to export
#'  to each thread. It should be minimized to spare memory.
#' @param max_cells integer(1). Maximum number of cells to be read at once.
#' Higher values will expedite processing, but will increase memory usage.
#' Maximum possible value is `2^31 - 1`.
#' See [`exactextractr::exact_extract`] for details.
#' @param geom FALSE/"sf"/"terra".. Should the function return with geometry?
#' Default is `FALSE`, options with geometry are "sf" or "terra". The
#' coordinate reference system of the `sf` or `SpatVector` is that of `from.`
#' @param ... Arguments passed to `preprocess`.
# nolint start
#' @description `calculate_modis` essentially runs [`calculate_modis_daily`] function
#' in each thread (subprocess). Based on daily resolution, each day's workload
#' will be distributed to each thread. With `product` argument,
#' the files are processed by a customized function where the unique structure
#' and/or characteristics of the products are considered. `nthreads`
#' argument should be carefully selected in consideration of the machine's
#' CPU and memory capacities as products have their own memory pressure.
#' `locs` should be `sf` object as it is exportable to parallel workers.
# nolint end
#' @note Overall, this function and dependent routines assume that the file
#' system can handle concurrent access to the (network) disk by multiple
#' processes. File system characteristics, package versions, and hardware
#' settings and specification can affect the processing efficiency.
#' `locs` is expected to be convertible to `sf` object. `sf`, `SpatVector`, and
#' other class objects that could be converted to `sf` can be used.
#' Common arguments in `preprocess` functions such as `date` and `path` are
#' automatically detected and passed to the function. Please note that
#' `locs` here and `path` in `preprocess` functions are assumed to have a
#' standard naming convention of raw files from NASA.
#' The argument `subdataset` should be in a proper format
#' depending on `preprocess` function:
#' * `process_modis_merge()`: Regular expression pattern.
#'   e.g., `"^LST_"`
#' * `process_modis_swath()`: Subdataset names.
#'   e.g., `c("Cloud_Fraction_Day", "Cloud_Fraction_Night")`
#' * `process_blackmarble()`: Subdataset number.
#'   e.g., for VNP46A2 product, 3L.
#' Dates with less than 80 percent of the expected number of tiles,
#' which are determined by the mode of the number of tiles, are removed.
#' Users will be informed of the dates with insufficient tiles.
#' The result data.frame will have an attribute with the dates with
#' insufficient tiles.
#' @return A data.frame or SpatVector with an attribute:
#' * `attr(., "dates_dropped")`: Dates with insufficient tiles.
#'   Note that the dates mean the dates with insufficient tiles,
#'   not the dates without available tiles.
#' @seealso
#' This function leverages the calculation of single-day MODIS
#' covariates:
#' * [`calculate_modis_daily()`]
#'
#' Also, for preprocessing, please refer to:
#' * [`process_modis_merge()`]
#' * [`process_modis_swath()`]
#' * [`process_blackmarble()`]
#' @importFrom methods is
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @importFrom terra nlyr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom rlang inject
#' @importFrom parallelly availableWorkers
#' @examples
#' ## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
#' ##       amount of data which is not included in the package.
#' \dontrun{
#' locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
#' locs <- terra::vect(locs, geom = c("lon", "lat"), crs = "EPSG:4326")
#' calculate_modis(
#'   from =
#'     list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
#'   locs = locs,
#'   locs_id = "site_id",
#'   radius = c(0L, 1000L),
#'   preprocess = process_modis_merge,
#'   name_covariates = "cloud_fraction_0",
#'   subdataset = "Cloud_Fraction",
#'   fun_summary = "mean"
#' )
#' }
#' @export
calculate_modis <-
  function(
    from = NULL,
    locs = NULL,
    locs_id = "site_id",
    radius = c(0L, 1e3L, 1e4L, 5e4L),
    preprocess = amadeus::process_modis_merge,
    name_covariates = NULL,
    subdataset = NULL,
    fun_summary = "mean",
    package_list_add = NULL,
    export_list_add = NULL,
    max_cells = 3e7,
    geom = FALSE,
    ...
  ) {
    amadeus::check_geom(geom)
    if (!is.function(preprocess)) {
      stop("preprocess should be one of process_modis_merge,
process_modis_swath, or process_blackmarble.")
    }
    # read all arguments
    # nolint start
    hdf_args <- c(as.list(environment()), list(...))
    # nolint end
    dates_available_m <-
      regmatches(from, regexpr("A20\\d{2,2}[0-3]\\d{2,2}", from))
    dates_available <- sort(unique(dates_available_m))
    dates_available <- sub("A", "", dates_available)

    # When multiple dates are concerned,
    # the number of tiles are expected to be the same.
    # Exceptions could exist, so here the number of tiles are checked.
    summary_available <- table(dates_available_m)
    summary_available_mode <-
      sort(table(summary_available), decreasing = TRUE)[1]
    summary_available_mode <- as.numeric(names(summary_available_mode))
    summary_available_insuf <-
      which(summary_available < floor(summary_available_mode * 0.8))

    if (length(summary_available_insuf) > 0) {
      dates_insuf <-
        as.Date(dates_available[summary_available_insuf], "%Y%j")
      message(
        paste0(
          "The number of tiles on the following dates are insufficient: ",
          paste(dates_insuf, collapse = ", "),
          ".\n"
        )
      )
      # finally it removes the dates with insufficient tiles
      dates_available <- dates_available[-summary_available_insuf]
    } else {
      dates_insuf <- NA
    }

    locs_input <- try(sf::st_as_sf(locs), silent = TRUE)
    if (inherits(locs_input, "try-error")) {
      stop("locs cannot be convertible to sf.
      Please convert locs into a sf object to proceed.\n")
    }

    export_list <- c()
    package_list <-
      c("sf", "terra", "exactextractr", "data.table", "stars",
        "dplyr", "parallelly", "rlang", "amadeus")
    if (!is.null(export_list_add)) {
      export_list <- append(export_list, export_list_add)
    }
    if (!is.null(package_list_add)) {
      package_list <- append(package_list, package_list_add)
    }

    # make clusters
    idx_date_available <- seq_along(dates_available)
    list_date_available <-
      split(idx_date_available, idx_date_available)
    calc_results <-
      lapply(
        list_date_available,
        FUN = function(datei) {
          options(sf_use_s2 = FALSE)
          # nolint start
          day_to_pick <- dates_available[datei]
          # nolint end
          day_to_pick <- as.Date(day_to_pick, format = "%Y%j")

          radiusindex <- seq_along(radius)
          radiusindexlist <- split(radiusindex, radiusindex)

          hdf_args <- c(hdf_args, list(date = day_to_pick))
          hdf_args <- c(hdf_args, list(path = hdf_args$from))
          # unified interface with rlang::inject
          vrt_today <-
            rlang::inject(preprocess(!!!hdf_args))

          if (sum(terra::nlyr(vrt_today)) != length(name_covariates)) {
            message("The number of layers in the input raster do not match
                    the length of name_covariates.\n")
          }

          res0 <-
            lapply(radiusindexlist,
              function(k) {
                name_radius <-
                  sprintf("%s%05d",
                          name_covariates,
                          radius[k])
                extracted <-
                  try(
                    amadeus::calculate_modis_daily(
                      locs = locs_input,
                      from = vrt_today,
                      locs_id = locs_id,
                      date = as.character(day_to_pick),
                      fun_summary = fun_summary,
                      name_extracted = name_radius,
                      radius = radius[k],
                      max_cells = max_cells,
                      geom = FALSE
                    )
                  )
                if (inherits(extracted, "try-error")) {
                  # coerce to avoid errors
                  error_df <- data.frame(
                    matrix(-99999,
                           ncol = length(name_radius) + 1,
                           nrow = nrow(locs_input))
                  )
                  error_df <- stats::setNames(error_df, c(locs_id, name_radius))
                  error_df[[locs_id]] <- unlist(locs_input[[locs_id]])
                  error_df$time <- day_to_pick
                  extracted <- error_df
                }
                return(extracted)
              }
            )
          res <-
            Reduce(function(x, y) {
              dplyr::left_join(x, y,
                by = c(locs_id, "time")
              )
            },
            res0)
          return(res)

        }
      )
    calc_results <- do.call(dplyr::bind_rows, calc_results)
    if (geom %in% c("sf", "terra")) {
      # merge
      calc_results_return <- merge(
        locs_input,
        calc_results,
        by = locs_id
      )
      if (geom == "terra") {
        calc_results_return <- terra::vect(calc_results_return)
      }
    } else {
      calc_results_return <- calc_results
    }
    attr(calc_results_return, "dates_dropped") <- dates_insuf
    Sys.sleep(1L)
    return(calc_results_return)
  }



#' Calculate MODIS from preprocessed file
#' @param file character(1). File path of preprocessed GeoTIFF file.
#' @param site data.frame/sf. AQS sites. data.frame will be converted to sf.
#' @param site_id character(1). Unique identifier of `site`
#' @param radius numeric(1). Circular buffer radius. Distance unit is inherited
#'   from the sites' coordinate system.
#' @param colheader character. Column name header.
#' @param mark logical(1). Mark zero-padded (length of 5) radius in the column name.
#' @importFrom chopin extract_at
#' @importFrom sf st_as_sf
#' @importFrom stats setNames
#' @importFrom stringi stri_extract_first_regex
#' @export 
calculate_modis_direct <-
  function(
    file,
    site,
    site_id,
    radius,
    colheader,
    mark = TRUE
  ) {
    radius <- as.integer(unlist(radius))
    chr_padr <- sprintf("%05d", radius)
    chr_coln <- if (mark) {
      sprintf("%s%s", colheader, chr_padr)
    } else {
      colheader
    }
    print(chr_coln)
    stopifnot(inherits(site, "data.frame"))

    if (inherits(site, "SpatVector")) {
      site <- try(sf::st_as_sf(site))
    }
    if (!inherits(site, "sf")) {
      site <- try(sf::st_as_sf(site, sf_column_name = "geometry"))
    }

    # time info
    chr_file1 <- file[1]
    dateinfo <-
      stringi::stri_extract_first_regex(chr_file1, pattern = "2[0-9]{3,3}\\-[0-1][0-9]\\-[0-3][0-9]")
    dateinfo <- as.Date(dateinfo, format = "%Y-%m-%d")

    extracted <-
      chopin::extract_at(
        x = file,
        y = site,
        id = site_id,
        radius = radius
      )
    coln <- names(extracted)
    coln_replace <- grep("(mean|median|min|max)", coln)
    coln[coln_replace] <- chr_coln
    extracted <- setNames(extracted, coln)
    extracted[["time"]] <- dateinfo
    return(extracted)
  }




#' Modified isotropic Sum of Exponentially Decaying Contributions (SEDC)
#'   covariates
#' @description
#' Calculate toxic release values for polygons or isotropic buffer point
#' locations. Returns a \code{data.frame} object containing \code{locs_id}
#' and variables for each chemical in \code{from}.
#' @param from SpatVector(1). Output of \code{process_tri()}.
#' @param locs sf/SpatVector. Locations where TRI variables are calculated.
#' @param locs_id character(1). Unique site identifier column name.
#'  Default is `"site_id"`.
#' @param radius integer(1). Circular buffer radius.
#' @param ... Placeholders.
#' @author Insang Song, Mariana Kassien
#' @return a data.frame or SpatVector object
#' @note U.S. context.
#' @importFrom terra vect crs nearby hull buffer
#' @importFrom methods is
#' @importFrom data.table .SD rbindlist as.data.table merge.data.table
#' @importFrom utils read.csv
#' @importFrom dplyr ends_with all_of group_by ungroup
#' @importFrom dplyr filter mutate across summarize
#' @export
calc_tri_mod <-
  function(
    from = NULL,
    locs,
    locs_id = "site_id",
    radius = 1e3L,
    ...
  ) {
    stopifnot(length(radius) == 1)
    if (!methods::is(locs, "SpatVector")) {
      if (methods::is(locs, "sf")) {
        locs <- terra::vect(locs)
      }
    }
    if (!is.numeric(radius)) {
      stop("radius should be numeric.\n")
    }
    locs_re <- terra::project(locs, terra::crs(from))

    # split by year: locs and tri locations
    tri_cols <- grep("_AIR", names(from), value = TRUE)
    # error fix: no whitespace
    tri_cols <- sub(" ", "_", tri_cols)

    # list_radius <- split(radius, radius)
    locs_h <- terra::hull(locs)
    locs_hb <-
      terra::buffer(
        locs_h,
        width = radius,
        joinstyle = "mitre",
        capstyle = "square"
      )

    from_re <- from[locs_hb, ]

    df_tri <-
      sum_edc_mod(
        locs = locs_re,
        from = from_re,
        locs_id = locs_id,
        sedc_bandwidth = radius,
        target_fields = tri_cols
      )
    # bind element data.frames into one
    # df_tri <- data.table::as.data.table(df_tri)
    names(df_tri) <- gsub("X[0-9]{4,5}\\.", "", names(df_tri))
    if (nrow(df_tri) != nrow(locs)) {
      df_tri <-
        data.table::merge.data.table(
          data.table::as.data.table(locs),
          df_tri,
          by = locs_id
        )
    }


    # df_tri_return <- amadeus:::calc_return_locs(
    #   covar = df_tri,
    #   POSIXt = FALSE,
    #   geom = geom,
    #   crs = terra::crs(from)
    # )

    # read attr
    df_tri$time <- as.integer(attr(from, "tri_year"))

    return(df_tri)
  }



# nolint start
#' Calculate isotropic Sum of Exponentially Decaying Contributions (SEDC) covariates
#' @param from `SpatVector`(1). Point locations which contain point-source
#' covariate data.
#' @param locs sf/SpatVector(1). Locations where the sum of exponentially
#' decaying contributions are calculated.
#' @param locs_id character(1). Name of the unique id field in `point_to`.
#' @param sedc_bandwidth numeric(1).
#' Distance at which the source concentration is reduced to
#'  `exp(-3)` (approximately -95 %)
#' @param target_fields character(varying). Field names in characters.
#' @return a data.frame (tibble) or SpatVector object with input field names with
#'  a suffix \code{"_sedc"} where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - `attr(result, "sedc_bandwidth")``: the bandwidth where
#'  concentration reduces to approximately five percent
#'    - `attr(result, "sedc_threshold")``: the threshold distance
#'  at which emission source points are excluded beyond that
#' @note The function is originally from
#' [chopin](https://github.com/ropensci/chopin)
#' Distance calculation is done with terra functions internally.
#'  Thus, the function internally converts sf objects in
#'  \code{point_*} arguments to terra.
#'  The threshold should be carefully chosen by users.
#' @author Insang Song
#' @references
#' \insertRef{messier2012integrating}{amadeus}
#' 
#' \insertRef{web_sedctutorial_package}{amadeus}
#' @examples
#' set.seed(101)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' nc <- terra::project(nc, "EPSG:5070")
#' pnt_locs <- terra::centroids(nc, inside = TRUE)
#' pnt_locs <- pnt_locs[, "NAME"]
#' pnt_from <- terra::spatSample(nc, 10L)
#' pnt_from$pid <- seq(1, 10)
#' pnt_from <- pnt_from[, "pid"]
#' pnt_from$val1 <- rgamma(10L, 1, 0.05)
#' pnt_from$val2 <- rgamma(10L, 2, 1)
#'
#' vals <- c("val1", "val2")
#' sum_edc_mod(pnt_locs, pnt_from, "NAME", 1e4, vals)
#' @importFrom collapse set_collapse qDT
#' @importFrom terra nearby distance buffer
#' @importFrom rlang sym
#' @export
# nolint end
sum_edc_mod <-
  function(
    from = NULL,
    locs = NULL,
    locs_id = NULL,
    sedc_bandwidth = NULL,
    target_fields = NULL
  ) {
    collapse::set_collapse(mask = "manip")
    if (!methods::is(locs, "SpatVector")) {
      locs <- try(terra::vect(locs))
    }
    if (!methods::is(from, "SpatVector")) {
      from <- try(terra::vect(from))
    }

    cn_overlap <- intersect(names(locs), names(from))
    if (length(cn_overlap) > 0) {
      warning(
        sprintf(
          "There are %d fields with the same name.
The result may not be accurate.\n",
          length(cn_overlap)
        )
      )
    }
    len_point_locs <- seq_len(nrow(locs))

    locs$from_id <- len_point_locs
    locs_buf <-
      terra::buffer(
        locs,
        width = sedc_bandwidth * 2,
        quadsegs = 90
      )

    from_in <- from[locs_buf, ]
    len_point_from <- seq_len(nrow(from_in))

    # len point from? len point to?
    from_in$to_id <- len_point_from
    dist <- NULL

    # near features with distance argument: only returns integer indices
    # threshold is set to the twice of sedc_bandwidth
    res_nearby <-
      terra::nearby(locs, from_in, distance = sedc_bandwidth * 2)
    # attaching actual distance
    dist_nearby <- terra::distance(locs, from_in)
    dist_nearby_df <- as.vector(dist_nearby)
    # adding integer indices
    dist_nearby_tdf <-
      expand.grid(
        from_id = len_point_locs,
        to_id = len_point_from
      )
    dist_nearby_df <- cbind(dist_nearby_tdf, dist = dist_nearby_df)

    # summary
    res_sedc <- res_nearby |>
      as.data.table() |>
      left_join(as.data.table(locs)) |>
      left_join(as.data.table(from_in)) |>
      left_join(as.data.table(dist_nearby_df)) |>
      # per the definition in
      # https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html
      # exp(-3) is about 0.05 * (value at origin)
      mutate(w_sedc = exp((-3 * dist) / sedc_bandwidth)) |>
      group_by(!!rlang::sym(locs_id)) |>
      summarize(
        across(
          all_of(target_fields),
          ~sum(w_sedc * ., na.rm = TRUE)
        )
      ) |>
      ungroup()

    idx_air <- grep("_AIR_", names(res_sedc))
    names(res_sedc)[idx_air] <-
      sprintf("%s_%05d", names(res_sedc)[idx_air], sedc_bandwidth)


    attr(res_sedc, "sedc_bandwidth") <- sedc_bandwidth
    attr(res_sedc, "sedc_threshold") <- sedc_bandwidth * 2

    return(res_sedc)
  }



