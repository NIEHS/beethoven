## pipeline base functions
# nocov start

## file check: chunking
## if using tarchetypes::tar_files,
## the file *lists* should be stored as a single file
## Provided that the download is completed in a defined
## time period such that users can distiguish a **set** of files
## from each other,
## timestamp check: `fs::file_info(...)$modification_time`
## can be used in bulk file check (will be time consuming as
## the number of files grow, though).
## The file list of the previous successful run will be stored as a file
## and we just save the file list of the current run, which are
## older than a certain rerun interval (e.g., 6 months).
## If one really wants to keep the shorter rerun interval,
## the strategy should be changed.
## THINK: How can we know the downloaded files are complete and correct?
## quick diagram:
## file set 1 ...  file set x
## (listing function runs)
## list1.rds  ...  listx.rds
## (hashed; not modified) ...   (not run)
## (pass)    ...   (run)
## ...       ...   (downstream process + calculation)
## (as-is)   ...   (as-is)    --- unless modified or manually invalidated

#' Load arguments from the formatted argument list file
#' @keywords Utility
#' @param argfile character(1). Path to the argument file. RDS format.
#' @param dataset character(1). Dataset name.
#' @returns A list of arguments.
#' @importFrom qs qread
#' @export
loadargs <- function(argfile, dataset) {
  if (endsWith(argfile, ".rds")) {
    arglist <- readRDS(argfile)
  } else if (endsWith(argfile, ".qs")) {
    arglist <- qs::qread(argfile)
  } else {
    stop("Invalid format.")
  }
  arglist[[dataset]]
}


#' Check if a query date falls within a time interval
#'
#' This function checks if a given query date falls within a time interval
#' defined by a vector of two dates.
#' @keywords Miscellaneous
#' @param query_date The query date to check.
#' @param tvec A vector of two dates defining the time interval.
#'
#' @returns TRUE if the query date falls within the time interval,
#' FALSE otherwise.
#'
#' @examples
#' query_date <- as.Date("2022-01-01")
#' tvec <- c(as.Date("2021-01-01"), as.Date("2023-01-01"))
#' `%tin%`(query_date, tvec)
#'
#' @export
`%tin%` <- function(query_date, tvec) {
  tvec <- sort(tvec)
  query_date <= tvec[1] & query_date >= tvec[2]
}

#' Load MODIS files from a specified path.
#'
#' This function takes a path and an optional pattern as input and
#'   returns a list of MODIS files found in the specified path.
#' @keywords Utility
#' @param path The path where the MODIS files are located.
#' @param pattern An optional regular expression pattern to filter the files.
#'   The default pattern is "hdf$".
#' @param date A vector of two dates to filter the files by.
#'   The default is an empty character vector.
#' @returns A list of full file names of the MODIS files found
#'   in the specified path.
#'
#' @examples
#' \dontrun{
#' # Load MODIS files from the current directory
#' modis_files <- load_modis_files(".")
#'
#' # Load MODIS files from a specific directory with a custom pattern
#' modis_files <- load_modis_files("/path/to/files", pattern = "MOD.*hdf$")
#' }
#' @export
load_modis_files <- function(path, pattern = "hdf$", date = character(2)) {
  modis_files <-
    list.files(
      path, pattern = pattern,
      recursive = TRUE,
      full.names = TRUE
    )
  date_exp <-
    amadeus::generate_date_sequence(date[1], date[2], sub_hyphen = FALSE)
  date_exp <- strftime(date_exp, format = "%Y%j")
  modis_files <-
    grep(
      sprintf("(%s)", paste(paste0("A", date_exp), collapse = "|")),
      modis_files, value = TRUE
    )
  return(modis_files)
}

#' Injects the calculate function with specified arguments.
#'
#' This function injects the calculate function with the specified arguments,
#' allowing for dynamic customization of the function's behavior.
#' @keywords Calculation
#' @param covariate character(1). The name of the covariate to be calculated.
#' @param locs The locations to be used in the calculation.
#' @param injection Additional arguments to be injected into
#'   the calculate function.
#'
#' @returns The result of the calculate function with the injected arguments.
#'
#' @examples
#' \dontrun{
#' inject_calculate(
#'   locs = my_locs, buffer = 10, domain = my_domain,
#'   injection = list(arg1 = "value1", arg2 = "value2")
#' )
#' }
#' @export
inject_calculate <- function(covariate, locs, injection) {
  rlang::inject(
    calculate(
      locs = locs,
      !!!injection
    )
  )
}

#' Injects arguments to parallelize MODIS/VIIRS data processing
#'
#' @keywords Calculation
#' @param locs A data frame containing the locations for which MODIS
#'   features need to be calculated.
#' @param domain The domain in which the MODIS PAR data should be injected.
#' @param injection Additional parameters to be passed to the
#'   `calc_modis_par` function.
#' @returns The modified domain with the injected MODIS PAR data.
#' @export
inject_modis_par <- function(locs, domain, injection) {
  rlang::inject(
    amadeus::calc_modis_par(
      locs = locs,
      locs_id = "site_id",
      !!!injection
    )
  )
}

#' Injects geographic information into a data frame
#'
#' This function injects geographic information into a data frame using
#'   the `calc_geos_strict` function. The injected information includes
#'   latitude and longitude coordinates based on the specified locations,
#'   a location ID column, a window range, and a snapping option.
#'
#' @keywords Calculation
#' @param locs A data frame containing the locations for which
#'   geographic information needs to be injected.
#' @param injection A list of additional arguments to be passed to
#'   the `calc_geos_strict` function.
#' @returns A modified data frame with injected geographic information.
#' @export
inject_geos <- function(locs, injection) {
  rlang::inject(
    calc_geos_strict(
      locs = locs,
      locs_id = "site_id",
      win = c(-126, -62, 22, 52),
      snap = "out",
      !!!injection
    )
  )
}


#' Injects GMTED data into specified locations
#'
#' This function injects GMTED (Global Multi-resolution Terrain Elevation Data)
#'  into specified locations. It calculates the GMTED values for each
#'  location within different radii and returns the merged results.
#'
#' @keywords Calculation
#' @param locs A data frame/sf/SpatVector containing the locations
#'   where GMTED variables needs to be calculated
#' @param variable The variable for which GMTED data needs to be calculated.
#' @param radii A vector of radii for which GMTED data needs
#'   to be calculated.
#' @param injection A list of additional arguments to be passed to
#'   the `calc_gmted_direct` function.
#' @param nthreads The number of threads to be used for parallel processing.
#'  Default is 4.
#'
#' @returns A data frame containing the merged results of GMTED data
#'   for each location within different radii.
#' @importFrom future plan
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject
#' @export
inject_gmted <- function(locs, variable, radii, injection, nthreads = 4L) {
  future::plan(future::multicore, workers = nthreads)

  radii_list <- split(radii, seq_along(radii))
  radii_rep <-
    future.apply::future_lapply(
      radii_list,
      function(r) {
        rlang::inject(
          calc_gmted_direct(
            locs = locs,
            locs_id = "site_id",
            radius = r,
            variable = c(variable, "7.5 arc-seconds"),
            !!!injection
          )
        )
      }
    )
  radii_rep <- lapply(radii_rep, function(x) as.data.frame(x))
  radii_join <- reduce_merge(radii_rep, "site_id")
  future::plan(future::sequential)
  return(radii_join)
}


#' Reduce and merge a list of data tables
#'
#' This function takes a list of data tables and merges them together
#'  using the specified columns. It uses the `merge.data.table` function
#'  from the `data.table` package to perform the merge.
#'
#' @param list_in A list of data tables to be merged.
#' @param by The columns to merge the data tables on.
#' @param all.x logical(1). Keeping all rows from the first input.
#' @param all.y logical(1). Keeping all rows from the second input.
#' @returns A merged data table.
#' @keywords Post-calculation
#' @examples
#' # Create example data tables
#' dt1 <- data.table(a = 1:3, b = 4:6)
#' dt2 <- data.table(a = 2:4, c = 7:9)
#' dt3 <- data.table(a = 3:5, d = 10:12)
#'
#' # Merge the data tables
#' reduce_merge(list(dt1, dt2, dt3), by = "a")
#'
#' @importFrom data.table as.data.table merge.data.table
#' @export
reduce_merge <-
  function(
    list_in,
    by = c("site_id", "time"),
    all.x = TRUE, all.y = FALSE
  ) {
    list_check <- sapply(list_in, nrow)
    list_checkdiff <- diff(list_check)
    if (any(list_checkdiff > 0)) all.y <- TRUE
    for (i in seq_len(length(list_in))) {
      list_in[[i]] <- data.table::as.data.table(list_in[[i]])
    }

    Reduce(
      function(x, y) {
        if (is.null(by)) by <- intersect(names(x), names(y))
        data.table::merge.data.table(
          x, y, by = by, all.x = all.x, all.y = all.y
        )
      },
      list_in
    )
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

#' Add Time Column
#'
#' This function adds a time column to a data frame.
#'
#' @keywords Post-calculation
#' @param df The data frame to which the time column will be added.
#' @param time_value The value to be assigned to the time column.
#' @param time_id The name of the time column (default is "time").
#'
#' @returns The data frame with the added time column.
#'
#' @examples
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' add_time_col(df, "2022-01-01")
#'
#' @export
add_time_col <- function(df, time_value, time_id = "time") {
  if (!time_id %in% names(df)) {
    df[[time_id]] <- time_value
  }
  return(df)
}


# 2018~2022, 2017, 2020
# 2017 ... 2020 ...
# 2017
#' Map the available raw data years over the given period
#' @description
#' Many raw datasets are periodically updated and the period could
#' be longer than a year. This function maps the available years
#' over the given period.
#' @keywords Post-calculation
#' @param time_start integer(1). Starting year.
#' @param time_end integer(1). Ending year.
#' @param time_unit character(1). Time unit. Default is `"year"`.
#' @param time_available vector. Available years.
#' @returns integer vector of length (time_end - time_start + 1).
#' Each element will get the nearest preceeding available year.
#' @note
#' The minimum of `time_available` will be filled in front of the first
#' available year when the minimum of `time_available` is greater
#' than `time_start`.
#' @examples
#' process_year_expand(2018, 2022, "year", c(2017, 2020, 2021))
#' process_year_expand(2018, 2022, "year", c(2020, 2021))
#' @export
post_calc_year_expand <-
  function(
    time_start = NULL,
    time_end = NULL,
    time_unit = "year",
    time_available = NULL
  ) {
    time_seq <- seq(time_start, time_end)
    time_target_seq <- findInterval(time_seq, time_available)
    time_target_seq <- time_available[time_target_seq]
    if (min(time_available) > time_start) {
      time_target_seq <-
        c(
          rep(min(time_available),
              min(time_available) - time_start),
          time_target_seq
        )
    }
    return(time_target_seq)
  }


#' Expand a data frame by year
#'
#' This function expands a data frame by year, creating multiple rows
#'  for each year based on the time period specified.
#' @keywords Post-calculation
#' @param df The input data frame.
#' @param locs_id The column name of the location identifier in the data frame.
#' @param time_field The column name of the time field in the data frame.
#' @param time_start The start of the time period.
#' @param time_end The end of the time period.
#' @param time_unit The unit of time to expand the data frame. Only for record.
#' @param time_available A vector of available time periods.
#' @param ... Placeholders.
#' @note Year expansion rule is to assign the nearest past year
#' in the available years;#' if there is no past year in the available years,
#' the first available year is rolled back to the start of the time period.
#' @returns The expanded data frame with multiple rows for each year.
#' @seealso [`post_calc_year_expand()`]
#' @examples
#' df <- data.frame(year = c(2010, 2010, 2011, 2012),
#'                  value = c(1, 2, 3, 4))
#' df_expanded <- df_year_expand(df, locs_id = "site_id", time_field = "year",
#'                               time_start = 2011, time_end = 2012,
#'                               time_unit = "year")
#' print(df_expanded)
#' @importFrom stats sd
#' @export
post_calc_df_year_expand <- function(
  df,
  locs_id = "site_id",
  time_field = "time",
  time_start = NULL,
  time_end = NULL,
  time_unit = "year",
  time_available = NULL,
  ...
) {
  time_summary <- table(unlist(df[[time_field]]))
  if (length(time_summary) != 1) {
    if (stats::sd(time_summary) != 0) {
      stop("df should be a data frame with the same number of rows per year")
    }
  }
  # assume that df is the row-bound data frame
  if (is.character(df[[time_field]])) {
    df[[time_field]] <- as.integer(df[[time_field]])
  }
  df_years <- unique(df[[time_field]])
  nlocs <- length(unique(df[[locs_id]]))
  year_period <- seq(time_start, time_end)
  # assign the time period to the available years
  year_assigned <-
    post_calc_year_expand(time_start, time_end, time_unit, df_years)
  df_years_repeats <- table(year_assigned)

  # repeat data frames
  df_expanded <- Map(
    function(y) {
      df_sub <- df[df[[time_field]] == df_years[y], ]
      df_sub <- df_sub[rep(seq_len(nrow(df_sub)), df_years_repeats[y]), ]
      return(df_sub)
    },
    seq_along(year_assigned)
  )
  df_expanded <- do.call(rbind, df_expanded)
  df_expanded[[time_field]] <- rep(year_period, each = nlocs)
  return(df_expanded)
}


# calculate over a list
#' Spatiotemporal covariate calculation
#' @keywords Calculation
#' @param domain vector of integer/character/Date.
#' Depending on temporal resolution of raw datasets.
#' Nullable; If `NULL`, it will be set to `c(1)`.
#' @param domain_name character(1). Name of the domain. Default is `"year"`.
#' @param nthreads integer(1). Number of threads to use.
#' @param process_function Raw data processor. Default is
#' [`amadeus::process_covariates`]
#' @param calc_function Function to calculate covariates.
#' [`amadeus::calc_covariates`]
#' @param ... Arguments passed to `process_function` and `calc_function`
#' @returns A data.table object.
#' @importFrom data.table rbindlist
#' @importFrom rlang inject
#' @export
# FIXME: this function works inefficiently in expense of
# returning uniform list of length(|years|) output.
# It could seriously affect the performance in scaled calculation
# as it calculates the same covariate for several years.
# Future updates should reduce the workload by calculating
# source data years only then assign proper preceding years
# to the output as another target.
calculate <-
  function(
    domain = NULL,
    domain_name = "year",
    nthreads = 1L,
    process_function = amadeus::process_covariates,
    calc_function = amadeus::calc_covariates,
    ...
  ) {
    if (is.null(domain)) {
      domain <- c(1)
    }
    # split the domain, make years from the domain list
    # assuming that domain length is the same as the number of years
    domainlist <- split(domain, seq_along(domain))
    years_data <- seq_along(domain) + 2017

    if (nthreads == 1L) {
      future::plan(future::sequential)
    } else {
      future::plan(future::multicore, workers = nthreads)
    }
    # double twists: list_iteration is made to distinguish
    # cases where a single radius is accepted or ones have no radius
    # argument.
    res_calc <-
      #try(
      future.apply::future_mapply(
        function(domain_each, year_each) {
          # we assume that ... have no "year" and "from" arguments
          args_process <- c(arg = domain_each, list(...))
          names(args_process)[1] <- domain_name
          if (!is.null(args_process$covariate) &&
              any(names(args_process) %in% c("covariate"))
          ) {
            if (args_process$covariate == "nei") {
              args_process$county <- process_counties()
            }
          }

          # load balancing strategy
          # if radius is detected, split the list
          if (any(names(args_process) %in% c("radius"))) {
            list_iteration <-
              split(args_process$radius, seq_along(args_process$radius))
          } else {
            list_iteration <- list(1)
          }

          list_iteration_calc <-
            Map(
              function(r) {
                args_process$radius <- r
                from_in <-
                  rlang::inject(
                    process_function(!!!args_process)
                  )
                res <- rlang::inject(
                  calc_function(
                    from = from_in,
                    !!!args_process
                  )
                )
                # using domain_name, add both
                # data year and covariate year
                if (!is.null(domain) && domain_name == "year") {
                  res <-
                    add_time_col(
                      res, domain_each,
                      sprintf("%s_year", unname(args_process$covariate))
                    )
                }
                res <- data.table::as.data.table(res)
                return(res)
              },
              list_iteration
            )
          df_iteration_calc <-
            if (length(list_iteration_calc) == 1) {
              list_iteration_calc[[1]]
            } else {
              by_detected <-
                Reduce(intersect, lapply(list_iteration_calc, names))
              reduce_merge(list_iteration_calc, by = by_detected)
            }
          return(df_iteration_calc)
        },
        domainlist, years_data, SIMPLIFY = FALSE,
        future.seed = TRUE
      )

    future::plan(future::sequential)
    if (inherits(res_calc, "try-error")) {
      cat(paste0(attr(res_calc, "condition")$message, "\n"))
      stop("Results do not match expectations.")
    }
    res_calc <- lapply(res_calc,
      function(x) {
        if ("time" %in% names(x)) {
          if (nchar(x$time[1]) != 4) {
            x$time <- data.table::as.IDate(x$time)
          }
        }
        xconvt <- data.table::as.data.table(x)
        return(xconvt)
      }
    )
    # res_calcdf <- if (length(res_calc) == 1) {
    #   data.table::as.data.table(res_calc[[1]])
    # } else if (domain_name %in% c("year", "date")) {
    #   data.table::rbindlist(res_calc, use.names = TRUE, fill = TRUE)
    # } else {
    #   reduce_merge(res_calc, by = c("site_id", "time"))
    # }
    return(res_calc)
  }



#' Set resource management for SLURM
#' @keywords Utility
#' @param template_file SLURM job submission shell template path.
#' @param partition character(1). Name of partition. Default is `"geo"`
#' @param ncpus integer(1). Number of CPU cores assigned to each task.
#' @param ntasks integer(1). Number of tasks to submit.
#' @param memory integer(1). Specifically odds to 2*x GB.
#' @param user_email character(1). User email address.
#' @param error_log character(1). Error log file name.
#' @note This function is designed to be used with `tar_resources`.
#' Suggested number of `ncpus` is more than 1 for typical multicore R tasks.
#' @returns A list of resources for `tar_resources`
#' @author Insang Song
#' @importFrom future tweak
#' @importFrom future.batchtools batchtools_slurm
#' @importFrom targets tar_resources
#' @importFrom targets tar_resources_future
#' @export
set_slurm_resource <-
  function(
    template_file = "inst/targets/template_slurm.tmpl",
    partition = "geo",
    ncpus = 2L,
    ntasks = 2L,
    memory = 8,
    user_email,
    error_log = "slurm_error.log"
  ) {
    targets::tar_resources(
      future = targets::tar_resources_future(
        plan = future::tweak(
          future.batchtools::batchtools_slurm,
          template = template_file,
          resources =
            list(
              partition = partition,
              ntasks = ntasks,
              ncpus = ncpus,
              memory = memory,
              email = user_email,
              error.file = error_log
            )
        )
      )
    )
  }


#' Read AQS data
#' @keywords Utility
#' @param fun_aqs function to import AQS data.
#' Default is `amadeus::process_aqs`
#' @param export Export the file to qs. Default is FALSE.
#' @param ... Passed arguments to `fun_aqs`
#' @returns Depending on `fun_aqs` specification.
#' @importFrom qs qsave
#' @export
read_locs <-
  function(
    fun_aqs = amadeus::process_aqs,
    export = FALSE,
    ...
  ) {
    aqs_read <- fun_aqs(...)
    if (export) qs::qsave(aqs_read, file = "input/sf_feat_proc_aqs_sites.qs")
    return(aqs_read)
  }



#' Check file status and download if necessary
#' @keywords Utility
#' @param path download path.
#' @param dataset_name Dataset name. See [`amadeus::download_data`] for details.
#' @param ... Arguments passed to `amadeus::download_data`
#' @returns logical(1).
feature_raw_download <-
  function(
    path = NULL,
    dataset_name = NULL,
    ...
  ) {
    # run amadeus::download_data
    tryCatch(
      {
        amadeus::download_data(dataset_name = dataset_name, ...)
      },
      error = function(e) {
        stop(e)
      }
    )
  }

#' Load county sf object
#' @keywords Calculation
#' @param year integer(1). Year of the county shapefile.
#' @param exclude character. State FIPS codes to exclude.
#' Default is c("02", "15", "60", "66", "68", "69", "72", "78").
#' @returns sf object
#' @importFrom tigris counties
#' @export
process_counties <-
  function(
    year = 2020,
    exclude = c("02", "15", "60", "66", "68", "69", "72", "78")
  ) {
    options(tigris_use_cache = TRUE)
    cnty <- tigris::counties(year = year)
    cnty <-
      cnty[!cnty$STATEFP %in%
           c("02", "15", "60", "66", "68", "69", "72", "78"), ]
    return(cnty)
  }



#' Merge input data.frame objects
#' @param by character. Joining keys. See [`merge`] for details.
#' @param time logical(1). Whether or not include time identifier.
#' Set this `TRUE` will supersede `by` value by appending time identifier.
#' @param ... data.frame objects to merge
#' @returns data.table
#' @keywords Post-calculation
#' @importFrom data.table as.data.table
#' @importFrom data.table merge.data.table
#' @export
post_calc_merge_features <-
  function(
    by = c("site_id"),
    time = FALSE,
    ...
  ) {
    ellipsis <- list(...)
    if (time) {
      by <- c("site_id", "time")
      ellipsis_clean <-
        lapply(
          ellipsis,
          function(x) {
            x <- data.table::as.data.table(x)
            col_coords <- grep("(lon|lat)", names(x))
            if (length(col_coords) > 0 && !is.null(col_coords)) {
              x <- x[, -col_coords, with = FALSE]
            }
            x$time <- as.character(x$time)
            return(x)
          }
        )
    } else {
      ellipsis_clean <- ellipsis
    }
    joined <-
      Reduce(function(x, y) {
        data.table::merge.data.table(
          x, y,
          by = by, all.x = TRUE, suffixes = c("_Ma", "_Mb")
        )
      }, ellipsis_clean)
    return(joined)
  }


#' Change time column name
#' @param df data.frame
#' @param candidates character. Candidate column names.
#' @param replace character. New column name.
#' @returns data.frame
#' @keywords Post-calculation
#' @export
post_calc_unify_timecols <-
  function(
    df,
    candidates = c("year"),
    replace = "time"
  ) {
    if (sum(names(df) %in% candidates) > 1) {
      stop("More than a candidate is detected in the input.")
    }
    names(df)[names(df) %in% candidates] <- replace
    return(df)
  }


#' Convert time column to character
#' @keywords Post-calculation
#' @param df data.table
#' @note This function takes preprocessed data.table with
#'   a column named `"time"`.
#' @importFrom data.table as.data.table copy
#' @export
post_calc_convert_time <-
  function(
    df
  ) {
    df <- data.table::copy(data.table::as.data.table(df))
    df <- df[, `:=`(time, as.character(time))]
    return(df)
  }

#' Join a data.frame with a year-only date column to
#'  that with a full date column
#' @description The full date column will be converted to a year column
#' as a new column, then the data.frame with the year-only column will
#' be joined.
#' @keywords Post-calculation
#' @param df_year data.frame with a year-only date column
#' @param df_date data.frame with a full date column
#' @param field_year character(1). Year column in `df_year`
#' @param field_date character(1). Date column in `df_date`
#' @param spid character(1). Name of the unique location identifier field.
#' @importFrom methods is
#' @importFrom data.table merge.data.table
#' @importFrom data.table `:=`
#' @returns data.frame
post_calc_join_yeardate <-
  function(
    df_year,
    df_date,
    field_year = "time",
    field_date = "time",
    spid = "site_id"
  ) {
    if (!inherits(df_year, "data.frame") && !inherits(df_date, "data.frame")) {
      stop("Both inputs should be data.frame.")
    }

    names(df_year)[which(names(df_year) %in% field_year)] <- "year"
    df_year$year <- as.character(unlist(df_year$year))
    df_date$year <- as.character(substr(df_date[[field_date]], 1, 4))

    df_joined <-
      data.table::merge.data.table(
        df_date, df_year,
        by = c(spid, "year"),
        all.x = TRUE
      )

    df_joined <- df_joined[, c("year") := NULL]
    return(df_joined)
  }


#' Merge spatial and spatiotemporal covariate data
#' @keywords Post-calculation
#' @param locs Location. e.g., AQS sites.
#' @param locs_id character(1). Location identifier.
#' @param time_id character(1). Location identifier.
#' @param target_years integer. Used to dummify nominal year.
#' @param df_sp data.frame. Spatial-only covariates.
#' @param df_spt data.frame. Spatiotemporal covariates.
#' @note This version assumes the time_id contains Date-like strings.
#' @returns data.frame
#' @importFrom data.table merge.data.table
#' @export
post_calc_merge_all <-
  function(
    locs,
    locs_id,
    time_id,
    target_years = seq(2018, 2022),
    df_sp,
    df_spt
  ) {
    if (methods::is(locs, "sf")) {
      locs <- sf::st_drop_geometry(locs)
    }
    locs$time <- as.character(locs$time)
    locs <- data.table::as.data.table(locs)
    locs_merged <-
      data.table::merge.data.table(
        locs, df_sp, by = c(locs_id)
      )
    locs_merged <-
      data.table::merge.data.table(
        locs_merged, df_spt,
        by = c(locs_id, time_id)
      )
    locs_merged <-
      amadeus::calc_temporal_dummies(
        locs = locs_merged,
        locs_id = locs_id,
        year = target_years
      )
    return(locs_merged)
  }


#' Remove columns from a data frame based on regular expression patterns.
#' @keywords Post-calculation
#'
#' This function removes columns from a data frame that match
#'  any of the specified
#' regular expression patterns. By default, it removes columns with names that
#' match the patterns "^lon$|^lat$|geoid|year$|description".
#'
#' @param df The input data frame.
#' @param candidates A character vector of regular expression patterns
#'   to match against column names. Columns that match any of the patterns
#'   will be removed. The default value is
#'   "^lon$|^lat$|geoid|year$|description".
#' @param strict logical(1). If `TRUE`,
#'   only `c("site_id", "time")` will be kept.
#' @returns The modified data frame with the specified columns removed.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(lon = 1:5, lat = 6:10, geoid = 11:15, year = 2010:2014,
#'                  description = letters[1:5], other = 16:20)
#' post_calc_drop_cols(df)
#' }
#' @export
post_calc_drop_cols <-
  function(
    df,
    candidates = "(^lon$|^lat$|geoid|year$|description|geometry)",
    strict = FALSE
  ) {
    idx_remove <-
      if (!strict) {
        grep(candidates, names(df), value = TRUE)
      } else {
        grep("site_id|time", names(df), value = TRUE, invert = TRUE)
      }
    df <- df[, -idx_remove, with = FALSE]
    return(df)
  }

#' Automatic joining by the time and spatial identifiers
#' @description The key assumption is that all data frames will have
#' time field and spatial field and the data should have one of date or year.
#' Whether the input time unit is year or date
#' is determined by the coercion of the **first row value** of the time field
#' into a character with `as.Date()`. This function will fail if it
#' gets year-like string with length 4.
#'
#' @param df_fine The fine-grained data frame.
#' @param df_coarse The coarse-grained data frame.
#' @param field_sp The name of the spatial field in the data frames.
#' @param field_t The name of the time field in the data frames.
#' @param year_start The starting year of the time period.
#' @param year_end The ending year of the time period.
#' @keywords Post-calculation
#' @returns A merged data table.
#' @examples
# nolint start
#' \dontrun{
#' df_fine0 <- data.frame(site_id = c("A", "B", "B", "C"),
#'                       time = as.Date(c("2022-01-01", "2022-01-02", "2021-12-31", "2021-01-03")),
#'                       value = c(1, 2, 3, 5))
#' df_coarse0 <- data.frame(site_id = c("A", "B", "C"),
#'                         time = c("2022", "2022", "2021"),
#'                         other_value = c(10, 20, 30))
#' jdf <- post_calc_autojoin(df_fine0, df_coarse0)
#' print(jdf)
#' }
# nolint end
#' @importFrom data.table merge.data.table
#' @importFrom rlang as_name
#' @importFrom rlang sym
#' @export
post_calc_autojoin <-
  function(
    df_fine,
    df_coarse,
    field_sp = "site_id",
    field_t = "time",
    year_start = 2018L,
    year_end = 2022L
  ) {
    if (any(grepl("population", names(df_coarse)))) {
      df_coarse <- df_coarse[, -c("time"), with = FALSE]
    }
    common_field <- intersect(names(df_fine), names(df_coarse))
    df_fine <- data.table::as.data.table(df_fine)
    df_coarse <- data.table::as.data.table(df_coarse)
    df_fine <- post_calc_drop_cols(df_fine)
    df_coarse <- post_calc_drop_cols(df_coarse)
    # if (length(common_field) > 2) {
    #   message("The data frames have more than two common fields.")
    #   message("Trying to remove the redundant common fields...")
    #   common_field <- intersect(names(df_fine), names(df_coarse))
    #   print(common_field)
    #   common_field <-
    #     common_field[-which(!common_field %in% c(field_sp, field_t))]
    # }
    if (length(common_field) == 1) {
      print(common_field)
      if (common_field == field_sp) {
        joined <- data.table::merge.data.table(
          df_fine, df_coarse,
          by = field_sp,
          all.x = TRUE
        )
      }
    }
    if (length(common_field) == 2) {
      if (all(common_field %in% c(field_sp, field_t))) {
        # t_fine <- try(as.Date(df_fine[[field_t]][1]))
        df_fine[[field_t]] <- as.character(df_fine[[field_t]])
        df_coarse[[field_t]] <- as.character(df_coarse[[field_t]])
        t_coarse <- try(as.Date(df_coarse[[field_t]][1]))
        if (inherits(t_coarse, "try-error")) {
          message(
            "The time field includes years. Trying different join strategy."
          )
          coarse_years <- sort(unique(unlist(as.integer(df_coarse[[field_t]]))))
          df_coarse2 <- post_calc_df_year_expand(
            df_coarse,
            time_start = year_start,
            time_end = year_end,
            time_available = coarse_years
          )
          joined <-
            post_calc_join_yeardate(df_coarse2, df_fine, field_t, field_t)
        } else {
          joined <- data.table::merge.data.table(
            df_fine, df_coarse,
            by = c(field_sp, field_t),
            all.x = TRUE
          )
        }
      }
    }
    return(joined)
  }



#' Read paths from a directory with a specific file extension
#' @keywords Utility
#' @param path The directory path from which to read the paths.
#' @param extension The file extension to match. Defaults to ".hdf".
#' @param target_dates A character vector of length 2 containing
#'  the start and end dates.
#' @param julian logical(1). If `TRUE`, the dates are in Julian format.
#' @returns A character vector containing the full paths of the matching files.
#'
#' @examples
#' \dontrun{
#' # Read paths from a directory with default extension
#' read_paths("/path/to/directory")
#'
#' # Read paths from a directory with custom extension
#' read_paths("/path/to/directory", ".txt")
#' }
#' @export
read_paths <-
  function(
    path,
    extension = ".hdf",
    target_dates = c("2020-01-01", "2020-01-15"),
    julian = FALSE
  ) {
    flist <-
      list.files(
        path = path,
        pattern = sprintf("%s$", extension),
        full.names = TRUE,
        recursive = TRUE
      )
    if (!missing(target_dates)) {
      dateseq <-
        seq(as.Date(target_dates[1]), as.Date(target_dates[2]), by = "day")
      dateseq <-
        if (julian) format(dateseq, "%Y%j") else format(dateseq, "%Y%m%d")
      dateseq <- sprintf("A(%s)", paste(dateseq, collapse = "|"))
      flist <- grep(dateseq, flist, value = TRUE)
    }
    return(flist)
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
#' @export
search_function <- function(package, search) {
  library(package, character.only = TRUE)
  grep(search, ls(sprintf("package:%s", package)), value = TRUE)
}

#' Get data.frame of function parameters
#' @keywords Utility
#' @param functions character. Vector of function names.
#' @returns A data.frame containing the parameters of the functions.
#' @importFrom dplyr as_tibble bind_rows
#' @export
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

#' Process atmospheric composition data by chunks (v3)
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



#' Process NARR2 Data
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
#' @returns A SpatRaster object containing the processed NARR2 data.
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

#' Impute missing values and attach lagged features
#' @keywords Post-calculation
#' @note under construction.
#' This function performs imputation on a given data table
#'   by replacing missing values with imputed values.
#'   It follows a series of steps including data cleaning, name cleaning,
#'   geoscf column renaming, NDVI 16-day backward filling,
#'   zero-variance exclusion, excessive "true zeros" exclusion,
#'   and imputation using missRanger.
#'
#' @param dt The input data table to be imputed.
#' @param period The period for lagged features.
#' @param nthreads_dt The number of threads to be used for
#'   data.table operations.
#' @param nthreads_collapse The number of threads to be used for
#'   collapse operations.
#' @param nthreads_imputation The number of threads to be used for
#'   the imputation process.
#'
#' @returns The imputed data table with lagged features.
#'
#' @importFrom collapse set_collapse replace_inf replace_na fvar fnth
#' @importFrom data.table setDTthreads setnafill
#' @importFrom qs qread
#' @importFrom stats setNames
#' @importFrom stringi stri_replace_all_regex
#' @importFrom missRanger missRanger
#' @export
impute_all <-
  function(
    dt,
    period,
    nthreads_dt = 32L,
    nthreads_collapse = 32L,
    nthreads_imputation = 32L
  ) {
    data.table::setDTthreads(nthreads_dt)
    if (is.character(dt)) {
      dt <- file.path("output/qs", dt)
      dt <- qs::qread(dt)
    }
    # name cleaning
    dt <- stats::setNames(dt, sub("light_1", "OTH_HMSWL_0_00000", names(dt)))
    dt <- stats::setNames(dt, sub("medium_1", "OTH_HMSWM_0_00000", names(dt)))
    dt <- stats::setNames(dt, sub("heavy_1", "OTH_HMSWH_0_00000", names(dt)))
    dt <- stats::setNames(dt, sub("population_", "POP_SEDAC_0_", names(dt)))

    geoscn <-
      "ACET\tGEO_ACETO_0_00000
    ALD2\tGEO_ACETA_0_00000
    ALK4\tGEO_CALKA_0_00000
    BCPI\tGEO_HIBCA_0_00000
    BCPO\tGEO_HOBCA_0_00000
    BENZ\tGEO_BENZE_0_00000
    C2H6\tGEO_ETHTE_0_00000
    C3H8\tGEO_PROPA_0_00000
    CH4\tGEO_METHA_0_00000
    CO\tGEO_CMONO_0_00000
    DST1\tGEO_DUST1_0_00000
    DST2\tGEO_DUST2_0_00000
    DST3\tGEO_DUST3_0_00000
    DST4\tGEO_DUST4_0_00000
    EOH\tGEO_ETHOL_0_00000
    H2O2\tGEO_HYPER_0_00000
    HCHO\tGEO_FORMA_0_00000
    HNO3\tGEO_NITAC_0_00000
    HNO4\tGEO_PERAC_0_00000
    ISOP\tGEO_ISOPR_0_00000
    MACR\tGEO_METHC_0_00000
    MEK\tGEO_MEKET_0_00000
    MVK\tGEO_MVKET_0_00000
    N2O5\tGEO_DIPEN_0_00000
    NH3\tGEO_AMNIA_0_00000
    NH4\tGEO_AMNUM_0_00000
    NIT\tGEO_INNIT_0_00000
    NO\tGEO_NIOXI_0_00000
    NO2\tGEO_NIDIO_0_00000
    NOy\tGEO_NITRO_0_00000
    OCPI\tGEO_HIORG_0_00000
    OCPO\tGEO_HOORG_0_00000
    PAN\tGEO_PERNI_0_00000
    PM25_RH35_GCC\tGEO_PM25X_0_00000
    PM25_RH35_GOCART\tGEO_PM25R_0_00000
    PM25bc_RH35_GCC\tGEO_BLCPM_0_00000
    PM25du_RH35_GCC\tGEO_DUSPM_0_00000
    PM25ni_RH35_GCC\tGEO_NITPM_0_00000
    PM25oc_RH35_GCC\tGEO_ORCPM_0_00000
    PM25soa_RH35_GCC\tGEO_SORPM_0_00000
    PM25ss_RH35_GCC\tGEO_SEAPM_0_00000
    PM25su_RH35_GCC\tGEO_SULPM_0_00000
    PRPE\tGEO_CALKE_0_00000
    RCHO\tGEO_CALDH_0_00000
    SALA\tGEO_FSEAS_0_00000
    SALC\tGEO_CSEAS_0_00000
    SO2\tGEO_SULDI_0_00000
    SOAP\tGEO_SOAPR_0_00000
    SOAS\tGEO_SOASI_0_00000
    TOLU\tGEO_TOLUE_0_00000
    XYLE\tGEO_XYLEN_0_00000
    CO_y\tGEO_COVMR_0_00000
    NO2_y\tGEO_NOVMR_0_00000
    O3\tGEO_OZVMR_0_00000
    SO2_y\tGEO_SOVMR_0_00000"

    geoscn <- strsplit(geoscn, "\n")
    geoscn <- unlist(geoscn)
    geoscn <- strsplit(geoscn, "\t")
    geoscn <- do.call(rbind, geoscn)
    geoscndf <- as.data.frame(geoscn, stringsAsFactors = FALSE)
    colnames(geoscndf) <- c("variable", "code")
    geoscndf$variable <- trimws(geoscndf$variable)

    for (i in seq_len(nrow(geoscndf))) {
      dt <-
        setNames(
          dt,
          stringi::stri_replace_all_regex(
            names(dt), sprintf("%s$", geoscndf$variable[i]), geoscndf$code[i]
          )
        )
    }
    site_id <- NULL
    # NDVI 16-day
    # For each site_id, backward filling for 16-day NDVI
    # Last Observation Carried Forward is the method used;
    # it assumes that the rows are ordered by date
    dt <- dt[order(site_id, time), ]
    col_ndviv <- grep("MOD_NDVIV_", names(dt))
    dtndviv <-
      data.table::setnafill(
        dt, type = "nocb", nan = NA,
        cols = col_ndviv
      )

    collapse::set_collapse(mask = "manip", nthreads = nthreads_collapse)

    target_replace <- grep("^MOD_", names(dt), invert = TRUE)
    dt <- collapse::replace_inf(dtndviv, value = NA, replace.nan = TRUE)
    dt <- collapse::replace_na(dt, value = 0, cols = target_replace)

    # zero-variance exclusion
    dt_colvars <- collapse::fvar(dt[, 5:ncol(dt), with = FALSE])
    zero_var_fields <- names(dt_colvars[dt_colvars == 0])

    # Exclude fields with zero variance using data.table
    dt <- dt[, (zero_var_fields) := NULL]

    # Store the name of zero variance fields as an attribute of the input object
    attr(dt, "zero_var_fields") <- zero_var_fields

    # excluding columns with excessive "true zeros"
    # we should have a threshold for the zero rate
    # exc_zero <- collapse::fnth(dt[, 5:ncol(dt), with = FALSE], n = 0.9)
    # exc_zero <- unname(which(exc_zero == 0)) + 5L
    # dt <- dt[, (exc_zero) := NULL]

    # Q: Do we use all other features to impute? -- Yes.
    # 32-thread, 10% for tree building, 200 trees, 4 rounds: 11 hours
    imputed <-
      missRanger::missRanger(
        data = dt,
        maxiter = 30L,
        num.trees = 300L,
        num.threads = nthreads_imputation,
        mtry = 50L,
        sample.fraction = 0.1
      )

    imputed <- amadeus::calc_temporal_dummies(imputed, "time")
    return(imputed)
    # lagged features: changing period (period[1] + 1 day)
    # period <- as.Date(period)
    # period[1] <- period[1] + as.difftime(1, units = "days")
    # period <- as.character(period)
    # index_lag <-
    #   sprintf("MET_%s", c("ATSFC", "ACPRC", "PRSFC", "SPHUM", "WNDSP"))
    # index_lag <- grep(paste(index_lag, collapse = "|"), names(dt))
    # target_lag <- imputed[, index_lag, with = FALSE]

    # output <- amadeus::calc_lagged(target_lag, period, 1, "site_id")
    # return(output)
  }


#' Append Predecessors
#'
#' This function appends predecessors to an existing object or
#'  creates a new object if none exists.
#'
#' @keywords Post-calculation
#' @param path_qs The path where the predecessors will be stored.
#' @param period_new The new period to be appended.
#' @param input_new The new input object to be appended.
#' @param nthreads The number of threads to be used.
#'
#' @returns If no existing predecessors are found, the function saves
#'   the new input object and returns the name of the saved file.
#'   If existing predecessors are found, the function appends
#'   the new input object to the existing ones and returns the combined object.
#' @export
append_predecessors <-
  function(
    path_qs = "output/qs",
    period_new = NULL,
    input_new = NULL,
    nthreads = 8L
  ) {
    if (is.null(input_new)) {
      stop("Please provide a valid object.")
    }
    if (!dir.exists(path_qs)) {
      dir.create(path_qs, recursive = TRUE)
    }
    input_old <- list.files(path_qs, "*.*.qs$", full.names = TRUE)

    # validate input_old with period_new
    # if (length(input_old) > 0) {
    # periods_old <- do.call(rbind, strsplit(input_old, "_"))
    # periods_old <- periods_old[, 4:5]
    # periods_old_check <- vapply(
    #   seq(1, nrow(periods_old)),
    #   function(i) {
    #     period_old <- periods_old[i, ]
    #     period_old <- as.Date(period_old, format = "%Y-%m-%d")
    #     period_new <- as.Date(period_new, format = "%Y-%m-%d")
    #     if (period_new[1] < period_old[1] | period_new[2] < period_old[2]) {
    #       return(FALSE)
    #     } else {
    #       return(TRUE)
    #     }
    #   },
    #   logical(1)
    # )
    # if (!all(periods_old_check)) {
    #   stop("Results have an overlap period. Please provide a valid period.")
    # }
    # }
    period_new <- sapply(period_new, as.character)
    time_create <- gsub("[[:punct:]]|[[:blank:]]", "", Sys.time())
    name_qs <-
      sprintf(
        "dt_feat_pm25_%s_%s_%s.qs",
        period_new[1], period_new[2], time_create
      )
    if (length(input_old) == 0) {
      qs::qsave(input_new, file = file.path(path_qs, name_qs))
      return(name_qs)
    } else {
      # vv <- list()
      qs::qsave(input_new, file = file.path(path_qs, name_qs))
      input_update <- list.files(path_qs, "*.*.qs$", full.names = TRUE)
      bound_large <-
        Reduce(
          function(x, y) {
            if (inherits(x, "data.frame")) {
              bound <- rbind(x, qs::qread(y))
            } else {
              bound <- rbind(qs::qread(x), qs::qread(y))
            }
            return(bound)
          },
          input_update
        )
      return(bound_large)
    }
  }


# nested parallelization
# IN PROGRESS
# TODO: identify bottleneck
#' @noRd
par_nest <-
  function(
    path,
    ...
  ) {
    chopin::par_grid(
      path,
      fun_dist = calculate,
      ...
    )
  }


## base & meta learner fitting
# strategy:
# random subsample (~30%) ; row based
# P times...

#' Base learner: Multilayer perceptron with brulee
#'
#' Multilayer perceptron model with different configurations of
#' hidden units, dropout, activation, and learning rate using brulee
#' and tidymodels. With proper settings, users can utilize graphics
#' processing units (GPU) to speed up the training process.
#' @keywords Baselearner
#' @note Spatiotemporal cross-validation strategy is not yet implemented.
#'   tune package should be 1.2.0 or higher.
#' @param dt_imputed The input data table to be used for fitting.
#' @param folds pre-generated rset object. If NULL, it should be
#'   numeric to be used in [rsample::vfold_cv].
#' @param r_subsample The proportion of rows to be sampled.
#' @param yvar The target variable.
#' @param xvar The predictor variables.
#' @param vfold The number of folds for cross-validation.
#' @param ... Additional arguments to be passed.
#'
#' @returns The fitted workflow.
#' @importFrom recipes recipe update_role
#' @importFrom dplyr `%>%`
#' @importFrom parsnip mlp set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
#' @export
fit_base_brulee <-
  function(
    dt_imputed,
    folds = NULL,
    r_subsample = 0.3,
    yvar = "Arithmetic.Mean",
    xvar = seq(6, ncol(dt_imputed)),
    vfold = 5L,
    ...
  ) {
    # 2^9=512, 2^15=32768 (#param is around 10% of selected rows)
    grid_hyper_tune <-
      expand.grid(
        hidden_units = list(c(64, 64), c(32, 32), c(32, 32, 32), c(16, 16, 16)),
        dropout = 1 / seq(4, 2, -1),
        activation = c("relu", "leaky_relu"),
        learn_rate = c(0.1, 0.05, 0.01)
      )
    dt_imputed <-
      dt_imputed %>%
      dplyr::slice_sample(prop = r_subsample)

    base_recipe <-
      recipes::recipe(
        dt_imputed
      ) %>%
      # do we want to normalize the predictors?
      # if so, an additional definition of truly continuous variables is needed
      # recipes::step_normalize(recipes::all_numeric_predictors()) %>%
      recipes::update_role(!!xvar) %>%
      recipes::update_role(!!yvar, new_role = "outcome") #%>%
    # recipes::step_normalize(!!yvar)

    if (is.null(folds)) {
      base_vfold <- rsample::vfold_cv(dt_imputed, v = vfold)
    } else {
      base_vfold <- folds
    }
    base_model <-
      parsnip::mlp(
        hidden_units = parsnip::tune(),
        dropout = parsnip::tune(),
        epochs = 1000L,
        activation = parsnip::tune(),
        learn_rate = parsnip::tune()
      ) %>%
      parsnip::set_engine("brulee", device = "cuda") %>%
      parsnip::set_mode("regression")

    wf_config <- tune::control_resamples(save_pred = TRUE, save_workflow = TRUE)

    base_wf <-
      workflows::workflow() %>%
      workflows::add_recipe(base_recipe) %>%
      workflows::add_model(base_model) %>%
      tune::tune_grid(
        resamples = base_vfold,
        grid = grid_hyper_tune,
        metrics = yardstick::metric_set(yardstick::rmse, yardstick::mape),
        control = wf_config
      )
    return(base_wf)
  }


# dt <- qs::qread("output/dt_feat_design_imputed_061024.qs")
# dtd <- dplyr::as_tibble(dt)
# dtfit <- fit_base_brulee(dtd, r_subsample = 0.3)


#' Base learner: Extreme gradient boosting (XGBoost)
#'
#' XGBoost model is fitted at the defined rate (`r_subsample`) of
#' the input dataset by grid search.
#' With proper settings, users can utilize graphics
#' processing units (GPU) to speed up the training process.
#' @keywords Baselearner
#' @note Spatiotemporal cross-validation strategy is not yet implemented.
#' @param dt_imputed The input data table to be used for fitting.
#' @param folds pre-generated rset object. If NULL, it should be
#'   numeric to be used in [rsample::vfold_cv].
#' @param r_subsample The proportion of rows to be sampled.
#' @param yvar The target variable.
#' @param xvar The predictor variables.
#' @param vfold The number of folds for cross-validation.
#' @param ... Additional arguments to be passed.
#'
#' @returns The fitted workflow.
#' @importFrom recipes recipe update_role
#' @importFrom dplyr `%>%`
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
#' @export
fit_base_xgb <-
  function(
    dt_imputed,
    folds = NULL,
    r_subsample = 0.3,
    yvar = "Arithmetic.Mean",
    xvar = seq(6, ncol(dt_imputed)),
    vfold = 5L,
    ...
  ) {
    grid_hyper_tune <-
      expand.grid(
        mtry = floor(c(0.02, 0.1, 0.02) * ncol(dt_imputed)),
        trees = seq(500, 3000, 500),
        learn_rate = c(0.05, 0.01, 0.005, 0.001)
      )
    dt_imputed <-
      dt_imputed %>%
      dplyr::slice_sample(prop = r_subsample)

    base_recipe <-
      recipes::recipe(
        dt_imputed
      ) %>%
      # recipes::step_normalize(recipes::all_numeric_predictors()) %>%
      recipes::update_role(tidyselect::all_of(xvar)) %>%
      recipes::update_role(tidyselect::all_of(yvar), new_role = "outcome")
    if (is.null(folds)) {
      base_vfold <- rsample::vfold_cv(dt_imputed, v = vfold)
    } else {
      base_vfold <- folds
    }
    base_model <-
      parsnip::boost_tree(
        mtry = parsnip::tune(),
        trees = parsnip::tune(),
        learn_rate = parsnip::tune()
      ) %>%
      parsnip::set_engine("xgboost", device = "cuda") %>%
      parsnip::set_mode("regression")

    wf_config <- tune::control_resamples(save_pred = TRUE, save_workflow = TRUE)

    base_wf <-
      workflows::workflow() %>%
      workflows::add_recipe(base_recipe) %>%
      workflows::add_model(base_model) %>%
      tune::tune_grid(
        resamples = base_vfold,
        grid = grid_hyper_tune,
        metrics = yardstick::metric_set(yardstick::rmse, yardstick::mape),
        control = wf_config
      )
    return(base_wf)

  }

# dt <- qs::qread("output/dt_feat_design_imputed_061024.qs")
# dtd <- dplyr::as_tibble(dt)
# dtfitx <- fit_base_xgb(dtd, xvar = names(dtd)[6:105], r_subsample = 0.3)


#' Base learner: Elastic net
#'
#' Elastic net model is fitted at the defined rate (`r_subsample`) of
#' the input dataset by grid search.
#' @keywords Baselearner
#' @note Spatiotemporal cross-validation strategy is not yet implemented.
#' @param dt_imputed The input data table to be used for fitting.
#' @param folds pre-generated rset object. If NULL, it should be
#'   numeric to be used in [rsample::vfold_cv].
#' @param r_subsample The proportion of rows to be sampled.
#' @param yvar The target variable.
#' @param xvar The predictor variables.
#' @param vfold The number of folds for cross-validation.
#' @param nthreads The number of threads to be used. Default is 16L.
#' @param ... Additional arguments to be passed.
#'
#' @returns The fitted workflow.
#' @importFrom future plan multicore multisession
#' @importFrom dplyr `%>%`
#' @importFrom recipes recipe update_role
#' @importFrom parsnip linear_reg set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
#' @export
fit_base_elnet <-
  function(
    dt_imputed,
    folds = NULL,
    r_subsample = 0.3,
    yvar = "Arithmetic.Mean",
    xvar = seq(6, ncol(dt_imputed)),
    vfold = 5L,
    nthreads = 16L,
    ...
  ) {
    grid_hyper_tune <-
      expand.grid(
        mixture = seq(0, 1, length.out = 21),
        penalty = 10 ^ seq(-3, 5)
      )
    dt_imputed <-
      dt_imputed %>%
      dplyr::slice_sample(prop = r_subsample)

    base_recipe <-
      recipes::recipe(
        dt_imputed
      ) %>%
      # recipes::step_normalize(recipes::all_numeric_predictors()) %>%
      recipes::update_role(tidyselect::all_of(xvar)) %>%
      recipes::update_role(tidyselect::all_of(yvar), new_role = "outcome")
    if (is.null(folds)) {
      base_vfold <- rsample::vfold_cv(dt_imputed, v = vfold)
    } else {
      base_vfold <- folds
    }
    base_model <-
      parsnip::linear_reg(
        mixture = parsnip::tune(),
        penalty = parsnip::tune()
      ) %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::set_mode("regression")

    wf_config <-
      tune::control_resamples(save_pred = TRUE, save_workflow = TRUE)

    future::plan(future::multicore, workers = nthreads)
    base_wf <-
      workflows::workflow() %>%
      workflows::add_recipe(base_recipe) %>%
      workflows::add_model(base_model) %>%
      tune::tune_grid(
        resamples = base_vfold,
        grid = grid_hyper_tune,
        metrics = yardstick::metric_set(yardstick::rmse, yardstick::mape),
        control = wf_config,
        parallel_over = "resamples"
      )
    future::plan(future::sequential)
    return(base_wf)

  }

#' Generate manual rset object from spatiotemporal cross-validation indices
#' @keywords Baselearner
#' @param cvindex integer length of nrow(data).
#' @param data data.frame.
#' @param ref_list List of custom reference indices. Default is NULL.
#'   if not NULL, it will be used as a reference instead of max(cvindex).
#' @param cv_mode character(1). Spatiotemporal cross-validation indexing
#'   method label.
#' @returns rset object of `rsample` package. A tibble with a list column of
#' training-test data.frames and a column of labels.
#' @author Insang Song
#' @importFrom rsample make_splits
#' @importFrom rsample manual_rset
#' @export
convert_cv_index_rset <-
  function(
    cvindex,
    data,
    ref_list = NULL,
    cv_mode = "spt"
  ) {
    if (length(cvindex) != nrow(data)) {
      stop("cvindex length should be equal to nrow(data).")
    }

    if (!is.null(ref_list)) {
      list_cvi <- ref_list
      len_cvi <- seq_along(list_cvi)
    } else {
      maxcvi <- max(cvindex)
      len_cvi <- seq_len(maxcvi)
      list_cvi <- split(len_cvi, len_cvi)
    }
    list_cvi_rows <-
      lapply(
        list_cvi,
        function(x) {
          list(analysis = which(!cvindex %in% x),
               assessment = which(cvindex %in% x))
        }
      )
    list_split_dfs <-
      lapply(
        list_cvi_rows,
        function(x) {
          rsample::make_splits(x = x, data = data)
        }
      )
    modename <- sprintf("cvfold_%s_%03d", cv_mode, len_cvi)
    rset_stcv <- rsample::manual_rset(list_split_dfs, modename)
    return(rset_stcv)
  }


#' Attach XY coordinates to a data frame
#'
#' This function attaches XY coordinates to a data frame based on a spatial
#' object containing the coordinates. It performs a left join operation to
#' match the coordinates with the corresponding locations in the data frame.
#' @keywords Utility
#' @param data_full The full data frame to which XY coordinates will
#'   be attached.
#' @param data_sf The spatial object containing the XY coordinates.
#' @param locs_id The column name in the spatial object that represents the
#'   location identifier.
#' @param time_id The column name in the data frame that represents the time
#'   identifier.
#'
#' @returns A data frame with the XY coordinates attached.
#'
#' @importFrom sf st_coordinates
#' @importFrom stats setNames
#' @importFrom collapse join
#' @export
attach_xy <-
  function(
    data_full,
    data_sf,
    locs_id = "site_id",
    time_id = "time"
  ) {
    data_sfd <- sf::st_coordinates(data_sf)
    data_sf <- data_sf[[locs_id]]
    data_sfd <- data.frame(site_id = data_sf, data.frame(data_sfd))
    data_sfd <- stats::setNames(data_sfd, c(locs_id, "lon", "lat"))

    data_full_lean <- data_full[, c(locs_id, time_id), with = FALSE]
    data_full_lean <-
      collapse::join(
        data_full_lean, data_sfd, on = locs_id, how = "left"
      )
    return(data_full_lean)
  }



#' Generate spatio-temporal cross-validation index with anticlust
#'
#' This function generates a spatio-temporal cross-validation index
#' based on the anticlust package. The function first calculates the
#' spatial clustering index using the balanced_clustering function as
#' default, and if `cv_pairs` is provided,
#' it generates rank-based pairs based on the proximity between
#' cluster centroids.
#' @keywords Baselearner
#' @param data data.table with X, Y, and time information.
#' @param target_cols character(3). Names of columns for X, Y, and time.
#'   Default is c("lon", "lat", "time").
#'   Order insensitive.
#' @param preprocessing character(1). Preprocessing method.
#'   * "none": no preprocessing.
#'   * "normalize": normalize the data.
#'   * "standardize": standardize the data.
#' @param cv_fold integer(1). Number of folds for cross-validation.
#'   default is 5L.
#' @param cv_pairs integer(1). Number of pairs for cross-validation.
#'   This value will be used to generate a rank-based pairs
#'   based on `target_cols` values.
#' @param pairing character(1) Pair selection method.
#'   * "1": search the nearest for each cluster then others
#'    are selected based on the rank.
#'   * "2": rank the pairwise distances directly
#' @param cv_mode character(1). Spatiotemporal cross-validation indexing
#' @note nrow(data) %% cv_fold should be 0.
#' @returns rsample::manual_rset() object.
#' @author Insang Song
#' @importFrom rsample manual_rset
#' @importFrom anticlust balanced_clustering
#' @importFrom dplyr group_by summarize across ungroup all_of
#' @export
generate_cv_index <-
  function(
    data,
    target_cols = c("lon", "lat", "time"),
    preprocessing = c("none", "normalize", "standardize"),
    cv_fold = 5L,
    cv_pairs = NULL,
    pairing = c("1", "2"),
    cv_mode = "spt"
  ) {
    if (length(target_cols) != 3) {
      stop("Please provide three target columns.")
    }
    data <- data[, target_cols, with = FALSE]
    data$time <- as.numeric(data$time)
    data_proc <-
      switch(
        preprocessing,
        none = data,
        normalize = (data + abs(apply(data, 2, min))) /
          (apply(data, 2, max) + abs(apply(data, 2, min))),
        standardize = collapse::fscale(data)
      )
    index_cv <- anticlust::balanced_clustering(data_proc, cv_fold)
    cv_index <- NULL
    ref_list <- NULL
    if (!is.null(cv_pairs)) {
      pairing <- match.arg(pairing)
      data_ex <- data_proc
      data_ex$cv_index <- index_cv
      data_exs <- data_ex |>
        dplyr::group_by(cv_index) |>
        dplyr::summarize(
          dplyr::across(dplyr::all_of(target_cols), ~mean(as.numeric(.x)))
        ) |>
        dplyr::ungroup()

      data_exs$cv_index <- NULL
      data_exm <- stats::dist(data_exs)
      data_exd <- as.vector(data_exm)
      data_exmfull <- as.matrix(data_exm)
      # index searching in dist matrix out of dist
      data_exd_colid <-
        unlist(Map(seq_len, seq_len(max(index_cv) - 1)))
      # rep(seq_len(max(index_cv) - 1), seq(max(index_cv) - 1, 1, -1))
      data_exd_rowid <- rep(seq(2, max(index_cv)), seq_len(max(index_cv) - 1))
      if (pairing == "2") {
        search_idx <- which(rank(-data_exd) <= cv_pairs)
      } else {
        # min rank element index per each cluster centroid
        search_each1 <-
          apply(data_exmfull, 1, \(x) which.min(replace(x, which.min(x), Inf)))
        # sort the index
        search_each1sort <-
          Map(c, seq_along(search_each1), search_each1)
        # keep the distinct pairs
        search_each1sort <-
          unique(Map(sort, search_each1sort))
        # return(list(data_exd_colid, data_exd_rowid, search_each1sort))
        search_idx_each1 <-
          which(
            Reduce(
              `|`,
              Map(
                \(x) data_exd_colid %in% x[1] & data_exd_rowid %in% x[2],
                search_each1sort
              )
            )
          )

        # replace the nearest pairs' distance to Inf
        search_idx_others <-
          which(rank(-replace(data_exd, search_idx_each1, Inf)) <= cv_pairs)
        # remove the nearest pairs
        # sort the distance of the remaining pairs
        search_idx_others <-
          search_idx_others[1:(cv_pairs - length(search_idx_each1))]
        search_idx <- c(search_idx_each1, search_idx_others)
      }
      ref_list <-
        Map(c, data_exd_rowid[search_idx], data_exd_colid[search_idx])
    }

    rset_cv <-
      convert_cv_index_rset(
        index_cv, data, ref_list = ref_list, cv_mode = cv_mode
      )
    return(rset_cv)
  }

#' Visualize the spatio-temporal cross-validation index
#' @keywords Baselearner
#' @param rsplit rsample::manual_rset() object.
#' @param angle numeric(1). Viewing angle of 3D plot.
#' @returns None. A plot will be generated.
#' @seealso [`generate_cv_index`]
#' @export
vis_rset <-
  function(rsplit, angle = 60) {
    nsplit <- nrow(rsplit)
    graphics::par(mfrow = c(ceiling(nsplit / 3), 3))
    for (i in seq_len(nsplit)) {
      cleared <- rsplit[i, 1][[1]][[1]]$data
      cleared$indx <- 0
      cleared$indx[rsplit[i, 1][[1]][[1]]$in_id] <- "In"
      cleared$indx[rsplit[i, 1][[1]][[1]]$out_id] <- "Out"
      cleared$indx <- factor(cleared$indx)
      cleared$time <- as.POSIXct(cleared$time)
      scatterplot3d::scatterplot3d(
        cleared$lon, cleared$lat, cleared$time,
        color = rev(as.integer(cleared$indx) + 1),
        cex.symbols = 0.02, pch = 19,
        angle = angle
      )
    }
  }
# nocov end

#' Get Divisors
#' @keywords Miscellaneous
#' @param x integer(1). A positive integer.
#' @returns A vector of divisors of x.
#' @export
divisor <-
  function(x) {
    xv <- seq_len(x)
    xv[which(x %% xv == 0)]
  }
