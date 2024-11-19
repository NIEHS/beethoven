
#' Check file status and download if necessary
#' @keywords Utility
#' @param path Path to qs file with all download specifications per
#'   dataset.
#' @param dataset_name character(1). Dataset name.
#' @param ... Arguments passed to `amadeus::download_data`
#' @importFrom amadeus download_data
#' @importFrom rlang inject
#' @return logical(1).
#' @export
feature_raw_download <-
  function(
    path = NULL,
    dataset_name = NULL,
    ...
  ) {
    if (!file.exists(path)) {
      stop("The path does not exist.")
    }
    if (!endsWith(path, ".qs")) {
      stop("The file should be in QS format.")
    }
    args_check <- loadargs(path, dataset = dataset_name)

    # run amadeus::download_data
    tryCatch(
      {
        if (is.list(args_check[[1]])) {
          for (i in seq_along(args_check)) {
            rlang::inject(
              amadeus::download_data(
                acknowledgement = TRUE,
                download = TRUE,
                !!!args_check[[i]]
              )
            )
          }
        } else {
          rlang::inject(
            amadeus::download_data(
              acknowledgement = TRUE,
              download = TRUE,
              !!!args_check
            )
          )
        }
        return(TRUE)
      },
      error = function(e) {
        stop(e)
      }
    )
  }


#' Set which years to be processed
#' @keywords Utility
#' @note This function is designed to define the temporal domain
#'   from the calculation period and the available years of raw data.
#' @param period character(2)/integer(2) of integer/character/Date.
#' @param available vector of integer or Date. Available years to be processed.
#' @return A vector of years to be processed.
#' @export
set_target_years <-
  function(
    period = NULL,
    available = NULL
  ) {
    if (is.character(period)) {
      if (all(nchar(period) == 4)) {
        period <- as.integer(period)
      } else {
        period <- as.integer(substr(period, 1, 4))
      }
    }
    assigned <-
      post_calc_year_expand(period[1], period[2], time_available = available)
    return(assigned)
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
#' [`amadeus::calculate_covariates`]
#' @param ... Arguments passed to `process_function` and `calc_function`
#' @return A data.table object.
#' @importFrom data.table rbindlist
#' @importFrom rlang inject
#' @importFrom amadeus process_covariates calculate_covariates
#' @export
calculate <-
  function(
    domain = NULL,
    domain_name = "year",
    nthreads = 1L,
    process_function = amadeus::process_covariates,
    calc_function = amadeus::calculate_covariates,
    ...
  ) {
    if (is.null(domain)) {
      domain <- c(1)
    }
    # split the domain, make years from the domain list
    # assuming that domain length is the same as the number of years
    domainlist <- split(domain, seq_along(domain))
    years_data <- seq_along(domain) + 2017

    # double twists: list_iteration is made to distinguish
    # cases where a single radius is accepted or ones have no radius
    # argument.
    res_calc <-
      mapply(
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
        domainlist, years_data,
        SIMPLIFY = FALSE
      )

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




#' Injects the calculate function with specified arguments.
#' @description
#' This function injects the calculate function with the specified arguments,
#' allowing for dynamic customization of the function's behavior.
#' @keywords Calculation
#' @param covariate character(1). The name of the covariate to be calculated.
#' @param locs The locations to be used in the calculation.
#' @param injection Additional arguments to be injected into
#'   the calculate function.
#' @return The result of the calculate function with the injected arguments.
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
#' @keywords Calculation
#' @note Soon to be deprecated per dropping future dependency.
#' @param locs A data frame containing the locations for which MODIS
#'   features need to be calculated.
#' @param injection **List** of dditional parameters to be passed to the
#'   `calculate_modis_par` function.
#' @return MODIS/VIIRS feature data.frame.
#' @seealso [`amadeus::calculate_modis_daily`], [`amadeus::calculate_modis_par`]
#' @importFrom rlang inject
#' @examples
#' \dontrun{
#' files <-
#'   c(
#'     "/downloads/modis/mod06/MOD06_L2.A2022001.0000.061.2022001160000.hdf",
#'    "/downloads/modis/mod06/MOD06_L2.A2022001.0005.061.2022001160000.hdf"
#'   )
#' my_locs <- data.frame(site_id = 1:2, lon = c(-88, -87), lat = c(35, 35))
#' my_locs <- sf::st_as_sf(my_locs, coords = c("lon", "lat"))
#' inject_modis_par(
#'   locs = my_locs,
#'   injection = list(path = files, subdataset = "Cloud_Fraction_Day",
#'      name_covariates = "MOD_CLCVD_0_", nthreads = 2L,
#'      preprocess = amadeus::process_modis_swath, radius = c(1000)))
#' }
#' @export
inject_modis_par <- function(locs, injection) {
  rlang::inject(
    amadeus::calculate_modis_par(
      locs = locs,
      locs_id = "site_id",
      !!!injection
    )
  )
}

#' Injects arguments into MODIS/VIIRS data processing function
#' @keywords Calculation
#' @param locs A data frame containing the locations for which MODIS
#'   features need to be calculated.
#' @param injection **List** of dditional parameters to be passed to the
#'   `calculate_modis_par` function.
#' @return MODIS/VIIRS feature data.frame.
#' @importFrom rlang inject
#' @examples
#' \dontrun{
#' files <-
#'   c(
#'     "/downloads/modis/mod06/MOD06_L2.A2022001.0000.061.2022001160000.hdf",
#'    "/downloads/modis/mod06/MOD06_L2.A2022001.0005.061.2022001160000.hdf"
#'   )
#' my_locs <- data.frame(site_id = 1:2, lon = c(-88, -87), lat = c(35, 35))
#' my_locs <- sf::st_as_sf(my_locs, coords = c("lon", "lat"))
#' inject_modis(
#'   locs = my_locs,
#'   injection = list(path = files, subdataset = "Cloud_Fraction_Day",
#'      name_covariates = "MOD_CLCVD_0_",
#'      preprocess = amadeus::process_modis_swath, radius = c(1000)))
#' }
#' @export
inject_modis <- function(locs, injection) {
  rlang::inject(
    calculate_modis(
      locs = locs,
      locs_id = "site_id",
      !!!injection
    )
  )
}


#' Injects geographic information into a data frame
#' @description
#' This function injects geographic information into a data frame using
#'   the `calc_geos_strict` function. The injected information includes
#'   latitude and longitude coordinates based on the specified locations,
#'   a location ID column, a window range, and a snapping option.
#' @keywords Calculation
#' @param locs A data frame containing the locations for which
#'   geographic information needs to be injected.
#' @param injection A list of additional arguments to be passed to
#'   the `calc_geos_strict` function.
#' @param ... Placeholders
#' @return A modified data frame with injected geographic information.
#' @export
inject_geos <- function(locs, injection, ...) {
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
#' @description
#' This function injects GMTED (Global Multi-resolution Terrain Elevation Data)
#'  into specified locations. It calculates the GMTED values for each
#'  location within different radii and returns the merged results.
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
#' @return A data frame containing the merged results of GMTED data
#'   for each location within different radii.
#' @importFrom rlang inject
#' @export
inject_gmted <- function(locs, variable, radii, injection, nthreads = 4L) {

  radii_list <- split(radii, seq_along(radii))

  radii_rep <-
    lapply(
      radii_list,
      function(r) {
        rlang::inject(
          beethoven::calc_gmted_direct(
            locs = locs,
            locs_id = "site_id",
            radius = r,
            variable = c(variable, "7.5 arc-seconds"),
            !!!injection
          )
        )
      },
      future.seed = TRUE
    )

  radii_rep <- lapply(radii_rep, function(x) as.data.frame(x))
  radii_join <- beethoven::reduce_merge(radii_rep, "site_id")
  return(radii_join)
}


#' Reduce and merge a list of data tables
#' @description
#' This function takes a list of data tables and merges them together
#'  using the specified columns. It uses the `merge.data.table` function
#'  from the `data.table` package to perform the merge.
#' @param list_in A list of data tables to be merged.
#' @param by The columns to merge the data tables on.
#'   If `NULL`, the function will automatically detect the common column names.
#' @param all.x logical(1). Keeping all rows from the first input.
#' @param all.y logical(1). Keeping all rows from the second input.
#' @return A merged data table.
#' @keywords Post-calculation
#' @examples
#' \dontrun{
#' # Create example data tables
#' dt1 <- data.table(a = 1:3, b = 4:6)
#' dt2 <- data.table(a = 2:4, c = 7:9)
#' dt3 <- data.table(a = 3:5, d = 10:12)
#'
#' # Merge the data tables
#' reduce_merge(list(dt1, dt2, dt3), by = "a")
#' }
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


#' Injects the calculate function with matched arguments.
#' @keywords Calculation
#' @param f function.
#' @param args List of arguments that are attempted to be injected into `f`.
#' @return Injected function evaluation.
#' @export
#' @importFrom rlang inject
inject_match <- function(f, args) {
  f_args <- formals(f)

  # Find the matching arguments
  matching_args <- intersect(names(args), names(f_args))

  # Inject the matching arguments
  rlang::inject(f(!!!args[matching_args]))

}


#' Inject arguments into NLCD calculation function for branching
#' @keywords Calculation
#' @param year An integer specifying the year to calculate NLCD data for.
#' @param radius An integer specifying the radius for the NLCD calculation.
#' @param ... Additional arguments to be passed to the NLCD calculation
#'  function.
#' @return data.frame object.
#' @export
inject_nlcd <-
  function(
    year = 2019,
    radius = 1000,
    ...
  ) {
    args_ext <- list(...)
    args_ext <- c(args_ext, list(year = year, radius = radius))
    inject_match(amadeus::calculate_nlcd, args_ext)
    inject_match(amadeus::calculate_nlcd, args_ext)
  }
