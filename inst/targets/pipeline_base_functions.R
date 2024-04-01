## pipeline base functions

#' Running commands with a punchcard
#' @param varname variable name to call
#' @param file Path to the punchcard
#' @param ... Arguments passed to the command
#' @returns Depending on the specification in the punchcard.
#' @examples
#' meta_run(varname = "root_absolute")
#' meta_run(varname = "root_relative")
#' meta_run(varname = "y2018")
#' meta_run(varname = "dir_input_modis_mod11")
meta_run <-
  function(
    varname = NULL,
    file = file.path("./tools/pipeline/punchcard.csv"),
    ...
  ) {
    metaspec <- utils::read.csv(file)
    if (varname == "root_absolute") {
      getwd()
    } else {
      spec <- metaspec[metaspec$varname == varname, ]
      foo_run <- get(spec$command)
      foo_run(spec$value, ...)
    }
  }

mr <- meta_run
mr("nei_year_sequence", split = "|", fixed = TRUE)[[1]]


#' Set resource management for SLURM
#' @param template_file SLURM job submission shell template path.
#' @param partition character(1). Name of partition. Default is `"geo"`
#' @param ncpus integer(1). Number of CPU cores assigned to each task.
#' @param ntasks integer(1). Number of tasks to submit.
#' @param memory integer(1). Specifically odds to 2*x GB.
#' @param user_email character(1). User email address.
#' @param error_log character(1). Error log file name.
#' @notes This function is designed to be used with `tar_resources`.
#' Suggested number of `ncpus` is more than 1 for typical multicore R tasks.
#' @returns A list of resources for `tar_resources`
#' @author Insang Song
set_slurm_resource <-
  function(
    template_file = "tools/pipeline/template_slurm.tmpl",
    partition = "geo",
    ncpus = 2L,
    ntasks = 2L,
    memory = 8,
    user_email = meta_run("slurm_user_email"),
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
              # template = template_file,
              ntasks = ntasks,
              ncpus = ncpus,
              memory = memory,
              error.file = error_log
            )
        )
      )
    )
  }


#' Read AQS data
#' @param fun_aqs function to import AQS data.
#' Default is `amadeus::process_aqs`
#' @param ... Passed arguments to `fun_aqs`
#' @returns Depending on `fun_aqs` specification.
read_locs <-
  function(
    fun_aqs = amadeus::process_aqs,
    ...
  ) {
    fun_aqs(...)
  }


#' Filter monitors with the minimum POC value
#' @param path data.frame/tbl_df/data.table
#' @param site_spt Space-time site data.
#' @param locs_id character(1). Name of site id (not monitor id)
#' @param poc_name character(1). Name of column containing POC values.
#' @param date_start character(1).
#' @param date_end character(1).
#' @param return_format character(1). One of `"sf"` or `"terra"`
#' @author Insang Song
#' @returns a data.table object
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom rlang sym
#' @export
get_aqs_data <-
  function(
    path = file.path(mr("dir_output"), mr("file_aqs_pm")),
    site_spt = NULL,
    locs_id = mr("pointid"),
    time_id = mr("timeid"),
    poc_name = "POC",
    date_start = "2018-01-01",
    date_end = "2022-12-31"
  ) {
    #nocov start
    if (!is.character(locs_id)) {
      stop("locs_id should be character.\n")
    }
    if (!is.character(poc_name)) {
      stop("poc_name should be character.\n")
    }
    # aqs_prep <-
    #   amadeus::process_aqs(
    #     path = path,
    #     date = NULL,
    #     return_format = return_format
    #   )
    input_df <- lapply(path, read.csv) |> data.table::rbindlist()
    input_df <- input_df[,
      list(
        pm2.5 = Arithmetic.Mean,
        site_id =
        sprintf("%02d%03d%04d%05d",
          State.Code, County.Code, Site.Num, Parameter.Code),
        time = as.Date(Date.Local),
        POC = POC
      )]
    poc_filtered <- input_df |>
      dplyr::group_by(!!rlang::sym(locs_id)) |>
      dplyr::filter(!!rlang::sym(poc_name) == min(!!rlang::sym(poc_name))) |>
      dplyr::ungroup() |>
      data.table::data.table()

    poc_res <-
      merge(poc_filtered,
        as.data.frame(site_spt),
        by = c(locs_id, time_id)
      )
    return(poc_res)
    #nocov end
  }

#' Join dependent variable (y) and covariates (x)
#' @param df_pm PM2.5 data.frame
#' @param df_covar covariates data.frame
#' @param locs_id location identifier
#' @param time_id time identifier
#' @returns data.frame
#' @author Insang Song
join_yx <-
  function(
    df_pm,
    df_covar,
    locs_id = mr("pointid"),
    time_id = mr("timeid")
  ) {
    # full join
    merge(df_pm, df_covar, by = c(locs_id, time_id),
      all.x = TRUE, all.y = TRUE)
  }



# TODO: is it possible to download missing files only?
#' Check file status and download if necessary
#' @param file_status Output of `check_file_status`
#' @param path 
#' @param dname Dataset name. See [`amadeus::download_data`] for details.
#' @returns logical(1).
fastdown <-
  function(
    file_status = NULL,
    path = NULL,
    dname = NULL,
    ...
  ) {
    # if no files exist in path (comparing with static list)
    # run amadeus::download_data
    tryCatch(
      {
        amadeus::download_data(dname, ...)
        return(TRUE)
      },
      error = function(e) {
        print(e)
        return(FALSE)
      }
    )
  }

#' Load county sf object
#' @returns sf object
#' @importFrom tigris counties
load_county <- function() {
  options(tigris_use_cache = TRUE)
  cnty <- tigris::counties(year = 2020)
  cnty <-
    cnty[!cnty$STATEFP %in% c("02", "15", "60", "66", "68", "69", "72", "78"), ]
  return(cnty)
}


# calculate (no year is concerned)
#' Temporally marginal calculation
#' @note As `amadeus::calc_covariates` suggests,
#' covariate data are managed by raw datasets.
#' @param status File status. Output of `check_file_status`
#' @param outpath character(1). Full file path of calculated covariates.
#' Should end with `"rds"`.
#' @param process_function Raw data processor. Default is
#' [`amadeus::process_covariates`]
#' @param calc_function Covariate calculator. Default is
#' [`amadeus::calc_covariates`]
#' @param ... Arguments passed to `calc_function`
#' @returns Nothing. It will automatically save xz-compressed
#' RDS file to `outpath`
calculate_single <-
  function(
    # status = NULL,
    # outpath = NULL,
    process_function = amadeus::process_covariates,
    calc_function = amadeus::calc_covariates,
    ...
  ) {
    # if (!status) {
      prep_calc <-
        try(
          rlang::inject(
            process_function(
              !!!list(...)
            )
          )
        )
      arg_ext <- list(...)
      arg_ext$from <- prep_calc
      
      res_calc <-
        try(
          rlang::inject(
            calc_function(
              !!!arg_ext
            )
          )
        )
      if (inherits(res_calc, "try-error")) {
        stop("Results do not match expectations.")
      }
      return(res_calc)
      # saveRDS(res_calc, file = outpath, compress = "xz")
    # }
  }

# calculate over a list
#' Spatiotemporal covariate calculation
#' @param domain vector of integer/character/Date.
#' Depending on temporal resolution of raw datasets.
#' @param process_function Raw data processor. Default is
#' [`amadeus::process_covariates`]
#' @param calc_function Function to calculate covariates.
#' [`amadeus::calc_covariates`]
#' @param ... Arguments passed to `process_function` and `calc_function`
#' @returns Nothing. RDS file is saved.
calculate_multi <-
  function(
    # status = NULL,
    # outpath = NULL,
    domain = NULL,
    process_function = amadeus::process_covariates,
    calc_function = amadeus::calc_covariates,
    ...
  ) {
    # if (!status) {
      domainlist <- split(domain, seq_along(domain))
      res_calc <-
        try(
          lapply(
            domainlist,
            function(el) {
              from_in <-
                rlang::inject(
                  process_function(year = el, !!!list(...))
                )
              rlang::inject(
                calc_function(
                  from = from_in,
                  !!!list(...)
                )
              )
            }
          )
        )
      if (inherits(res_calc, "try-error")) {
        cat(paste0(attr(res_calc, "condition")$message, "\n"))
        stop("Results do not match expectations.")
      }
      res_calc <- lapply(res_calc, function(x) as.data.frame(x))
      res_calc <- data.table::rbindlist(res_calc, fill = TRUE)
      return(res_calc)
      # saveRDS(res_calc, file = outpath, compress = "xz")
    # }

  }


# sspat <- readRDS("~/sites_unique.rds")
# kk <- calculate_multi(
#         # sequence: could be refered from dates
#         domain = 2018,#c(2018, 2019, 2020, 2021, 2022),
#         path = mr("dir_input_tri"),
#         covariate = "tri",
#         locs = sspat,
#         locs_id = mr("pointid")
#       )

# rr <-
# read_locs(
#         path = list.files(
#           path = mr("dir_input_aqs"),
#           pattern = "daily_88101_[0-9]{4}.csv",
#           full.names = TRUE),
#         date = NULL,
#         return_format = "sf"
#       )

#' Merge input data.frame objects
#' @param by character. Joining keys. See [`merge`] for details.
#' @param time logical(1). Whether or not include time identifier.
#' Set this `TRUE` will supersede `by` value by appending time identifier.
#' @param ... data.frame objects to merge
#' @returns data.frame
combine <-
  function(
    by = c(meta_run("pointid")),
    time = FALSE,
    ...
  ) {
    ellipsis <- list(...)
    if (time) {
      by <- c(meta_run("pointid"), meta_run("timeid"))
      ellipsis_clean <-
        lapply(ellipsis,
          function(x) {
            x <- as.data.frame(x)
            col_coords <- grep("(lon|lat)", names(x))
            if (length(col_coords) > 0 && !is.null(col_coords)) {
              x <- x[, -col_coords]
            }
            x$time <- as.character(x$time)
            return(x)
          })
    } else {
      ellipsis_clean <- ellipsis
    }
    joined <-
      Reduce(function(x, y) {
        dplyr::left_join(x, y, by = by)
      }, ellipsis_clean)
    return(joined)
  }


#' Change time column name
#' @param df data.frame
#' @param candidates character. Candidate column names.
#' @param replace character. New column name.
#' @returns data.frame
unify_timecols <-
  function(
    df,
    candidates = c("date"),
    replace = mr("timeid")
  ) {
    if (sum(names(df) %in% candidates) > 1) {
      stop("More than a candidate is detected in the input.")
    }
    names(df)[names(df) %in% candidates] <- replace
    return(df)
  }


#' Join a data.frame with a year-only date column to that with a full date column
#' @description The full date column will be converted to a year column
#' as a new column, then the data.frame with the year-only column will
#' be joined.
#' @param df_year data.frame with a year-only date column
#' @param df_date data.frame with a full date column
#' @param field_year character(1). Year column in `df_year`
#' @param field_date character(1). Date column in `df_date`
#' @param spid character(1). Name of the unique location identifier field.
#' @importFrom methods is
#' @importFrom dplyr full_join
#' @returns data.frame
join_yeardate <-
  function(
    df_year,
    df_date,
    field_year = "time",
    field_date = "time",
    spid = meta_run("pointid")
  ) {
    if (!inherits(df_year, "data.frame") && !inherits(df_date, "data.frame")) {
      stop("Both inputs should be data.frame.")
    }
    year <- NULL
    names(df_year)[names(df_year) %in% field_year] <- "year"
    df_date$year <- as.integer(format(as.Date(df_date[[field_date]]), "%Y"))
    df_joined <- dplyr::left_join(df_date, df_year, by = c(spid, "year")) |>
      dplyr::select(-year)
    return(df_joined)
  }



### WARNING  THIS WILL NOT WORK PROPERLY ####
#' Merge spatial and spatiotemporal covariate data
#' @param locs Location. e.g., AQS sites.
#' @param locs_id character(1). Location identifier.
#' @param time_id character(1). Location identifier.
#' @param target_years integer. Used to dummify nominal year.
#' @param df_sp data.frame. Spatial-only covariates.
#' @param df_spt data.frame. Spatiotemporal covariates.
#' @note This version assumes the time_id contains Date-like strings.
#' Year-only covariate processing submodule should be added.
combine_final <-
  function(
    locs,
    locs_id,
    time_id,
    target_years = seq(2018, 2022),
    df_sp,
    df_spt
  ) {
    locs <- as.data.frame(locs)
    locs_combined <-
      merge(locs, df_sp, by = c(locs_id))
    locs_combined <-
      merge(
        locs_combined, df_spt,
        by = c(locs_id, time_id)
      )
    locs_combined <-
      amadeus::calc_temporal_dummies(
        locs = locs_combined,
        locs_id = locs_id,
        year = target_years
      )
    return(locs_combined)
  }


#' Read paths from a directory with a specific file extension
#' @param path The directory path from which to read the paths.
#' @param extension The file extension to match. Defaults to ".hdf".
#' @param target_dates A character vector of length 2 containing the start and end dates.
#' @returns A character vector containing the full paths of the matching files.
#'
#' @examples
#' # Read paths from a directory with default extension
#' read_paths("/path/to/directory")
#'
#' # Read paths from a directory with custom extension
#' read_paths("/path/to/directory", ".txt")
#'
#' @export
read_paths <- function(path, extension = ".hdf", target_dates = c("2020-01-01", "2020-01-15"), julian = FALSE) {
  flist <-
    list.files(
      path = path,
      pattern = sprintf("%s$", extension),
      full.names = TRUE,
      recursive = TRUE
    )
  if (!missing(target_dates)) {
    dateseq <- seq(as.Date(target_dates[1]), as.Date(target_dates[2]), by = "day")
    dateseq <- if (julian) format(dateseq, "%Y%j") else format(dateseq, "%Y%m%d")
    dateseq <- sprintf("A(%s)", paste(dateseq, collapse = "|"))
    flist <- grep(dateseq, flist, value = TRUE)
  }
  return(flist)
}



#' Search package functions
#' @param package character(1). Package name.
#' @param search character(1). Search term.
#' @returns A character vector containing the matching function names.
#' @examples
#' # Search for functions in the `amadeus` package
#' search_function("amadeus", "process_")
search_function <- function(package, search){
  library(package, character.only = TRUE)
  grep(search, ls(sprintf("package:%s", package)), value = TRUE)
}

#' Get data.frame of function parameters
#' @param functions character. Vector of function names.
#' @returns A data.frame containing the parameters of the functions.
df_params <- function(functions) {
  params <- lapply(functions, function(x) {
    args <- dplyr::as_tibble(lapply(as.list(formals(get(x))), \(p) list(p)), .name_repair = "minimal")
    return(args)
  })
  paramsdf <- Reduce(dplyr::bind_rows, params)
  return(paramsdf)
}

# sched <- search_function("amadeus", "process_")
# schec <- search_function("amadeus", "calc_")
# df_params(sched[-c(1, 2, 3, 4, 5, 6, 8, 11, 14, 15, 17, 18, 19, 20, 21, 25)])
# df_params(schec[-c(1, 16)]) |> colnames()





#' Process atmospheric composition data by chunks (v2)
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

    summary_byvar <- function(x = data_variables, fs) {
      #do.call(c, 
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
#' @description
#' Returning a single `SpatRasterDataset` object.
#' Removed `tapp` for performance; impose a strict assumption that
#' there are no missing values
#' @param date character(2). length of 10. Format "YYYY-MM-DD".
#' @param path character(1). Directory with downloaded netCDF (.nc4) files. or
#' netCDF file paths.
#' @param ... Arguments passed to [`terra::rast`].
#' @note
#' Layer names of the returned `SpatRaster` object contain the variable,
#' pressure level, date
#' Reference duration: 1 day summary, all layers: 106 seconds
#' @author Mitchell Manware, Insang Song
#' @return a `SpatRaster` object;
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra crs
#' @importFrom terra subset
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
    collection <- regmatches(
      data_paths[1],
      # the pattern accommodates 3-4 characters for the variable name,
      # 3-4 alphanumerics for the temporal resolution,
      # 8-9 alphanumerics for the output dimensions
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
    if (length(unique(filename_date)) > 1) {
      message(
        "Dates are not supposed to be different. Put in the same date's files."
      )
    }

    # to export locs (pointers are not exportable)
    locs <- sf::st_as_sf(locs)

    # split filename date every 10 days
    filename_date <- as.Date(filename_date, format = "%Y%m%d")
    filename_date_cl <- as.integer(as.factor(filename_date))

    future_inserted <- split(data_paths, filename_date_cl)
    other_args <- list(...)
    data_variables <- names(terra::rast(data_paths[1]))

    summary_byvar <- function(x = data_variables, fs) {
      rast_in <- rlang::inject(terra::rast(fs, !!!other_args))
      # strongly assume that we take the single day. no need to filter dates
      sds_proc <-
        lapply(
        x,
        function(v) {
          rast_inidx <- grep(v, x)
          #rast_in <- mean(rast_in[[rast_inidx]])
          rast_summary <- terra::mean(rast_in[[rast_inidx]])
          rtin <- as.Date(terra::time(rast_in))
          rtin_u <- unique(rtin)
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

    # summary by 10 days
    # FIXME: .x is an hourly data -> to daily data for proper use
    rast_10d_summary <-
      lapply(
        future_inserted,
        function(x) summary_byvar(fs = x)
      )
    rast_10d_summary <- data.table::rbindlist(rast_10d_summary)
    # extract

    return(rast_10d_summary)

  }


# system.time(
#   cgeo <- calc_geos_strict(path = "input/geos/chm_tavg_1hr_g1440x721_v1",
#             date = c("2018-05-17", "2018-05-17"),
#             locs = terra::vect(data.frame(site_id = 1, lon = -90, lat = 40)),
#             locs_id = "site_id",
#             win = c(-126, -62, 22, 52),
#             snap = "out")
# )


## base & meta learner fitting

fit_base <-
  function(

  ) {

  }


predict_base <-
  function(
    fitted,
    targetdf
  ) {

  }



predict_meta <-
  function(
    metalearner = NULL,
    targetdf = NULL,
    threads = NULL
  ) {
    beethoven::meta_predict(
      metalearner,
      targetdf,
      nthreads = threads
    )
  }

export_res <-
  function(

  ) {

  }


#' Check file status with a static list
#' @concept deprecated
#' @description A static list refers to a fixed state of
#' the list of files at a certain time point. Users should update the static
#' list if needed. The static list could reduce the risk of rerunning the
#' entire pipeline due to a trivial or accidental change in files
#' @note Directory representations should match in `dir` and `static_list`.
#' Both should be absolute or relative, but should not be crossed.
#' @param dir Directory path to search files
#' @param extension File extension according to raw data
#' @param static_list character. A static list of files.
#' Its elements should match `extension` with their extensions.
#' @returns Length of the number of files with `extension` in `dir`.
check_file_status <-
  function(
    dir,
    extension,
    static_list
  ) {
    your_list <-
      list.files(
        path = dir,
        pattern = extension,
        recursive = TRUE,
        full.names = TRUE
      )
    all(your_list %in% static_list)
  }


# calc_geos_strictlite <-
#   function(path = NULL,
#            date = c("2018-01-01", "2018-01-01"),
#            locs = NULL,
#            locs_id = NULL,
#            ...) {
#     #### directory setup
#     if (length(path) == 1) {
#       if (dir.exists(path)) {
#         path <- amadeus::download_sanitize_path(path)
#         paths <- list.files(
#           path,
#           pattern = "GEOS-CF.v01.rpl",
#           full.names = TRUE
#         )
#         paths <- paths[grep(
#           ".nc4",
#           paths
#         )]
#       }
#     } else {
#       paths <- path
#     }
#     #### check for variable
#     amadeus::check_for_null_parameters(mget(ls()))
#     #### identify file paths
#     #### identify dates based on user input
#     dates_of_interest <- amadeus::generate_date_sequence(
#       date[1],
#       date[2],
#       sub_hyphen = TRUE
#     )
#     #### subset file paths to only dates of interest
#     data_paths <- unique(
#       grep(
#         paste(
#           dates_of_interest,
#           collapse = "|"
#         ),
#         paths,
#         value = TRUE
#       )
#     )
#     print(data_paths)
#     #### identify collection
#     collection <- regmatches(
#       data_paths[1],
#       regexpr(
#         "GEOS-CF.v01.rpl.(aqc|chm)_[[:alpha:]]{3,4}_[[:alnum:]]{3,4}_[[:alnum:]]{8,9}_v[1-9]",
#         data_paths[1]
#       )
#     )
#     cat(
#       paste0(
#         "Identified collection ",
#         collection,
#         ".\n"
#       )
#     )
#     if (length(unique(collection)) > 1) {
#       warning(
#         "Multiple collections detected. Returning data for all collections.\n"
#       )
#     }

#     filename_date <- sort(regmatches(
#       data_paths,
#       regexpr(
#         "20[0-9]{2}(0[1-9]|1[0-2])([0-2][0-9]|3[0-1])",
#         data_paths
#       )
#     ))
#     if (any(table(filename_date) < 24)) {
#       warning(
#         "Some dates include less than 24 hours. Check the downloaded files."
#       )
#     }
#     if (length(unique(filename_date)) > 1) {
#       message(
#         "Dates are not supposed to be different. Put in the same date's files."
#       )
#     }

#     # split filename date every 10 days
#     filename_date <- as.Date(filename_date, format = "%Y%m%d")
#     filename_date_cl <- as.integer(as.factor(filename_date))

#     future_inserted <- split(data_paths, filename_date_cl)
#     other_args <- list(...)
#     data_variables <- names(terra::rast(data_paths[1]))

#     summary_byvar <- function(x = data_variables, fs) {
#       # rast_in <- rlang::inject(terra::rast(fs, !!!other_args))
#       sds_proc <-
#         lapply(
#         x,
#         function(v) {
#           rast_inidx <- grep(v, data_variables)
#           rast_in <- rlang::inject(terra::rast(fs, subds = rast_inidx, !!!other_args))
#           rtin <- as.Date(terra::time(rast_in))
#           rtin_u <- unique(rtin)
#           rast_summary <- vector("list", length = length(unique(rtin)))
#           for (d in seq_along(rtin_u)) {
#             rast_d <- rast_in[[rtin == rtin_u[d]]]
#             rast_summary[[d]] <- mean(rast_d)
#           }
#           rast_summary <- do.call(c, rast_summary)
#           # rast_summary <- terra::tapp(rast_in, index = "days", fun = "mean")
#           names(rast_summary) <-
#             paste0(
#               rep(gsub("_lev=.*", "", v), terra::nlyr(rast_summary))
#               #, "_", terra::time(rast_summary)
#             )
#           terra::set.crs(rast_summary, "EPSG:4326")
#           return(rast_summary)
#         }
#       )
#       sds_proc <- terra::sds(sds_proc)

#       rast_ext <- terra::extract(sds_proc, locs)
#       rast_ext <- lapply(rast_ext,
#         function(df) {
#           df$ID <- unlist(locs[[locs_id]])
#           return(df)
#         }
#       )
#       rast_ext <-
#         Reduce(function(dfa, dfb) dplyr::full_join(dfa, dfb, by = "ID"),
#           rast_ext
#         )
#       # NOTE: assuming that the date is the same for all layers
#       rast_ext$time <- date[1]

#       return(rast_ext)

#     }

#     # summary by 10 days
#     # FIXME: .x is an hourly data -> to daily data for proper use
#     rast_10d_summary <-
#       purrr::map(
#         .x = future_inserted,
#         .f = ~summary_byvar(fs = .x)
#       )
#     rast_10d_summary <- data.table::rbindlist(rast_10d_summary)
#     # extract

#     return(rast_10d_summary)

#   }

# system.time(
#   cgeo2 <- calc_geos_strictlite(path = "input/geos/chm_tavg_1hr_g1440x721_v1",
#             date = c("2018-05-17", "2018-05-17"),
#             locs = terra::vect(data.frame(site_id = 1, lon = -90, lat = 40)),
#             locs_id = "site_id",
#             win = c(-126, -62, 22, 52),
#             snap = "out")
# )
# 110.5 (strict)
# 119.287 (strictlite)