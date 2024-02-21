## pipeline base functions

#' Running commands with a punchcard
#' @param varname variable name to call
#' @param file Path to the punchcard
#' @returns Depending on the specification in the punchcard.
meta_run <-
  function(
    varname = NULL,
    file = file.path("./tools/pipeline/punchcard.csv")
  ) {
    metaspec <- utils::read.csv(file)
    if (varname == "root_absolute") {
      getwd()
    } else {
      spec <- metaspec[metaspec$varname == varname, ]
      get(spec$command)(spec$value)
    }
  }

mr <- meta_run

meta_run(varname = "root_absolute")
meta_run(varname = "root_relative")
meta_run(varname = "y2018")
meta_run(varname = "dir_input_modis_mod11")


#' Read AQS data
#' @param fun_aqs function to import AQS data.
#' Default is `get("import_aqs")`
#' @param ... Passed arguments to `fun_aqs`
#' @returns Depending on `fun_aqs` specification.
read_locs <-
  function(
    fun_aqs = get("import_aqs"),
    ...
  ) {
    fun_aqs(...)
  }


#' Filter monitors with the minimum POC value
#'
#' @param input_df data.frame/tbl_df/data.table
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
#' @importFrom rlang sym
#' @export
get_aqs_data <-
  function(
    path = file.path(mr("dir_output"), mr("file_aqs_pm")),
    locs_id = mr("pointid"),
    time_id = mr("timeid"),
    poc_name = "POC",
    date_start = "2018-01-01",
    date_end = "2022-12-31",
    return_format = "terra"
  ) {
    #nocov start
    if (!is(input_df, "data.frame")) {
      stop("input_df should be data.frame/tbl_df/data.table.\n")
    }
    if (!is.character(locs_id)) {
      stop("locs_id should be character.\n")
    }
    if (!is.character(poc_name)) {
      stop("poc_name should be character.\n")
    }
    aqs_prep <-
      amadeus::process_aqs(
        path = path,
        include_time = TRUE,
        date_start = date_start,
        date_end = date_end,
        return_format = return_format
      )
    input_df <- readRDS(path)

    poc_filtered <- input_df |>
      dplyr::group_by(!!rlang::sym(locs_id), !!rlang::sym(time_id)) |>
      dplyr::filter(!!rlang::sym(poc_name) == min(!!rlang::sym(poc_name))) |>
      dplyr::ungroup() |>
      data.table::data.table()

    poc_res <- merge(poc_filtered, aqs_prep, by = c(locs_id, time_id))
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
    merge(df_pm, df_covar, by = c(locs_id, time_id))
  }

#' Check file status with a static list
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


# calculate (no year is concerned)
#' Temporally marginal calculation
#' @note As `amadeus::calc_covariates` suggests,
#' covariate data are managed by raw datasets.
#' @param status File status. Output of `check_file_status`
#' @param outpath character(1). Full file path of calculated covariates.
#' Should end with `"rds"`.
#' @param calc_function Covariate calculator. Default is
#' [`amadeus::calc_covariates`]
#' @param ... Arguments passed to `calc_function`
#' @returns Nothing. It will automatically save xz-compressed
#' RDS file to `outpath`
calculate_single <-
  function(
    status = NULL,
    outpath = NULL,
    calc_function = amadeus::calc_covariates,
    ...
  ) {
    if (!status) {
      res_calc <-
        try(
          calc_function(
            ...
          )
        )
      if (inherits(res_calc, "try-error")) {
        stop("Results do not match expectations.")
      }
      saveRDS(res_calc, file = outpath, compress = "xz")
    }
  }

# calculate over a list
#' Spatiotemporal covariate calculation
#' @param status File status. Output of `check_file_status`
#' @param outpath character(1). Full file path of calculated covariates.
#' Should end with `"rds"`
#' @param domain vector of integer/character/Date.
#' Depending on temporal resolution of raw datasets.
#' @param process_function Function to prepare raw datasets.
#' [`amadeus::process_raw`]
#' @param calc_function Function to calculate covariates.
#' [`amadeus::calc_covariates`]
#' @param ... Arguments passed to `process_function` and `calc_function`
#' @returns Nothing. RDS file is saved.
calculate_multi <-
  function(
    status = NULL,
    outpath = NULL,
    domain = NULL,
    process_function = amadeus::process_raw,
    calc_function = amadeus::calc_covariates,
    ...
  ) {
    if (!status) {
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
        stop("Results do not match expectations.")
      }
      res_calc <- data.table::rbindlist(res_calc)
      saveRDS(res_calc, file = outpath, compress = "xz")
    }

  }


#' Merge input data.frame objects
#' @param by character. Joining keys. See [`merge`] for details.
#' @param time logical(1). Whether or not include time identifier.
#' Set this `TRUE` will supersede `by` value by appending time identifier.
#' @param ... data.frame objects to merge
#' @returns data.frame
combine <-
  function(
    by = c(mr("pointid")),
    time = FALSE,
    ...
  ) {
    if (time) {
      by <- c(mr("pointid"), mr("timeid"))
    }
    ellipsis <- list(...)
    do.call(function(x, y) merge(x, y, by = by), ellipsis)
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

#' Configure cross-validation row indices
#' @param covars Merged covariate data.frame
#' @param cvtypes character. Cross-validation types.
#' @returns List of cross-validation row indices
#' @seealso [`generate_cv_index`]
configure_cv <-
  function(
    covars = NULL,
    cvtypes = c("lolo", "loto", "lolto", "lblo", "lbto", "lblto", "random")
  ) {
    config <-
      list(
        list(
          covars = covars,
          cv_mode = "lolo"
        ),
        list(
          covars = covars,
          cv_mode = "loto"
        ),
        list(
          covars = covars,
          cv_mode = "lolto"
        ),
        list(
          covars = covars,
          cv_mode = "lblo",
          blocks = c(5, 5)
        ),
        list(
          covars = covars,
          cv_mode = "lbto",
          cv_fold = 10L
        ),
        list(
          covars = covars,
          cv_mode = "lblto",
          cv_fold = 10L,
          blocks = c(5, 5)
        ),
        list(
          covars = covars,
          cv_mode = "random"
        )
      )
    configured <-
      lapply(
        config,
        function(x) rlang::inject(generate_cv_index(!!!x))
      )
    names(configured) <- cvtypes
    return(configured)
  }


batch_base_learner <-
  function(

  )

fit_base <-
  function(

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