## pipeline base functions

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


join_yx <-
  function(
    df_pm,
    df_covar,
    locs_id = mr("pointid"),
    time_id = mr("timeid")
  ) {
    merge(df_pm, df_covar, by = c(locs_id, time_id))
  }


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



# is it possible to download missing files only?
fastdown <-
  function(
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

fit_meta <-
  function(

  ) {

  }

export_res <-
  function(

  ) {

  }