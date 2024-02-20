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
  function() {

  }

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