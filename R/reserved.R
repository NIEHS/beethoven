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
#' @return A character vector containing the matching function names.
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
#' @return A data.frame containing the parameters of the functions.
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


# nolint start
#' Base learner: Multilayer perceptron with brulee
#'
#' Multilayer perceptron model with different configurations of
#' hidden units, dropout, activation, and learning rate using brulee
#' and tidymodels. With proper settings, users can utilize graphics
#' processing units (GPU) to speed up the training process.
#' @keywords Baselearner soft-deprecated
#' @note tune package should be 1.2.0 or higher.
#' brulee should be installed with GPU support.
#' @details Hyperparameters `hidden_units`, `dropout`, `activation`,
#'   and `learn_rate` are tuned. `With tune_mode = "grid"`,
#'   users can modify `learn_rate` explicitly, and other hyperparameters
#'   will be predefined (56 combinations per `learn_rate`).
#' @param dt_imputed The input data table to be used for fitting.
#' @param folds pre-generated rset object with minimal number of columns.
#'   If NULL, `vfold` should be numeric to be used in [rsample::vfold_cv].
#' @param tune_mode character(1). Hyperparameter tuning mode.
#'   Default is "bayes", "grid" is acceptable.
#' @param tune_bayes_iter integer(1). The number of iterations for
#'  Bayesian optimization. Default is 10. Only used when `tune_mode = "bayes"`.
#' @param learn_rate The learning rate for the model. For branching purpose.
#'   Default is 0.1.
#' @param yvar The target variable.
#' @param xvar The predictor variables.
#' @param vfold The number of folds for cross-validation.
#' @param device The device to be used for training.
#'   Default is "cuda:0". Make sure that your system is equipped
#'   with CUDA-enabled graphical processing units.
#' @param trim_resamples logical(1). Default is TRUE, which replaces the actual
#'   data.frames in splits column of `tune_results` object with NA.
#' @param return_best logical(1). If TRUE, the best tuned model is returned.
#' @param ... Additional arguments to be passed.
#'
#' @return The fitted workflow.
#' @importFrom recipes recipe update_role
#' @importFrom dplyr `%>%`
#' @importFrom parsnip mlp set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid fit_best
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
fit_base_brulee <-
  function(
    dt_sample,
    dt_full,
    folds = NULL,
    cv_mode  = c("spatiotemporal", "spatial", "temporal"),
    tune_mode = "bayes",
    tune_bayes_iter = 10L,
    learn_rate = 0.1,
    yvar = "Arithmetic.Mean",
    xvar = seq(5, ncol(dt_sample)),
    vfold = 5L,
    device = "cuda:0",
    trim_resamples = FALSE,
    return_best = TRUE,
    args_generate_cv = NULL,
    ...
  ) {
    tune_mode <- match.arg(tune_mode, c("grid", "bayes"))
    cv_mode <- match.arg(cv_mode)

    # 2^9=512, 2^15=32768 (#param is around 10% of selected rows)
    grid_hyper_tune <-
      expand.grid(
        hidden_units = list(c(1024), c(64, 64), c(32, 32, 32), c(16, 16, 16)),
        dropout = 1 / seq(4, 2, -1),
        activation = c("relu", "leaky_relu"),
        learn_rate = learn_rate
      )

    base_recipe <-
      recipes::recipe(
        dt_sample[1, ]
      ) %>%
      # do we want to normalize the predictors?
      # if so, an additional definition of truly continuous variables is needed
      # recipes::step_normalize(recipes::all_numeric_predictors()) %>%
      recipes::update_role(!!xvar) %>%
      recipes::update_role(!!yvar, new_role = "outcome") #%>%
    # recipes::step_normalize(!!yvar)

    if (is.null(folds)) {
      base_vfold <- rsample::vfold_cv(dt_sample, v = vfold)
    } else {
      args_generate_cv <-
        c(
          list(data = dt_sample, cv_mode = cv_mode),
          args_generate_cv
        )
      # generate row index
      cv_index <- inject_match(switch_generate_cv_rset, args_generate_cv)

      # using cv_index, restore rset
      # NOTE 08122024: not modified -- should be recoded
      base_vfold <-
        convert_cv_index_rset(
          cv_index, dt_sample, cv_mode = cv_mode
        )
    }
    # base_vfold <-
    #   restore_rset_full(rset = base_vfold, data_full = dt_sample)

    base_model <-
      parsnip::mlp(
        hidden_units = parsnip::tune(),
        dropout = parsnip::tune(),
        epochs = 1000L,
        activation = parsnip::tune(),
        learn_rate = parsnip::tune()
      ) %>%
      parsnip::set_engine("brulee", device = device) %>%
      parsnip::set_mode("regression")

    base_wftune <-
      fit_base_tune(
        recipe = base_recipe,
        model = base_model,
        resample = base_vfold,
        tune_mode = tune_mode,
        grid = grid_hyper_tune,
        iter_bayes = tune_bayes_iter,
        trim_resamples = trim_resamples,
        return_best = return_best
      )

    return(base_wftune)
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
#' @keywords Baselearner soft-deprecated
#' @note tune package should be 1.2.0 or higher.
#' xgboost should be installed with GPU support.
#' @details Hyperparameters `mtry`, `ntrees`, and `learn_rate` are
#'   tuned. With `tune_mode = "grid"`,
#'   users can modify `learn_rate` explicitly, and other hyperparameters
#'   will be predefined (30 combinations per `learn_rate`).
#' @param dt_imputed The input data table to be used for fitting.
#' @param folds pre-generated rset object with minimal number of columns.
#'   If NULL, `vfold` should be numeric to be used in [rsample::vfold_cv].
#' @param tune_mode character(1). Hyperparameter tuning mode.
#'   Default is "bayes", "grid" is acceptable.
#' @param tune_bayes_iter integer(1). The number of iterations for
#' Bayesian optimization. Default is 10. Only used when `tune_mode = "bayes"`.
#' @param learn_rate The learning rate for the model. For branching purpose.
#'   Default is 0.1.
#' @param yvar The target variable.
#' @param xvar The predictor variables.
#' @param vfold The number of folds for cross-validation.
#' @param device The device to be used for training.
#'   Default is "cuda:0". Make sure that your system is equipped
#'   with CUDA-enabled graphical processing units.
#' @param trim_resamples logical(1). Default is TRUE, which replaces the actual
#'   data.frames in splits column of `tune_results` object with NA.
#' @param return_best logical(1). If TRUE, the best tuned model is returned.
#' @param ... Additional arguments to be passed.
#'
#' @return The fitted workflow.
#' @importFrom recipes recipe update_role
#' @importFrom dplyr `%>%`
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid fit_best
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
fit_base_xgb <-
  function(
    dt_imputed,
    folds = NULL,
    tune_mode = "bayes",
    tune_bayes_iter = 10L,
    learn_rate = 0.1,
    yvar = "Arithmetic.Mean",
    xvar = seq(5, ncol(dt_imputed)),
    vfold = 5L,
    device = "cuda:0",
    trim_resamples = TRUE,
    return_best = FALSE,
    ...
  ) {
    tune_mode <- match.arg(tune_mode, c("grid", "bayes"))
    # P --> ++ / fix as many hyperparams as possible
    grid_hyper_tune <-
      expand.grid(
        mtry = floor(c(0.02, 0.1, 0.02) * ncol(dt_imputed)),
        trees = seq(1000, 3000, 500),
        learn_rate = learn_rate
      )
    # dt_imputed <-
    #   dt_imputed %>%
    #   dplyr::slice_sample(prop = r_subsample)

    # generate row index for restoring rset
    cv_index <- switch_generate_cv_rset(
      data = dt_imputed,
      cv_mode = cv_mode
    )
    # using cv_index, restore rset
    # NOTE 08122024: not modified -- should be recoded
    rset_cv <-
      convert_cv_index_rset(
        index_cv, data_orig, ref_list = ref_list, cv_mode = cv_mode
      )



    base_recipe <-
      recipes::recipe(
        dt_imputed[1, ]
      ) %>%
      # recipes::step_normalize(recipes::all_numeric_predictors()) %>%
      recipes::update_role(tidyselect::all_of(xvar)) %>%
      recipes::update_role(tidyselect::all_of(yvar), new_role = "outcome")
    if (is.null(folds)) {
      base_vfold <- rsample::vfold_cv(dt_imputed, v = vfold)
    } else {
      base_vfold <- folds
    }
    base_vfold <-
      restore_rset_full(rset = base_vfold, data_full = dt_imputed)

    base_model <-
      parsnip::boost_tree(
        mtry = parsnip::tune(),
        trees = parsnip::tune(),
        learn_rate = parsnip::tune()
      ) %>%
      parsnip::set_engine("xgboost", device = device) %>%
      parsnip::set_mode("regression")

    base_wftune <-
      fit_base_tune(
        recipe = base_recipe,
        model = base_model,
        resample = base_vfold,
        tune_mode = tune_mode,
        grid = grid_hyper_tune,
        iter_bayes = tune_bayes_iter,
        trim_resamples = trim_resamples,
        return_best = return_best
      )

    return(base_wftune)

  }

# dt <- qs::qread("output/dt_feat_design_imputed_061024.qs")
# dtd <- dplyr::as_tibble(dt)
# dtfitx <- fit_base_xgb(dtd, xvar = names(dtd)[6:105], r_subsample = 0.3)


#' Base learner: Light Gradient Boosting Machine (LightGBM)
#'
#' LightGBM model is fitted at the defined rate (`r_subsample`) of
#' the input dataset by grid or Bayesian optimization search.
#' With proper settings, users can utilize graphics
#' processing units (GPU) to speed up the training process.
#' @keywords Baselearner soft-deprecated
#' @note tune package should be 1.2.0 or higher.
#' xgboost should be installed with GPU support.
#' @details Hyperparameters `mtry`, `ntrees`, and `learn_rate` are
#'   tuned. With `tune_mode = "grid"`,
#'   users can modify `learn_rate` explicitly, and other hyperparameters
#'   will be predefined (30 combinations per `learn_rate`).
#' @param dt_imputed The input data table to be used for fitting.
#' @param folds pre-generated rset object with minimal number of columns.
#'   If NULL, `vfold` should be numeric to be used in [rsample::vfold_cv].
#' @param tune_mode character(1). Hyperparameter tuning mode.
#'   Default is "bayes", "grid" is acceptable.
#' @param tune_bayes_iter integer(1). The number of iterations for
#' Bayesian optimization. Default is 10. Only used when `tune_mode = "bayes"`.
#' @param learn_rate The learning rate for the model. For branching purpose.
#'   Default is 0.1.
#' @param yvar The target variable.
#' @param xvar The predictor variables.
#' @param vfold The number of folds for cross-validation.
#' @param device The device to be used for training.
#'   Default is `"gpu"`. Make sure that your system is equipped
#'   with OpenCL-capable graphical processing units.
#'   A GPU-enabled version of LightGBM should be installed.
#' @param trim_resamples logical(1). Default is TRUE, which replaces the actual
#'   data.frames in splits column of `tune_results` object with NA.
#' @param return_best logical(1). If TRUE, the best tuned model is returned.
#' @param ... Additional arguments to be passed.
#'
#' @return The fitted workflow.
#' @importFrom recipes recipe update_role
#' @importFrom dplyr `%>%`
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid fit_best
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
fit_base_lightgbm <-
  function(
    dt_imputed,
    folds = NULL,
    tune_mode = "bayes",
    tune_bayes_iter = 10L,
    learn_rate = 0.1,
    yvar = "Arithmetic.Mean",
    xvar = seq(5, ncol(dt_imputed)),
    vfold = 5L,
    device = "gpu",
    trim_resamples = TRUE,
    return_best = FALSE,
    ...
  ) {
    tune_mode <- match.arg(tune_mode, c("grid", "bayes"))
    # P --> ++ / fix as many hyperparams as possible
    grid_hyper_tune <-
      expand.grid(
        mtry = floor(c(0.02, 0.1, 0.02) * ncol(dt_imputed)),
        trees = seq(1000, 3000, 500),
        learn_rate = learn_rate
      )

    # generate row index for restoring rset
    cv_index <- switch_generate_cv_rset(
      data = dt_imputed,
      cv_mode = cv_mode
    )
    # using cv_index, restore rset
    # NOTE 08122024: not modified -- should be recoded
    rset_cv <-
      convert_cv_index_rset(
        index_cv, data_orig, ref_list = ref_list, cv_mode = cv_mode
      )


    base_recipe <-
      recipes::recipe(
        dt_imputed[1, ]
      ) %>%
      # recipes::step_normalize(recipes::all_numeric_predictors()) %>%
      recipes::update_role(tidyselect::all_of(xvar)) %>%
      recipes::update_role(tidyselect::all_of(yvar), new_role = "outcome")
    if (is.null(folds)) {
      base_vfold <- rsample::vfold_cv(dt_imputed, v = vfold)
    } else {
      base_vfold <- folds
    }
    base_vfold <-
      restore_rset_full(rset = base_vfold, data_full = dt_imputed)

    base_model <-
      parsnip::boost_tree(
        mtry = parsnip::tune(),
        trees = parsnip::tune(),
        learn_rate = parsnip::tune()
      ) %>%
      parsnip::set_engine("lightgbm", device_type = device) %>%
      parsnip::set_mode("regression")

    base_wftune <-
      fit_base_tune(
        recipe = base_recipe,
        model = base_model,
        resample = base_vfold,
        tune_mode = tune_mode,
        grid = grid_hyper_tune,
        iter_bayes = tune_bayes_iter,
        trim_resamples = trim_resamples,
        return_best = return_best
      )

    return(base_wftune)

  }


#' Base learner: Elastic net
#'
#' Elastic net model is fitted at the defined rate (`r_subsample`) of
#' the input dataset by grid search.
#' @keywords Baselearner soft-deprecated
#' @note tune package should be 1.2.0 or higher.
#' @param dt_imputed The input data table to be used for fitting.
#' @param folds pre-generated rset object with minimal number of columns.
#'   If NULL, `vfold` should be numeric to be used in [rsample::vfold_cv].
#' @param yvar The target variable.
#' @param xvar The predictor variables.
#' @param vfold The number of folds for cross-validation.
#' @param tune_mode character(1). Hyperparameter tuning mode.
#'   Default is "grid", "bayes" is acceptable.
#' @param tune_bayes_iter integer(1). The number of iterations for
#' Bayesian optimization. Default is 50. Only used when `tune_mode = "bayes"`.
#' @param nthreads The number of threads to be used. Default is 16L.
#' @param return_best logical(1). If TRUE, the best tuned model is returned.
#' @param ... Additional arguments to be passed.
#'
#' @return The fitted workflow.
#' @importFrom future plan multicore multisession
#' @importFrom dplyr `%>%`
#' @importFrom recipes recipe update_role
#' @importFrom parsnip linear_reg set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid fit_best
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
fit_base_elnet <-
  function(
    dt_imputed,
    folds = NULL,
    # r_subsample = 0.3,
    yvar = "Arithmetic.Mean",
    xvar = seq(5, ncol(dt_imputed)),
    tune_mode = "grid",
    tune_bayes_iter = 50L,
    vfold = 5L,
    nthreads = 16L,
    trim_resamples = TRUE,
    return_best = FALSE,
    ...
  ) {
    grid_hyper_tune <-
      expand.grid(
        mixture = seq(0, 1, length.out = 21),
        penalty = 10 ^ seq(-3, 5)
      )

    # generate row index for restoring rset
    cv_index <- switch_generate_cv_rset(
      data = dt_imputed,
      cv_mode = cv_mode
    )
    # using cv_index, restore rset
    # NOTE 08122024: not modified -- should be recoded
    rset_cv <-
      convert_cv_index_rset(
        index_cv, data_orig, ref_list = ref_list, cv_mode = cv_mode
      )



    base_recipe <-
      recipes::recipe(
        dt_imputed[1, ]
      ) %>%
      # recipes::step_normalize(recipes::all_numeric_predictors()) %>%
      recipes::update_role(tidyselect::all_of(xvar)) %>%
      recipes::update_role(tidyselect::all_of(yvar), new_role = "outcome")

    if (is.null(folds)) {
      base_vfold <- rsample::vfold_cv(dt_imputed, v = vfold)
    } else {
      base_vfold <- folds
    }
    base_vfold <-
      restore_rset_full(rset = base_vfold, data_full = dt_imputed)

    base_model <-
      parsnip::linear_reg(
        mixture = parsnip::tune(),
        penalty = parsnip::tune()
      ) %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::set_mode("regression")

    future::plan(future::multicore, workers = nthreads)
    base_wftune <-
      fit_base_tune(
        recipe = base_recipe,
        model = base_model,
        resample = base_vfold,
        tune_mode = tune_mode,
        grid = grid_hyper_tune,
        iter_bayes = tune_bayes_iter,
        trim_resamples = trim_resamples,
        return_best = return_best
      )
    future::plan(future::sequential)
    return(base_wftune)

  }
# nolint end

#' Restore the full data set from the rset object
#' @keywords Baselearner soft-deprecated
#' @param rset [rsample::manual_rset()] object's `splits` column
#' @param data_full data.table with all features
#' @return A list of data.table objects.
#' @importFrom collapse join
#' @note $splits should be present in rset.
restore_rset_full <-
  function(rset, data_full) {
    rset$splits <-
      lapply(
        rset$splits,
        function(x) {
          x$data <-
            collapse::join(
              x$data,
              data_full,
              on = c("site_id", "time"),
              how = "left"
            )
          return(x)
        }
      )
    return(rset)
  }


#' Restore the full data set from two rset objects then fit the best model
#' @keywords Baselearner soft-deprecated
#' @param rset_trimmed rset object without data in splits column.
#' @param rset_full rset object with full data.
#' @param df_full data.table with full data.
#' @param nested logical(1). If TRUE, the rset object is nested.
#' @param nest_length integer(1). Length of the nested list.
#'   i.e., Number of resamples.
#' @note Per introduction of fit_base_tune,
#'  the utility of this function might be limited.
#' @return rset object with full data in splits column.
#' @importFrom dplyr %>%
#' @export
restore_fit_best <-
  function(
    rset_trimmed,
    rset_full,
    df_full,
    by = c("site_id", "time"),
    nested = TRUE,
    nest_length = 30L
  ) {
    parsnip_spec <-
      workflows::extract_spec_parsnip(rset_trimmed[[1]])
    # Do I need to restore full data in rset_trimmed?

    # reassemble the branched rsets
    if (nested) {
      rset_trimmed <-
        as.list(seq_len(nest_length)) %>%
        lapply(
          function(x) {
            # here we have list length of 4, each has .metric column,
            # which we want to bind_rows at
            # [[1]] $.metric
            # [[2]] $.metric ...
            template <- x[[1]]
            combined_lr <- x[seq(x, length(rset_trimmed), nest_length)]
            # length of 4;
            # combine rows of each element in four lists
            combined_lr <-
              mapply(
                function(df1, df2, df3, df4) {
                  dplyr::bind_rows(df1, df2, df3, df4)
                },
                combined_lr[[1]], combined_lr[[2]],
                combined_lr[[3]], combined_lr[[4]],
                SIMPLIFY = FALSE
              )
            template$.metric <- combined_lr
            return(template)
          }
        )
    }

    tuned_best <- tune::show_best(rset_trimmed, n = 1)
    model_best <-
      rlang::inject(
        parsnip::update(parsnip_spec, parameters = !!!as.list(tuned_best))
      )

    # fit the entire data
    model_fit <- parsnip::fit(model_best, data = df_full)
    pred <- predict(model_fit, data = df_full)
    return(pred)

  }



# nocov end
