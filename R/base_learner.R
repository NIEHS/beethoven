# Base learners and auxiliary functions for base learners


# nocov start

#' Make sampled subdataframes for base learners
#'
#' Per beethoven resampling strategy, this function selects
#' the predefined number of rows from the input data table and
#' saves the row index in .rowindex field.
#'
#' @keywords Baselearner
#' @param data An object that inherits data.frame.
#' @param n The number of rows to be sampled.
#' @param p The proportion of rows to be used. Default is 0.3.
#' @returns The sampled data table with row index saved in .rowindex field.
make_subdata <-
  function(
    data,
    n = NULL,
    p = 0.3
  ) {
    if (is.null(n) && is.null(p)) {
      stop("Please provide either n or p.")
    }
    nr <- seq_len(nrow(data))
    if (!is.null(n)) {
      nsample <- sample(nr, n)
    } else {
      nsample <- sample(nr, ceiling(nrow(data) * p))
    }
    data <- data[nsample, ]
    data$.rowindex <- nsample
    data_name <- as.character(substitute(data))
    attr(data, "object_origin") <- data_name[length(data_name)]
    return(data)
  }



#' Base learner: Multilayer perceptron with brulee
#'
#' Multilayer perceptron model with different configurations of
#' hidden units, dropout, activation, and learning rate using brulee
#' and tidymodels. With proper settings, users can utilize graphics
#' processing units (GPU) to speed up the training process.
#' @keywords Baselearner
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
#'   Default is "grid", "bayes" is acceptable.
#' @param tune_bayes_iter integer(1). The number of iterations for
#'  Bayesian optimization. Default is 50. Only used when `tune_mode = "bayes"`.
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
#' @returns The fitted workflow.
#' @importFrom recipes recipe update_role
#' @importFrom dplyr `%>%`
#' @importFrom parsnip mlp set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid fit_best
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
#' @export
fit_base_brulee <-
  function(
    dt_imputed,
    folds = NULL,
    tune_mode = "grid",
    tune_bayes_iter = 50L,
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

    # 2^9=512, 2^15=32768 (#param is around 10% of selected rows)
    grid_hyper_tune <-
      expand.grid(
        hidden_units = list(c(1024), c(64, 64), c(32, 32, 32), c(16, 16, 16)),
        dropout = 1 / seq(4, 2, -1),
        activation = c("relu", "leaky_relu"),
        learn_rate = learn_rate
      )
    # dt_imputed <-
    #   dt_imputed %>%
    #   dplyr::slice_sample(prop = r_subsample)

    base_recipe <-
      recipes::recipe(
        dt_imputed[1, ]
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
    base_vfold <-
      restore_rset_full(rset = base_vfold, data_full = dt_imputed)

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


    base_wf <-
      workflows::workflow() %>%
      workflows::add_recipe(base_recipe) %>%
      workflows::add_model(base_model)

    if (tune_mode == "grid") {
      wf_config <-
        tune::control_grid(
          verbose = TRUE,
          save_pred = TRUE,
          save_workflow = TRUE
        )
      base_wf <-
        base_wf %>%
        tune::tune_grid(
          resamples = base_vfold,
          grid = grid_hyper_tune,
          metrics =
          yardstick::metric_set(
            yardstick::rmse,
            yardstick::mape,
            yardstick::rsq,
            yardstick::mae
          ),
          control = wf_config
        )
    } else {
      wf_config <-
        tune::control_bayes(
          verbose = TRUE,
          save_pred = TRUE,
          save_workflow = TRUE
        )
      base_wf <-
        base_wf %>%
        tune::tune_bayes(
          resamples = base_vfold,
          iter = tune_bayes_iter,
          metrics = yardstick::metric_set(yardstick::rmse, yardstick::mape),
          control = wf_config
        )
    }
    if (trim_resamples) {
      base_wf$splits <- NA
    }
    if (return_best) {
      base_wf <- tune::show_best(base_wf, n = 1)
    }
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
#'   Default is "grid", "bayes" is acceptable.
#' @param tune_bayes_iter integer(1). The number of iterations for
#' Bayesian optimization. Default is 50. Only used when `tune_mode = "bayes"`.
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
#' @returns The fitted workflow.
#' @importFrom recipes recipe update_role
#' @importFrom dplyr `%>%`
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid fit_best
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
#' @export
fit_base_xgb <-
  function(
    dt_imputed,
    folds = NULL,
    tune_mode = "grid",
    tune_bayes_iter = 50L,
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

    base_wf <-
      workflows::workflow() %>%
      workflows::add_recipe(base_recipe) %>%
      workflows::add_model(base_model)

    if (tune_mode == "grid") {
      wf_config <-
        tune::control_grid(
          verbose = TRUE,
          save_pred = TRUE,
          save_workflow = TRUE
        )

      base_wf <-
        base_wf %>%
        tune::tune_grid(
          resamples = base_vfold,
          grid = grid_hyper_tune,
          metrics =
          yardstick::metric_set(
            yardstick::rmse,
            yardstick::mape,
            yardstick::rsq,
            yardstick::mae
          ),
          control = wf_config
        )
    } else {
      wf_config <-
        tune::control_bayes(
          verbose = TRUE,
          save_pred = TRUE,
          save_workflow = TRUE
        )
      base_wf <-
        base_wf %>%
        tune::tune_bayes(
          resamples = base_vfold,
          iter = tune_bayes_iter,
          metrics = yardstick::metric_set(yardstick::rmse, yardstick::mape),
          control = wf_config
        )
    }
    if (trim_resamples) {
      base_wf$splits <- NA
    }
    if (return_best) {
      base_wf <- tune::show_best(base_wf, n = 1)
    }

    return(base_wf)

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
#' @keywords Baselearner
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
#'   Default is "grid", "bayes" is acceptable.
#' @param tune_bayes_iter integer(1). The number of iterations for
#' Bayesian optimization. Default is 50. Only used when `tune_mode = "bayes"`.
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
#' @returns The fitted workflow.
#' @importFrom recipes recipe update_role
#' @importFrom dplyr `%>%`
#' @importFrom parsnip boost_tree set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid fit_best
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
#' @export
fit_base_lightgbm <-
  function(
    dt_imputed,
    folds = NULL,
    tune_mode = "grid",
    tune_bayes_iter = 50L,
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
    # dt_imputed <-
    #   dt_imputed %>%
    #   dplyr::slice_sample(prop = r_subsample)

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
      parsnip::set_engine("xgboost", device_type = device) %>%
      parsnip::set_mode("regression")

    base_wf <-
      workflows::workflow() %>%
      workflows::add_recipe(base_recipe) %>%
      workflows::add_model(base_model)

    if (tune_mode == "grid") {
      wf_config <-
        tune::control_grid(
          verbose = TRUE,
          save_pred = TRUE,
          save_workflow = TRUE
        )
      base_wf <-
        base_wf %>%
        tune::tune_grid(
          resamples = base_vfold,
          grid = grid_hyper_tune,
          metrics =
          yardstick::metric_set(
            yardstick::rmse,
            yardstick::mape,
            yardstick::rsq,
            yardstick::mae
          ),
          control = wf_config
        )
    } else {
      wf_config <-
        tune::control_bayes(
          verbose = TRUE,
          save_pred = TRUE,
          save_workflow = TRUE
        )
      base_wf <-
        base_wf %>%
        tune::tune_bayes(
          resamples = base_vfold,
          iter = tune_bayes_iter,
          metrics = yardstick::metric_set(yardstick::rmse, yardstick::mape),
          control = wf_config
        )
    }
    if (trim_resamples) {
      base_wf$splits <- NA
    }
    if (return_best) {
      base_wf <- tune::show_best(base_wf, n = 1)
    }

    return(base_wf)

  }


#' Base learner: Elastic net
#'
#' Elastic net model is fitted at the defined rate (`r_subsample`) of
#' the input dataset by grid search.
#' @keywords Baselearner
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
#' @returns The fitted workflow.
#' @importFrom future plan multicore multisession
#' @importFrom dplyr `%>%`
#' @importFrom recipes recipe update_role
#' @importFrom parsnip linear_reg set_engine set_mode
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid fit_best
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv
#' @export
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
    # dt_imputed <-
    #   dt_imputed %>%
    #   dplyr::slice_sample(prop = r_subsample)

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
    base_wf <-
      workflows::workflow() %>%
      workflows::add_recipe(base_recipe) %>%
      workflows::add_model(base_model)

    if (tune_mode == "grid") {
      wf_config <-
        tune::control_grid(
          verbose = TRUE,
          save_pred = TRUE,
          save_workflow = TRUE
        )

      base_wf <-
        base_wf %>%
        tune::tune_grid(
          resamples = base_vfold,
          grid = grid_hyper_tune,
          metrics =
          yardstick::metric_set(
            yardstick::rmse,
            yardstick::mape,
            yardstick::rsq,
            yardstick::mae
          ),
          control = wf_config,
          parallel_over = "resamples"
        )
    } else {
      wf_config <-
        tune::control_bayes(
          verbose = TRUE,
          save_pred = TRUE,
          save_workflow = TRUE
        )
      base_wf <-
        base_wf %>%
        tune::tune_bayes(
          resamples = base_vfold,
          iter = tune_bayes_iter,
          metrics = yardstick::metric_set(yardstick::rmse, yardstick::mape),
          control = wf_config,
          parallel_over = "resamples"
        )
    }
    if (trim_resamples) {
      base_wf$splits <- NA
    }
    if (return_best) {
      base_wf <- tune::show_best(base_wf, n = 1)
    }
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
    data_full_attach <-
      collapse::join(
        data_full_lean, data_sfd, on = locs_id, how = "left"
      )
    return(data_full_attach)
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
#' @param ... Additional arguments to be passed.
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
    cv_mode = "spt",
    ...
  ) {
    if (length(target_cols) != 3) {
      stop("Please provide three target columns.")
    }
    data_orig <- data
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
        index_cv, data_orig, ref_list = ref_list, cv_mode = cv_mode
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



#' Prepare spatial and spatiotemporal cross validation sets
#' @keywords Baselearner
#' @param data data.table with X, Y, and time information.
#' @param r_subsample The proportion of rows to be sampled.
#' @param target_cols character(3). Names of columns for X, Y.
#'   Default is `c("lon", "lat")`. It is passed to sf::st_as_sf to
#'   subsequently generate spatial cross-validation indices using
#'   `spatialsample::spatial_block_cv` and
#'   `spatialsample::spatial_clustering_cv`.
#' @param cv_make_fun function(1). Function to generate spatial
#'   cross-validation indices. Default is `spatialsample::spatial_block_cv`.
#' @seealso [`generate_cv_index`] [`spatialsample::spatial_block_cv`]
#'   [`spatialsample::spatial_clustering_cv`]
#' @returns rsample::manual_rset() object.
#' @importFrom rlang inject
#' @importFrom sf st_as_sf
#' @export
prepare_cvindex <-
  function(
    data,
    r_subsample = 0.3,
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    ...
  ) {
    data <- data %>%
      dplyr::slice_sample(prop = r_subsample)

    if (methods::getPackageName(environment(cv_make_fun)) == "beethoven") {
      cv_index <-
        rlang::inject(
          cv_make_fun(
            data = data,
            target_cols = target_cols,
            !!!list(...)
          )
        )
    } else {
      data_sf <- sf::st_as_sf(data, coords = target_cols, remove = FALSE)
      cv_index <-
        rlang::inject(
          cv_make_fun(
            data_sf,
            !!!list(...)
          )
        )
      # assign id with function name
      fun_name <- as.character(substitute(cv_make_fun))
      fun_name <- fun_name[length(fun_name)]
      cv_index$id <- sprintf("%s_%02d", fun_name, seq_len(nrow(cv_index)))
    }
    return(cv_index)
  }


#' Restore the full data set from the rset object
#' @keywords Baselearner
#' @param rset rsample::manual_rset() object's `splits` column
#' @param data_full data.table with all features
#' @returns A list of data.table objects.
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
#' @keywords Baselearner
#' @param rset_trimmed rset object without data in splits column.
#' @param rset_full rset object with full data.
#' @param df_full data.table with full data.
#' @param nested logical(1). If TRUE, the rset object is nested.
#' @param nest_length integer(1). Length of the nested list.
#'   i.e., Number of resamples.
#' @returns rset object with full data in splits column.
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
