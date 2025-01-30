# Base learners and auxiliary functions for base learners

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
#' @param ngroup_init integer(1). Initial number of splits for
#'  pairing groups. Default is NULL. Ensures that subsample is divisible
#' by `ngroup_init` for `generate_cv_index_spt`.
#' @return The row index of the original data. The name of the original
#'   data object is stored in attribute "object_origin".
#' @export
make_subdata <- function(
  data,
  n = NULL,
  p = 0.3,
  ngroup_init = NULL
) {
  if (is.null(n) && is.null(p)) {
    stop("Please provide either n or p.")
  }
  if (!is.null(ngroup_init) && ngroup_init <= 0) {
    stop("ngroup_init must be a positive integer.")
  }

  nr <- seq_len(nrow(data))

  if (!is.null(n)) {
    if (!is.null(ngroup_init)) {
      n <- floor(n / ngroup_init) * ngroup_init
    }
    nsample <- sample(nr, n)
  } else {
    sample_size <- ceiling(nrow(data) * p)
    if (!is.null(ngroup_init)) {
      sample_size <- floor(sample_size / ngroup_init) * ngroup_init
    }
    nsample <- sample(nr, sample_size)
  }

  rowindex <- nsample
  data_name <- as.character(substitute(data))
  attr(rowindex, "object_origin") <- data_name[length(data_name)]

  return(rowindex)
}


#' Define a base learner model based on parsnip and tune
#' @keywords Baselearner
#' @param model_type character(1). Model type to be used.
#'  Default is "mlp". Available options are "mlp", "xgb", "lgb", "elnet".
#' @param learn_rate numeric(1). The learning rate for the model.
#' Default is 0.1.
#' @param device character(1). The device to be used for training.
#' Default is "cuda:0". Make sure that your system is equipped
#' with CUDA-enabled graphical processing units.
#' @return A parsnip model object.
#' @importFrom parsnip mlp set_engine set_mode boost_tree linear_reg
#' @importFrom dplyr %>%
#' @export
switch_model <-
  function(
    model_type = c("mlp", "xgb", "lgb", "elnet"),
    learn_rate = 0.1,
    device = "cuda:0"
  ) {

    switch(
      model_type,
      mlp =
        parsnip::mlp(
          hidden_units = parsnip::tune(),
          dropout = parsnip::tune(),
          epochs = 500,
          activation = parsnip::tune(),
          learn_rate = parsnip::tune()
        ) %>%
        parsnip::set_engine("brulee", device = device) %>%
        parsnip::set_mode("regression"),
      lgb =
        parsnip::boost_tree(
          mtry = parsnip::tune(),
          trees = parsnip::tune(),
          learn_rate = parsnip::tune()
        ) %>%
        parsnip::set_engine("lightgbm", device = device) %>%
        parsnip::set_mode("regression"),
      xgb =
        parsnip::boost_tree(
          mtry = parsnip::tune(),
          trees = parsnip::tune(),
          learn_rate = parsnip::tune()
        ) %>%
        parsnip::set_engine("xgboost", device = device) %>%
        parsnip::set_mode("regression"),
      elnet =
        parsnip::linear_reg(
          mixture = parsnip::tune(),
          penalty = parsnip::tune()
        ) %>%
        parsnip::set_engine("glmnet") %>%
        parsnip::set_mode("regression")
    )

  }



#' Base learner: tune hyperparameters and retrieve the best model
#'
#' Multilayer perceptron model with different configurations of
#' hidden units, dropout, activation, and learning rate using brulee
#' and tidymodels. With proper settings, users can utilize graphics
#' processing units (GPU) to speed up the training process.
#'
#' LightGBM model is fitted at the defined rate (`r_subsample`) of
#' the input dataset by grid or Bayesian optimization search.
#' With proper settings, users can utilize graphics
#' processing units (GPU) to speed up the training process.
#'
#' XGBoost model is fitted at the defined rate (`r_subsample`) of
#' the input dataset by grid or Bayesian optimization search.
#' With proper settings, users can utilize graphics
#' processing units (GPU) to speed up the training process.
#'
#' Elastic net model is fitted at the defined rate (`r_subsample`) of
#' the input dataset by grid search or Bayesian optimization.
#'
#' @keywords Baselearner
#' @note tune package should be 1.2.0 or higher.
#' brulee, xgboost, and lightgbm should be installed with GPU support.
#' Grid search is not activated in this function, regardless of other parts'
#' description.
#' @details
#'  * MLP: Hyperparameters `hidden_units`, `dropout`, `activation`,
#'    and `learn_rate` are tuned. `With tune_mode = "grid"`,
#'    users can modify `learn_rate` explicitly, and other hyperparameters
#'    will be predefined (56 combinations per `learn_rate` for mlp).
#'  * XGBoost: Hyperparameters `mtry`, `ntrees`, and `learn_rate` are
#'    tuned. With `tune_mode = "grid"`,
#'    users can modify `learn_rate` explicitly, and other hyperparameters
#'    will be predefined (30 combinations per `learn_rate`).
#'  * LightGBM: Hyperparameters `mtry`, `ntrees`, and `learn_rate` are
#'    tuned. With `tune_mode = "grid"`,
#'    users can modify `learn_rate` explicitly, and other hyperparameters
#'    will be predefined (30 combinations per `learn_rate`).
#'  * Elastic net: Hyperparameters `mixture` and `penalty` are tuned.
#'
#' Tuning is performed based on random grid search (size = 10).
#' @param learner character(1). The base learner to be used.
#'   Default is "mlp". Available options are "mlp", "xgb", "lgb", "elnet".
#' @param dt_full The full data table to be used for prediction.
#' @param r_subsample numeric(1). The proportion of rows to be used.
#' @param model The parsnip model object. Preferably generated from
#'   `switch_model`.
#' @param folds integer(1). Number of cross-validation folds.
#'   If NULL, `cv_mode` should be defined to be used in [rsample::vfold_cv].
#' @param cv_mode character(1).
#'   Cross-validation mode. Default is "spatiotemporal".
#'   Available options are "spatiotemporal", "spatial", "temporal".
#' @param args_generate_cv List of arguments to be passed to
#'  `switch_generate_cv_rset` function.
#' @param tune_mode character(1). Hyperparameter tuning mode.
#'   Default is "grid", "bayes" is acceptable.
#' @param tune_bayes_iter integer(1). The number of iterations for
#'  Bayesian optimization. Default is 10. Only used when `tune_mode = "bayes"`.
#' @param tune_grid_in data.frame object that includes the grid for
#'   hyperparameter tuning. `tune_grid_size` rows will be randomly picked
#'   from this data.frame for grid search.
#' @param tune_grid_size integer(1). The number of grid size for hyperparameter
#'  tuning. Default is 10. Only used when `tune_mode = "grid"`.
#' @param learn_rate The learning rate for the model. For branching purpose.
#'   Default is 0.1.
#' @param yvar The target variable.
#' @param xvar The predictor variables.
#' @param nthreads integer(1). The number of threads to be used for
#'  tuning. Default is 8L. `learner = "elnet"` will utilize the multiple
#'  threads in [future::multicore()] plan.
#' @param trim_resamples logical(1). Default is TRUE, which replaces the actual
#'   data.frames in splits column of `tune_results` object with NA.
#' @param return_best logical(1). If TRUE, the best tuned model is returned.
#' @param metric character(1). The metric to be used for selecting the best.
#' Must be one of "rmse", "rsq", "mae". Default = "rmse"
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
#' @export
fit_base_learner <-
  function(
    learner = c("mlp", "xgb", "lgb", "elnet"),
    dt_full,
    r_subsample = 0.3,
    model = NULL,
    folds = 5L,
    cv_mode  = c("spatiotemporal", "spatial", "temporal"),
    args_generate_cv = NULL,
    tune_mode = "grid",
    tune_bayes_iter = 10L,
    tune_grid_in = NULL,
    tune_grid_size = 10L,
    learn_rate = 0.1,
    yvar = "Arithmetic.Mean",
    xvar = seq(5, ncol(dt_sample)),
    nthreads = 8L,
    trim_resamples = FALSE,
    return_best = TRUE,
    metric = "rmse",
    ...
  ) {
    learner <- match.arg(learner)
    tune_mode <- match.arg(tune_mode, c("grid", "bayes"))
    cv_mode <- match.arg(cv_mode)
    stopifnot("parsnip model must be defined." = !is.null(model))

    if (cv_mode == "spatiotemporal") {
      ngroup_init <- args_generate_cv$ngroup_init
    } else {
      ngroup_init <- NULL
    }
    dt_sample_rowidx <- beethoven:::make_subdata(
      dt_full, p = r_subsample, ngroup_init = ngroup_init
    )
    dt_sample <- dt_full[dt_sample_rowidx, ]

    # detect model name
    model_name <- model$engine

    base_recipe <-
      recipes::recipe(
        dt_sample[1, ]
      ) %>%
      # do we want to normalize the predictors?
      # if so, an additional definition of truly continuous variables is needed
      # recipes::step_normalize(recipes::all_numeric_predictors()) %>%
      recipes::update_role(!!xvar) %>%
      recipes::update_role(!!yvar, new_role = "outcome")

    if (!is.null(folds)) {
      base_vfold <- rsample::vfold_cv(dt_sample, v = folds)
    } else {
      args_generate_cv <-
        c(
          list(data = dt_sample, cv_mode = cv_mode),
          args_generate_cv
        )
      # generate row index
      # cv_index <- inject_match(switch_generate_cv_rset, args_generate_cv)

      # manually replicate switch_generate_cv_rset function
      # the formals() argument used in inject_match does not properly
      # identify the expected arguments in the switched functions
      # identify cv_mode
      cv_mode_arg <- match.arg(
        cv_mode, c("spatial", "temporal", "spatiotemporal")
      )
      target_fun <-
        switch(
          cv_mode_arg,
          spatial = generate_cv_index_sp,
          temporal = generate_cv_index_ts,
          spatiotemporal = generate_cv_index_spt
        )

      # generate row index
      cv_index <- beethoven::inject_match(target_fun, args_generate_cv)

      # using cv_index, restore rset
      base_vfold <-
        beethoven::convert_cv_index_rset(
          cv_index, dt_sample, cv_mode = cv_mode
        )
    }

    # generate random grid from hyperparameters
    # dials approach is too complicated to implement since
    # we already declared tuning hyperparameters with tune(),
    # which is not compatible with dials approach to limit
    # possible value ranges per hyperparameter.
    # model_params <- tune::extract_parameter_set_dials(model)
    # grid_params <- dials::grid_random(model_params, size = tune_grid_size)
    if (tune_mode == "grid") {
      grid_row_idx <-
        sample(nrow(tune_grid_in), tune_grid_size, replace = FALSE)
      grid_params <- tune_grid_in[grid_row_idx, ]
    } else {
      grid_params <- NULL
      # drop mtry from model arguments if using baysian tuning
      # for xgboost
      if (model$engine %in% c("xgboost", "lightgbm")) {
        model <- model %>% parsnip::set_args(mtry = NULL)
      }
    }


    if (model_name == "glmnet") {
      future::plan(future::multicore, workers = nthreads)
    }
    base_wftune <-
      fit_base_tune(
        recipe = base_recipe,
        model = model,
        resample = base_vfold,
        tune_mode = tune_mode,
        grid = grid_params,
        iter_bayes = tune_bayes_iter,
        trim_resamples = trim_resamples,
        return_best = return_best,
        data_full = dt_full,
        metric = metric
      )
    if (model_name == "glmnet") {
      future::plan(future::sequential)
    }

    return(base_wftune)
  }


#' Tune base learner
#' @keywords Baselearner internal
#' @param recipe The recipe object.
#' @param model The model object.
#' @param resample The resample object. It is expected to be generated from the
#'   subsamples.
#' @param tune_mode character(1). Hyperparameter tuning mode.
#'  Default is "bayes", "grid" is acceptable.
#' @param grid The grid object for hyperparameter tuning.
#' @param trim_resamples logical(1). Default is TRUE, which replaces the actual
#'  data.frames in splits column of `tune_results` object with NA.
#' @param return_best logical(1). If TRUE, the best tuned model is returned.
#' @param data_full The full data frame to be used for prediction.
#' @param metric character(1). The metric to be used for selecting the best.
#' Must be one of "rmse", "rsq", "mae". Default = "rmse"
#' @return List of 3:
#'   * `base_prediction`: `data.frame` of the best model prediction.
#'   * `base_parameter`: `tune_results` object of the best model.
#'   * `best_performance`: `data.frame` of the performance metrics. It
#'     includes RMSE, MAPE, R-squared, and MAE for **all** tuned models.
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid tune_bayes control_grid control_bayes
#' @importFrom yardstick metric_set rmse mape rsq mae
#' @importFrom parsnip fit
#' @importFrom stats predict
#' @importFrom rlang quo_get_expr
#' @export
fit_base_tune <-
  function(
    recipe,
    model,
    resample,
    tune_mode = c("bayes", "grid"),
    grid = NULL,
    iter_bayes = 10L,
    trim_resamples = TRUE,
    return_best = TRUE,
    data_full = NULL,
    metric = "rmse"
  ) {
    stopifnot("data_full must be entered." = !is.null(data_full))
    tune_mode <- match.arg(tune_mode)
    base_wf <-
      workflows::workflow() %>%
      workflows::add_recipe(recipe) %>%
      workflows::add_model(model)

    if (tune_mode == "grid") {
      wf_config <-
        tune::control_grid(
          verbose = TRUE,
          save_pred = FALSE,
          save_workflow = TRUE
        )
      base_wftune <-
        base_wf %>%
        tune::tune_grid(
          resamples = resample,
          grid = grid,
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
          save_pred = FALSE,
          save_workflow = TRUE
        )
      base_wftune <-
        base_wf %>%
        tune::tune_bayes(
          resamples = resample,
          iter = iter_bayes,
          metrics =
          yardstick::metric_set(
            yardstick::rmse,
            yardstick::mae,
            yardstick::mape,
            yardstick::rsq
          ),
          control = wf_config
        )
    }
    # DEVELOPMENT CHANGE
    # mm-0904 Drop base_wftune from return when trim_resamples = TRUE
    # due to large data size. 1 iter > 25Gb
    # if (trim_resamples) {
    #   base_wftune$splits <- NA
    # }
    if (return_best) {
      # Select the best hyperparameters
      metric <- match.arg(metric, c("rmse", "rsq", "mae"))
      base_wfparam <-
        tune::select_best(
          base_wftune,
          metric = metric
        )
      # finalize workflow with the best tuned hyperparameters
      base_wfresult <- tune::finalize_workflow(base_wf, base_wfparam)

      # DEVELOPMENT CHANGE
      # mm-0904 unlist multi-layered hidden units if mlp model
      if (model$engine == "brulee" && is.list(grid$hidden_units)) {
        base_wfresult$fit$actions$model$spec$args$hidden_units <-
          unlist(
            rlang::quo_get_expr(
              base_wfresult$fit$actions$model$spec$args$hidden_units
            )
          )
      }

      # Best-fit model
      base_wf_fit_best <- parsnip::fit(base_wfresult, data = data_full)
      # Prediction with the best model
      base_wf_pred_best <-
        stats::predict(base_wf_fit_best, new_data = data_full)

      base_wflist <-
        list(
          base_prediction = base_wf_pred_best,
          base_parameter = base_wfparam,
          best_performance = base_wftune
        )
    }
    # DEVELOPMENT CHANGE
    # mm-0904 see above
    if (trim_resamples) {
      base_wflist <- base_wflist[-3]
    }
    return(base_wflist)
  }


#' Shuffle cross-validation mode for each learner type
#' @keywords Baselearner
#' @param learner character(1). The base learner to be used.
#'  Default is "mlp". Available options are "mlp", "lgb", "elnet".
#' @param cv_mode character(1). The cross-validation mode to be used.
#'  Default is "spatiotemporal". Available options are "spatiotemporal",
#'  "spatial", "temporal".
#' @param cv_rep integer(1). The number of repetitions for each `cv_mode`.
#' @param num_device integer(1). The number of CUDA devices to be used.
#'   Each device will be assigned to each eligible learner (i.e., lgb, mlp).
#' @param balance logical(1). If TRUE, the number of CUDA devices will be
#' equally distributed based on the number of eligible devices.
#' @return A data frame with three columns: learner, cv_mode, and device.
#' @export
assign_learner_cv <-
  function(
    learner = c("lgb", "mlp", "elnet"),
    cv_mode = c("spatiotemporal", "spatial", "temporal"),
    cv_rep = 100L,
    num_device = ifelse(torch::cuda_device_count() > 1, 2, 1),
    balance = FALSE
  ) {
    learner_eligible <- c("lgb", "mlp", "xgb")
    learner <- sort(learner)
    learner_eligible_flag <- learner %in% learner_eligible
    cuda_devices <- seq_len(sum(learner_eligible_flag)) - 1
    cuda_devices <- sprintf("cuda:%d", cuda_devices)
    cuda_devices <-
      rep(cuda_devices, ceiling(length(learner) / length(cuda_devices)))
    cuda_devices <- cuda_devices[seq_len(sum(learner_eligible_flag))]
    cuda_get <- learner_eligible_flag
    cuda_get[learner_eligible_flag] <- cuda_devices
    cuda_get[!learner_eligible_flag] <- "null"

    learner_l <- split(learner, learner)
    learner_l <- mapply(
      function(x, y) {
        cv_mode_rep <- rep(cv_mode, each = cv_rep)
        df <-
          data.frame(
            learner = rep(x, cv_rep * length(cv_mode)),
            cv_mode =
            cv_mode_rep[sample(length(cv_mode_rep), length(cv_mode_rep))],
            device = y
          )
        return(df)
      },
      learner_l, cuda_get, SIMPLIFY = FALSE
    )
    learner_v <- do.call(rbind, learner_l)

    if (balance) {
      learner_v_cuda <- learner_v[grep("cuda", learner_v$device), ]
      learner_v_cuda$device <- sprintf(
        "cuda:%d", (seq_len(nrow(learner_v_cuda)) - 1) %% num_device
      )
      learner_v[grep("cuda", learner_v$device), ] <- learner_v_cuda
    }

    return(learner_v)
  }


#' Generate manual rset object from spatiotemporal cross-validation indices
#' @keywords Baselearner
#' @param cvindex One of:
#'   * integer row indices for `id_out` in a `rset` object.
#'   * List of integer row indices stored in elements named `analysis` and
#'    `assessment`.
#' @param data data.frame object from which the `cvindex` is used
#'   to create `rset` object
#' @param ref_list List of custom reference group indices.
#'   Default is `attr(cvindex, "ref_list")`, where it is assumed that `cvindex`
#'   contains an `list` attribute named "ref_list".
#'   if not NULL, it will be used as a reference instead of max(cvindex).
#' @param cv_mode character(1). Spatiotemporal cross-validation indexing
#'   method label.
#' @return rset object of `rsample` package. A tibble with a list column of
#' training-test data.frames and a column of labels.
#' @author Insang Song
#' @importFrom rsample make_splits manual_rset
#' @export
convert_cv_index_rset <-
  function(
    cvindex,
    data,
    ref_list = attr(cvindex, "ref_list"),
    cv_mode = c("spatiotemporal", "spatial", "temporal")
  ) {
    cv_mode <- match.arg(cv_mode)
    # if (length(cvindex) != nrow(data)) {
    #   stop("cvindex length should be equal to nrow(data).")
    # }

    len_cvi <- seq_along(cvindex)

    if (is.list(cvindex)) {
      list_split_dfs <- Map(function(x) {
        rsample::make_splits(x = x, data = data)
      }, cvindex)
    } else {
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
    }

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
#' @return A data frame with the XY coordinates attached.
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

    # data_full_lean <- data_full[, c(locs_id, time_id), with = FALSE]
    data_full_attach <-
      collapse::join(
        data_full, data_sfd, on = locs_id, how = "left"
      )
    return(data_full_attach)
  }



#' Generate spatio-temporal cross-validation index with anticlust
#'
#' This function generates a spatio-temporal cross-validation index
#' based on the anticlust package. The function first calculates the
#' spatial clustering index using the [anticlust::balanced_clustering()]
#' function as default, and if `cv_pairs` is provided, it generates rank-based
#' pairs based on the proximity between cluster centroids.
#' `cv_pairs` can be NULL, in which case only the spatial clustering index
#' is generated. `ngroup_init` should be lower than `cv_pairs`, while
#' it imposes a condition that `nrow(data) %% ngroup_init` should be 0
#' and `cv_pairs` should be less than the number of 2-combinations of
#' `ngroup_init`. Each training set will get 50% overlap
#' with adjacent training sets. "Pairs (combinations)" are selected
#' based on the rank of centroids of `ngroup_init` number of initial
#' clusters, where users have two options.
#'
#'  * Mode "1" assigns at least one pair for each
#'    initial cluster, meaning that `ngroup_init` pairs are assigned for each
#'    initial cluster, then the remaining pairs will be ranked to finalize
#'    the `cv_pairs` sets.
#'  * Mode "2" will rank the pairwise distances
#'    directly, which may ignore some overly large initial clusters for pairing.
#'
#' Of course, mode "2" is faster than mode "1", thus users are advised
#' to use mode "2" when they are sure that the initial clusters are
#' spatially uniformly distributed.
#' @keywords Baselearner
#' @param data data.table with X, Y, and time information.
#' @param target_cols character(3). Names of columns for X, Y, and time.
#'   Default is c("lon", "lat", "time"). Order insensitive.
#' @param preprocessing character(1). Preprocessing method for the fields
#'   defined in `target_cols`. This serves to homogenize the scale of
#'   the data. Default is "none".
#'   * "none": no preprocessing.
#'   * "normalize": normalize the data.
#'   * "standardize": standardize the data.
#' @param ngroup_init integer(1). Initial number of splits for
#'  pairing groups. Default is 5L.
#' @param cv_pairs integer(1). Number of pairs for cross-validation.
#'   This value will be used to generate a rank-based pairs
#'   based on `target_cols` values.
#' @param pairing character(1) Pair selection method.
#'   * "1": search the nearest for each cluster then others
#'    are selected based on the rank.
#'   * "2": rank the pairwise distances directly
#' @param ... Additional arguments to be passed.
#' @note `nrow(data) %% ngroup_init` should be 0. This is a required
#'  condition for the anticlust::balanced_clustering().
#' @return List of numeric vectors with balanced cluster numbers and
#'   reference lists of assessment set pair numbers in attributes.
#' @author Insang Song
#' @importFrom rsample manual_rset
#' @importFrom anticlust balanced_clustering
#' @importFrom dplyr group_by summarize across ungroup all_of
#' @importFrom stats dist
#' @importFrom utils combn
#' @export
#' @examples
#' library(data.table)
#' data <- data.table(
#'   lon = runif(100),
#'   lat = runif(100),
#'   time =
#'   rep(
#'     seq.Date(from = as.Date("2021-01-01"), to = as.Date("2021-01-05"),
#'              by = "day"),
#'     20
#'   )
#' )
#' rset_spt <-
#'   generate_cv_index_spt(
#'     data, preprocessing = "normalize",
#'     ngroup_init = 5L, cv_pairs = 6L
#'   )
#' rset_spt
generate_cv_index_spt <-
  function(
    data,
    target_cols = c("lon", "lat", "time"),
    preprocessing = c("none", "normalize", "standardize"),
    ngroup_init = 5L,
    cv_pairs = NULL,
    pairing = c("1", "2"),
    ...
  ) {
    if (length(target_cols) != 3) {
      stop("Please provide three target columns.")
    }
    if (!is.null(cv_pairs)) {
      # ngroup_init check
      if (ngroup_init >= cv_pairs) {
        stop("ngroup_init should be less than cv_pairs.")
      }
      # 2-combinations of ngroup_init check
      if (ncol(utils::combn(seq_len(ngroup_init), 2)) < cv_pairs) {
        stop(
          paste0(
            "cv_pairs cannot be larger than ",
            "the number of 2-combinations of ngroup_init."
          )
        )
      }
    }
    # data_orig <- data
    data <- data[, target_cols, with = FALSE]
    data$time <- as.numeric(data$time)

    # select preprocessing plan
    # Yes, normalize/standardize spatiotemporal coordinates
    # may make little sense, but it would homogenize the scale of drastically
    # different value ranges of the coordinates (i.e., seconds in POSIXct)
    data_proc <-
      switch(
        preprocessing,
        none = data,
        normalize = (data + abs(apply(data, 2, min))) /
          (apply(data, 2, max) + abs(apply(data, 2, min))),
        standardize = collapse::fscale(data)
      )

    # !!! ngroup_init should be a divisor of nrow(data_proc) !!!
    index_cv <- anticlust::balanced_clustering(data_proc, ngroup_init)
    cv_index <- NULL
    # ref_list <- NULL
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
        ref_list <- NULL
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

      # ref_list contains the index of the group pairs
      ref_list <-
        Map(c, data_exd_rowid[search_idx], data_exd_colid[search_idx])
    } else {
      ref_list <- NULL
    }
    attr(index_cv, "ref_list") <- ref_list
    # generate row index for restoring rset
    # 0.3.9: ref_list is added to an attribute of index_cv

    return(index_cv)
  }



# non site-wise; just using temporal information
#' Generate temporal cross-validation index
#' @keywords Baselearner
#' @param data data.table with X, Y, and time information.
#' @param time_col character(1). Field name with time information.
#' @param cv_fold integer(1). Number of cross-validation folds.
#' @param window integer(1). Window size for each fold.
#'   Simply meaning overlaps between folds. Unit is
#'   the base unit of temporal values stored in `time_col`.
#'   Window size is put into `as.difftime` function, then the half of it
#'   (if odd, rounded number + 1 is applied) is used for overlaps
#'   in the middle folds.
#' @return List of numeric vector with out-of-sample indices.
#' @examples
#' data <- data.frame(
#'  time = seq.Date(from = as.Date("2021-01-01"), by = "day", length.out = 100),
#'  value = rnorm(100)
#' )
#' rset_ts <-
#'   generate_cv_index_ts(data, time_col = "time", cv_fold = 10, window = 14)
#' @importFrom stats quantile
#' @export
generate_cv_index_ts <-
  function(
    data,
    time_col = "time",
    cv_fold = 10L,
    window = 14L
  ) {
    tcol <- unlist(data[[time_col]])
    time_vec <- as.POSIXct(sort(unique(tcol)))
    # time_range interpretation
    time_vec_quantile <- quantile(time_vec, probs = seq(0, 1, 0.1))
    time_vec_quantile <- as.Date(time_vec_quantile)
    # define overlaps (half of window size)
    tdiff <- as.difftime(window / 2, units = "days")
    tdiff_h <- round(tdiff / 2) + 1

    cv_index <-
      lapply(
        seq_len(cv_fold),
        function(x) {
          if (x == 1) {
            # don't be confused! in_id is the training set
            in_id <- which(tcol > time_vec_quantile[x + 1] - tdiff)
            out_id <- which(tcol <= time_vec_quantile[x + 1] + tdiff)
          } else if (x == cv_fold) {
            # last fold
            in_id <- which(tcol <= time_vec_quantile[x] + tdiff)
            out_id <- which(tcol > time_vec_quantile[x] - tdiff)
          } else {
            in_id <-
              which(
                tcol < time_vec_quantile[x] + tdiff_h |
                  tcol >= time_vec_quantile[x + 1] - tdiff_h
              )
            out_id <-
              which(
                tcol >= time_vec_quantile[x] - tdiff_h &
                  tcol <= time_vec_quantile[x + 1] + tdiff_h
              )
          }
          return(list(analysis = in_id, assessment = out_id))
        }
      )

    return(cv_index)
  }


#' Prepare spatial and spatiotemporal cross validation sets
#' @keywords Baselearner
#' @param data data.table with X, Y, and time information.
#' @param target_cols character(3). Names of columns for X, Y.
#'   Default is `c("lon", "lat")`. It is passed to sf::st_as_sf to
#'   subsequently generate spatial cross-validation indices using
#'   `spatialsample::spatial_block_cv`.
#' @param cv_make_fun function(1). Function to generate spatial
#'   cross-validation indices.
#'   Default is `spatialsample::spatial_block_cv`.
#' @param ... Additional arguments to be passed to
#' `patialsample::spatial_block_cv`.
#' @seealso [`spatialsample::spatial_block_cv`],
#'   [`spatialsample::spatial_clustering_cv`],
#'   [`spatialsample::spatial_buffer_vfold_cv`]
#' @seealso [`spatialsample::spatial_block_cv`].
#' @return A list of numeric vectors with in- and out-of-sample row indices or
#'   a numeric vector with out-of-sample indices.
#' @importFrom rlang inject
#' @importFrom sf st_as_sf
#' @importFrom spatialsample spatial_block_cv
#' @importFrom rsample manual_rset
#' @importFrom dplyr %>% slice_sample
#' @importFrom methods getPackageName
#' @export
generate_cv_index_sp <-
  function(
    data,
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    ...
  ) {

    data_sf <- sf::st_as_sf(data, coords = target_cols, remove = FALSE)
    cv_index <-
      rlang::inject(
        cv_make_fun(
          data_sf,
          !!!list(...)
        )
      )

    # retrieve in_id
    data_rowid <- seq_len(nrow(data))
    newcv <- data_rowid

    if (
      !all(
        !is.na(Reduce(c, Map(function(x) is.na(x$out_id), cv_index$splits)))
      )
    ) {
      newcv <-
        lapply(
          cv_index$splits,
          function(x) list(analysis = x$in_id, assessment = x$out_id)
        )
    } else {
      cv_index <- lapply(cv_index$splits, function(x) x$in_id)
      for (i in seq_along(cv_index)) {
        newcv[setdiff(data_rowid, cv_index[[i]])] <- i
      }
    }

    return(newcv)
  }



#' Visualize the spatio-temporal cross-validation index
#' @keywords Baselearner
#' @param rsplit rsample::manual_rset() object.
#' @param cex numeric(1). Size of the points in the plot.
#' @param angle numeric(1). Viewing angle of 3D plot.
#' @return None. A plot will be generated.
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom graphics par
#' @export
vis_spt_rset <-
  function(rsplit, cex = 0.02, angle = 60) {
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
        cex.symbols = cex, pch = 19,
        angle = angle
      )
    }
  }



#' Choose cross-validation strategy for the base learner
#' @keywords Baselearner
#' @param learner character(1). Learner type. Should be one of:
#'  * "spatial": spatial cross-validation.
#'  * "temporal": temporal cross-validation.
#'  * "spatiotemporal": spatiotemporal cross-validation.
#' @param ... Additional arguments to be passed.
#' @note This function's returned value is used as an input for
#' `fit_base_brulee`, `fit_base_lightgbm`, and `fit_base_elnet`.
#' Learner values can be used as a branching point for the cross-validation
#' strategy.
#' @return [rsample::manual_rset()] output object.
#' @export
switch_generate_cv_rset <-
  function(
    learner = c("spatial", "temporal", "spatiotemporal"),
    ...
  ) {
    learner <- match.arg(learner)
    target_fun <-
      switch(
        learner,
        spatial = generate_cv_index_sp,
        temporal = generate_cv_index_ts,
        spatiotemporal = generate_cv_index_spt
      )
    cvindex <- inject_match(target_fun, list(...))
    return(cvindex)
  }
