################################################################################
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


################################################################################
#' Define a base learner model based on parsnip and tune
#' @keywords Baselearner
#' @param model_type character(1). Model type to be used.
#'  Default is "mlp". Available options are "mlp", "xgb", "lgb", "elnet".
#' @param device character(1). The device to be used for training.
#' GPU acceleration is possible for `brulee`-engine multi-layer
#' perceptron (`model_type = "mlp"`;`device = "cuda"`) and for `lightgbm`-engine
#' boosted tree (`model_type = "lgb"; device = "gpu"`). Ensure your system is
#' equipped with CUDA-enabled graphical processing units if utilizing `cuda` or
#' `gpu`.
#' @return A parsnip model object.
#' @importFrom parsnip mlp set_engine set_mode boost_tree linear_reg tune
#' @export
switch_model <-
  function(
    model_type = c("mlp", "mlp2", "xgb", "lgb", "elnet", "brulee_linear"),
    device = c("cpu", "cuda", "gpu")
  ) {
    device <- match.arg(device)
    switch(
      model_type,
      mlp = parsnip::mlp(
        hidden_units = parsnip::tune(),
        dropout = parsnip::tune(),
        epochs = 1000,
        activation = parsnip::tune(),
        learn_rate = parsnip::tune()
      ) |>
        parsnip::set_engine("brulee", device = device) |>
        parsnip::set_mode("regression"),
      mlp2 = parsnip::mlp(
        hidden_units = parsnip::tune(),
        dropout = parsnip::tune(),
        epochs = 1000,
        activation = parsnip::tune(),
        learn_rate = parsnip::tune()
      ) |>
        parsnip::set_engine(
          "brulee_two_layer",
          hidden_units_2 = parsnip::tune(),
          activation_2 = parsnip::tune(),
          device = device
        ) |>
        parsnip::set_mode("regression"),
      lgb = parsnip::boost_tree(
        mtry = parsnip::tune(),
        trees = parsnip::tune(),
        learn_rate = parsnip::tune(),
        tree_depth = parsnip::tune()
      ) |>
        parsnip::set_engine("lightgbm", device = device) |>
        parsnip::set_mode("regression"),
      xgb = parsnip::boost_tree(
        mtry = parsnip::tune(),
        trees = parsnip::tune(),
        learn_rate = parsnip::tune()
      ) |>
        parsnip::set_engine("xgboost", device = device) |>
        parsnip::set_mode("regression"),
      elnet = parsnip::linear_reg(
        mixture = parsnip::tune(),
        penalty = parsnip::tune()
      ) |>
        parsnip::set_engine("glmnet") |>
        parsnip::set_mode("regression"),
      brulee_linear = parsnip::linear_reg(
        mixture = parsnip::tune(),
        penalty = parsnip::tune()
      ) |>
        parsnip::set_engine("brulee", device = device) |>
        parsnip::set_mode("regression")
    )
  }


################################################################################
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
#' @param rset A space/time CV set generated from beethoven
#' @param full_data The full data frame to be used for training after the
#' tuning. Passed to `parsnip::fit`.
#' @param model The parsnip model object. Preferably generated from
#'   `switch_model`.
#' @param tune_grid_size numeric(1), finetune grid size.
#' @param yvar The target variable.
#' @param xvar The predictor variables.
#' @param drop_vars character vector or numeric.
#' The variables to be dropped from the data.frame.
#' @param normalize logical(1). If `TRUE`, all numeric predictors are
#' normalized. Default is `FALSE`.
#' @param metric character(1). The metric to be used for selecting the best.
#' Must be one of "rmse", "rsq", "msd", "mae". Default = "rmse"
#' @param ... Additional arguments to be passed.
#'
#' @return The fitted workflow.
#' @importFrom recipes recipe update_role step_normalize all_numeric_predictors
#' @importFrom recipes all_integer_predictors
#' @importFrom parsnip mlp set_engine set_mode fit
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom tune tune_grid fit_best collect_metrics select_best
#' @importFrom tune finalize_workflow
#' @importFrom tidyselect all_of
#' @importFrom yardstick metric_set rmse
#' @importFrom rsample vfold_cv training testing
#' @importFrom finetune control_race tune_race_anova
#' @importFrom dplyr arrange
#' @importFrom butcher butcher
#' @export
fit_base_learner <-
  function(
    rset = NULL,
    full_data = NULL,
    model = NULL,
    tune_grid_size = 10L,
    yvar = "Arithmetic.Mean",
    xvar = NULL,
    drop_vars = NULL,
    normalize = TRUE,
    metric = "rmse",
    ...
  ) {
    stopifnot("parsnip model must be defined." = !is.null(model))

    # detect model name
    model_name <- model$engine

    # supported metrics
    chr_metrics <- c("rmse", "rsq", "msd", "mae")
    stopifnot(metric %in% chr_metrics)

    # order metrics (first metric is used for tune_race_anova)
    chr_metrics_ordered <- c(metric, setdiff(chr_metrics, metric))

    # namespaced symbols
    sym_metrics <- lapply(chr_metrics_ordered, function(m) {
      # Create a call to :: (e.g., quote(yardstick::rmse))
      call("::", quote(yardstick), as.symbol(m))
    })

    # full call
    call_metric <- as.call(c(quote(yardstick::metric_set), sym_metrics))

    # first split training for covariate names in recipe
    training_data <- rsample::training(rset$splits[[1]])

    # define recipe
    base_recipe <-
      recipes::recipe(training_data[1, ]) |>
      recipes::update_role(!!xvar, new_role = "predictor") |>
      recipes::update_role(!!yvar, new_role = "outcome") |>
      recipes::step_zv(recipes::all_predictors())

    if (!is.null(drop_vars)) {
      base_recipe <- base_recipe |>
        recipes::update_role(dplyr::all_of(drop_vars), new_role = "id") |>
        recipes::update_role_requirements("id", bake = FALSE)
    }

    if (normalize) {
      base_recipe <-
        base_recipe |>
        recipes::step_normalize(
          recipes::all_numeric_predictors(),
          -recipes::all_integer_predictors()
        )
    }

    # initial workflow
    base_wf <-
      workflows::workflow() |>
      workflows::add_recipe(base_recipe) |>
      workflows::add_model(model)

    # finetune race control
    wf_config <- finetune::control_race(
      verbose_elim = TRUE,
      save_workflow = TRUE,
      verbose = TRUE,
      save_pred = TRUE
    )

    # initial base tuning
    base_wftune <- tryCatch(
      {
        base_wf |>
          finetune::tune_race_anova(
            resamples = rset,
            grid = tune_grid_size,
            metrics = eval(call_metric),
            control = wf_config
          )
      },
      error = function(e) {
        # if error due to zero-variance in the `rset` splits
        message("Re-tuning with additional recipes::step_nzv()...")

        # update the recipe with near-zero variance filter
        base_recipe_nzv <- base_recipe |>
          recipes::step_nzv(recipes::all_predictors())

        # rebuild the workflow
        base_wf <-
          workflows::workflow() |>
          workflows::add_recipe(base_recipe_nzv) |>
          workflows::add_model(model)

        # re-run base tuning
        base_wf |>
          finetune::tune_race_anova(
            resamples = rset,
            grid = tune_grid_size,
            metrics = eval(call_metric),
            control = wf_config
          )
      }
    )

    # Select best model
    best_params <- tune::select_best(base_wftune, metric = metric)

    oof_predictions <- tune::collect_predictions(
      base_wftune,
      parameters = best_params
    )

    # Finalize workflow with best parameters
    final_wf <- tune::finalize_workflow(base_wf, best_params) |>
      # Fit the finalized workflow to the full data
      parsnip::fit(data = full_data)

    base_results <- list(
      "workflow" = final_wf,
      "predictions" = oof_predictions,
      "metrics" = tune::collect_metrics(base_wftune)
    )

    return(base_results)
  }


################################################################################
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
#' @param workflow logical(1). If TRUE, the best fit model workflow is returned.
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
    workflow = TRUE,
    data_full = NULL,
    metric = "rmse"
  ) {
    stopifnot("data_full must be entered." = !is.null(data_full))
    tune_mode <- match.arg(tune_mode)
    base_wf <-
      workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(model)

    if (tune_mode == "grid") {
      wf_config <-
        tune::control_grid(
          verbose = TRUE,
          save_pred = FALSE,
          save_workflow = TRUE
        )
      base_wftune <-
        base_wf |>
        tune::tune_grid(
          resamples = resample,
          grid = grid,
          metrics = yardstick::metric_set(
            yardstick::rmse,
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
        base_wf |>
        tune::tune_bayes(
          resamples = resample,
          iter = iter_bayes,
          metrics = yardstick::metric_set(
            yardstick::rmse,
            yardstick::mae,
            yardstick::mape,
            yardstick::rsq
          ),
          control = wf_config
        )
    }

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

      # unlist multi-layered hidden units if mlp model
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
          best_performance = base_wftune,
          fit_workflow = base_wfresult
        )
    }

    if (!workflow) {
      base_wflist <- base_wflist[-4]
    }
    if (trim_resamples) {
      base_wflist <- base_wflist[-3]
    }

    return(base_wflist)
  }


################################################################################
#' Shuffle cross-validation mode for each learner type
#' @keywords Baselearner
#' @param learner character(1). The base learner to be used.
#'  Default is "mlp". Available options are "mlp", "lgb", "elnet".
#' @param cv_mode character(1). The cross-validation mode to be used.
#'  Default is "spatiotemporal". Available options are "spatiotemporal",
#'  "spatial", "temporal".
#' @param num_models integer(1). The number of repetitions for each `cv_mode`.
#' @param num_device integer(1). The number of CUDA devices to be used.
#'   Each device will be assigned to each eligible learner (i.e., lgb, mlp).
#' @param crs Coordinate reference system in `sf` style. Default is 5070L
#' Albers Equal Area Projected
#' @param cellsize cellsize for cross-validation spatial blocks. crs units.
#' Default is 100km
#' @param balance logical(1). If TRUE, the number of CUDA devices will be
#' equally distributed based on the number of eligible devices.
#' @return A data frame with three columns: learner, cv_mode, and device.
#' @export
assign_learner_cv <-
  function(
    learner = c("lgb", "mlp", "elnet"),
    cv_mode = c("spatiotemporal", "spatial", "temporal"),
    num_models = 100L,
    num_device = ifelse(torch::cuda_device_count() > 1, 2, 1),
    crs = 5070L,
    cellsize = 100000L,
    balance = FALSE
  ) {
    learner_eligible <- c("mlp", "xgb")
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
        num_models_rep <- rep(cv_mode, each = num_models)
        df <-
          data.frame(
            learner = rep(x, num_models * length(cv_mode)),
            cv_mode = num_models_rep[sample(
              length(num_models_rep),
              length(num_models_rep)
            )],
            device = y,
            crs = crs,
            cellsize = cellsize
          )
        return(df)
      },
      learner_l,
      cuda_get,
      SIMPLIFY = FALSE
    )
    learner_v <- do.call(rbind, learner_l)

    if (balance) {
      learner_v_cuda <- learner_v[grep("cuda", learner_v$device), ]
      learner_v_cuda$device <- sprintf(
        "cuda:%d",
        (seq_len(nrow(learner_v_cuda)) - 1) %% num_device
      )
      learner_v[grep("cuda", learner_v$device), ] <- learner_v_cuda
    }

    return(learner_v)
  }


################################################################################
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
      list_split_dfs <- Map(
        function(x) {
          rsample::make_splits(x = x, data = data)
        },
        cvindex
      )
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
            list(
              analysis = which(!cvindex %in% x),
              assessment = which(cvindex %in% x)
            )
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
    rset_stcv <- rsample::manual_rset(
      list_split_dfs,
      modename
    )
    return(rset_stcv)
  }


################################################################################
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
        data_full,
        data_sfd,
        on = locs_id,
        how = "left"
      )
    return(data_full_attach)
  }


################################################################################
#' Generate spatio-temporal cross-validation index with
#' `spatialsample::spatial_block_cv` and year-based temporal folds
#' @description
#' This function generates spatio-temporal cross-validation indices with
#' `v` spatial blocks and year-based temporal folds. The spatial blocks
#' are generated with `spatialsample::spatial_block_cv` function, and the
#' temporal folds are generated based on the years availble in `data$time`.
#' Total number of folds is equal to
#' `v * length(unique(substr(data$time, 1, 4)))`.
#' @keywords Baselearner
#' @param data data.table or data.frame with `id`, `coords`, and `time` columns.
#' @param crs The coordinate reference system (CRS) of the spatial object
#' @param cellsize The size of each spatial block in meters. Uses the crs units
#' @param locs_id The column name in `data` that represents the
#' location identifier.
#' @param coords The column names in the spatial object that represent the
#' XY coordinates. Default is `c("lon", "lat")`.
#' @param v integer(1). The number of partitions for the resampling.
#' @param time_id The column name in `data` that represents the time values.
#' @importFrom dplyr left_join select tibble
#' @importFrom spatialsample spatial_block_cv
#' @importFrom lubridate year week
#' @importFrom sf st_transform
#' @importFrom purrr map_dfr
#' @importFrom rsample assessment
#' @seealso [`spatialsample::spatial_block_cv`]
#' @export
generate_cv_index_spt <- function(
  data,
  crs = NULL,
  cellsize = NULL,
  locs_id = "site_id",
  coords = c("lon", "lat"),
  v = 10L,
  time_id = "time"
) {
  #########################       SPATIAL FOLDS       ##########################
  stopifnot(c(locs_id, coords, time_id) %in% names(data))

  data_trim <- unique(data.frame(data)[, c(locs_id, coords)])

  if (is.null(crs)) {
    df_sf <- sf::st_as_sf(
      data_trim,
      coords = coords,
      crs = 4326,
      remove = FALSE
    )
    data_sf <- sf::st_transform(df_sf, crs = 5070)
  } else {
    df_sf <- sf::st_as_sf(
      data_trim,
      coords = coords,
      crs = 4326,
      remove = FALSE
    )
    data_sf <- sf::st_transform(df_sf, crs = crs)
  }

  # generate spatial splits with `spatialsample::spatial_block_cv`
  if (!is.null(cellsize)) {
    sp_index <-
      rlang::inject(
        spatialsample::spatial_block_cv(
          data_sf,
          v = v,
          cellsize = cellsize,
          method = "random"
        )
      )
  } else {
    sp_index <- spatialsample::spatial_block_cv(
      data = data_sf,
      v = v,
      method = "random"
    )
  }

  # Create a data frame that assigns fold
  # labels to each row in the original data
  fold_labels <- purrr::map_dfr(seq_along(sp_index$splits), function(k) {
    assessment_ids <- rsample::assessment(sp_index$splits[[k]])$site_id

    dplyr::tibble(
      site_id = assessment_ids,
      fold = k
    )
  })

  data_spatial_cv <- data.frame(data)[, c(locs_id, coords)] |>
    sf::st_as_sf(coords = coords, remove = FALSE) |>
    # Join the fold labels back to the original data
    dplyr::left_join(fold_labels, by = "site_id")

  #########################       TEMPORAL FOLDS       #########################

  # identify site's unique year + week combination
  data_spatial_cv$yw <- paste0(
    lubridate::year(data[[time_id]]),
    sprintf("%02d", lubridate::week(data[[time_id]]))
  )

  data_spatial_cv <- data_spatial_cv |>
    dplyr::mutate(
      st_int = interaction(.data$fold, as.double(.data$yw), drop = FALSE)
    )

  # Create a unique list of (id1, id2) combinations
  unique_st_int <- data_spatial_cv$st_int |> unique() |> as.data.frame()
  colnames(unique_st_int) <- "st_int"

  # Assign P groups cyclically (to balance size)
  unique_st_int <- unique_st_int |>
    dplyr::mutate(spacetime_cv = rep(1:v, length.out = dplyr::n()))

  # Merge back to the full dataset
  data_sp <- data_spatial_cv |> dplyr::left_join(unique_st_int, by = "st_int")

  # length == nrow(data)
  spt_index_num <- data_sp$spacetime_cv

  return(spt_index_num)
}

################################################################################
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


################################################################################
#' Prepare spatial and spatiotemporal cross validation sets
#' @keywords Baselearner
#' @param data data.table with X, Y, and time information.
#' @param target_cols character(3). Names of columns for X, Y.
#'   Default is `c("lon", "lat")`. It is passed to sf::st_as_sf to
#'   subsequently generate spatial cross-validation indices using
#'   `spatialsample::spatial_block_cv`.
#' @param ... Additional arguments to be passed to
#' `patialsample::spatial_block_cv`.
#' @seealso [`spatialsample::spatial_block_cv`].
#' @return A list of numeric vectors with in- and out-of-sample row indices or
#'   a numeric vector with out-of-sample indices.
#' @importFrom rlang inject
#' @importFrom sf st_as_sf
#' @importFrom rsample manual_rset
#' @importFrom dplyr slice_sample
#' @importFrom methods getPackageName
#' @export
generate_cv_index_sp <-
  function(
    data,
    target_cols = c("lon", "lat"),
    ...
  ) {
    data_sf <- sf::st_as_sf(data, coords = target_cols, remove = FALSE)
    cv_index <-
      rlang::inject(
        spatialsample::spatial_block_cv(
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


################################################################################
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
        cleared$lon,
        cleared$lat,
        cleared$time,
        color = rev(as.integer(cleared$indx) + 1),
        cex.symbols = cex,
        pch = 19,
        angle = angle
      )
    }
  }


################################################################################
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
    cvindex <- beethoven::inject_match(target_fun, list(...))
    return(cvindex)
  }

################################################################################
#' Predict base learners at for cross validation folds.
#' @keywords Baselearner
#' @param fit list(1). List with 1) trained workflow and 2) performance metrics.
#' Workflow must be first item in list.
#' @param test data.table(1). `data.table` with un-trained testing data.
#' @param target_cols characters(1). Columns to retain from the full test data
#' `data.frame``.
#' @param name character(1). Name for prediction column. Should identify the
#' model and engine and/or hyperparameters.
#' @importFrom stats predict
#' @importFrom data.table data.table
#' @return `data.table` with attached site identifiers
#' @export
fit_prediction <- function(
  fit = NULL,
  test = NULL,
  target_cols = c("site_id", "time", "lon", "lat"),
  name = NULL
) {
  stopifnot(!is.null(fit))
  stopifnot(!is.null(test))
  stopifnot(!is.null(name))
  stopifnot(!is.null(target_cols))
  stopifnot("data.frame" %in% class(test))
  stopifnot("workflow" %in% class(fit[[1]]))

  workflow <- fit[[1]]
  num_prediction <- stats::predict(workflow, test)
  dt_prediction <- data.table::data.table(
    data.frame(
      test[, target_cols],
      num_prediction
    )
  )
  names(dt_prediction) <- c(
    target_cols,
    as.character(name)
  )
  return(dt_prediction)
}
################################################################################
# Calculation tier post-processing functions.

#' Add Time Column
#'
#' This function adds a time column to a data frame.
#'
#' @keywords Post-calculation
#' @param df The data frame to which the time column will be added.
#' @param time_value The value to be assigned to the time column.
#' @param time_id The name of the time column (default is "time").
#'
#' @return The data frame with the added time column.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' add_time_col(df, "2022-01-01")
#' }
#' @export
add_time_col <- function(df, time_value, time_id = "time") {
  if (!time_id %in% names(df)) {
    df[[time_id]] <- time_value
  }
  return(df)
}


################################################################################
#' Merge input data.frame objects
#' @param by character. Joining keys. See [`data.table::merge`] for details.
#' @param time logical(1). Whether or not include time identifier.
#' Set this `TRUE` will supersede `by` value by appending time identifier.
#' @param ... data.frame objects to merge
#' @return data.table
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
      Reduce(
        function(x, y) {
          data.table::merge.data.table(
            x,
            y,
            by = by,
            all.x = TRUE,
            suffixes = c("_Ma", "_Mb")
          )
        },
        ellipsis_clean
      )
    return(joined)
  }


################################################################################
#' Change time column name
#' @param df data.frame
#' @param candidates character. Candidate column names.
#' @param replace character. New column name.
#' @return data.frame
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


################################################################################
#' Convert time column to character
#' @keywords Post-calculation
#' @param df data.table
#' @note This function takes preprocessed data.table with
#'   a column named `"time"`.
#' @importFrom data.table as.data.table copy
#' @importFrom data.table :=
#' @export
post_calc_convert_time <-
  function(
    df
  ) {
    stopifnot("time" %in% names(df))
    df1 <- data.table::copy(data.table::as.data.table(df))
    df1[, time := as.character(time)]
    return(df1)
  }


################################################################################
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
#' @importFrom data.table :=
#' @return data.frame
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
        df_date,
        df_year,
        by = c(spid, "year"),
        all.x = TRUE
      )

    df_joined <- df_joined[, c("year") := NULL]
    return(df_joined)
  }


################################################################################
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
#' @return integer vector of length (time_end - time_start + 1).
#' Each element will get the nearest preceeding available year.
#' @note
#' The minimum of `time_available` will be filled in front of the first
#' available year when the minimum of `time_available` is greater
#' than `time_start`.
#' @examples
#' \dontrun{
#' process_calc_year_expand(2018, 2022, "year", c(2017, 2020, 2021))
#' process_calc_year_expand(2018, 2022, "year", c(2020, 2021))
#' }
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
          rep(min(time_available), min(time_available) - time_start),
          time_target_seq
        )
    }
    return(time_target_seq)
  }


################################################################################
#' Expand a data frame by year
#'
#' This function expands a data frame by year, creating multiple rows
#'  for each year based on the time period specified.
#' @keywords Post-calculation
#' @param df The input data frame. The data frame should have the same
#' number of rows per year, meaning that it assumes this argument is
#' a spatial-only feature data.frame.
#' @param locs_id The column name of the location identifier in the data frame.
#' @param time_field The column name of the time field in the data frame.
#' @param time_start The start of the time period.
#' @param time_end The end of the time period.
#' @param time_unit The unit of time to expand the data frame. Only for record.
#' @param time_available A vector of available time periods.
#' @param ... Placeholders.
#' @note Year expansion rule is to assign the nearest past year
#' in the available years; if there is no past year in the available years,
#' the first available year is rolled back to the start of the time period.
#' @return The expanded data frame with multiple rows for each year.
#' @seealso [`post_calc_year_expand()`]
#' @examples
#' \dontrun{
#' df <- data.frame(year = c(2010, 2010, 2011, 2012),
#'                  value = c(1, 2, 3, 4))
#' df_expanded <-
#'   post_calc_df_year_expand(df, locs_id = "site_id", time_field = "year",
#'                            time_start = 2011, time_end = 2012,
#'                            time_unit = "year")
#' print(df_expanded)
#' }
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


################################################################################
#' Merge spatial and spatiotemporal covariate data
#' @keywords Post-calculation
#' @param locs Location. e.g., AQS sites.
#' @param locs_id character(1). Location identifier.
#' @param time_id character(1). Location identifier.
#' @param target_years integer. Used to dummify nominal year.
#' @param df_sp data.frame. Spatial-only covariates.
#' @param df_spt data.frame. Spatiotemporal covariates.
#' @note This version assumes the time_id contains Date-like strings.
#' @return data.frame
#' @importFrom data.table merge.data.table
#' @importFrom amadeus calculate_temporal_dummies
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
        locs,
        df_sp,
        by = c(locs_id)
      )
    locs_merged <-
      data.table::merge.data.table(
        locs_merged,
        df_spt,
        by = c(locs_id, time_id)
      )
    # need POSIXt class for amadeus function
    locs_merged[[time_id]] <- as.POSIXct(locs_merged[[time_id]])
    locs_merged <-
      amadeus::calculate_temporal_dummies(
        locs = locs_merged,
        locs_id = locs_id,
        year = target_years
      )
    # reset time as character
    locs_merged[[time_id]] <- as.character(locs_merged[[time_id]])
    return(locs_merged)
  }


################################################################################
#' Remove columns from a data frame based on regular expression patterns.
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
#' @keywords Post-calculation
#' @return The modified data frame with the specified columns removed.
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


################################################################################
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
#' @return A merged data table.
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
#' @importFrom rlang as_name sym
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
          df_fine,
          df_coarse,
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
          # derive the available years from the coarsely resolved data
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
            df_fine,
            df_coarse,
            by = c(field_sp, field_t),
            all.x = TRUE
          )
        }
      }
    }
    return(joined)
  }


################################################################################
#' Impute missing values and attach lagged features
#' @keywords Post-calculation
#' @note
#' This function performs imputation on a given data table
#'   by replacing missing values with imputed values.
#'   It follows a series of steps including data cleaning, name cleaning,
#'   geoscf column renaming, NDVI 16-day backward filling,
#'   zero-variance exclusion, excessive "true zeros" exclusion,
#'   and imputation using missRanger.
#' A few points should be discussed to sophisticate the imputation
#' process: exclusion threshold for rates of zero observations,
#' which might lead to significant improvement in the imputation
#' process especially in terms of speed and accuracy.
#' @param dt The input data table to be imputed.
#' @param period The period for lagged features.
#' @param nthreads_dt The number of threads to be used for
#'   data.table operations.
#' @param nthreads_collapse The number of threads to be used for
#'   collapse operations.
#' @param nthreads_imputation The number of threads to be used for
#'   the imputation process.
#'
#' @return The imputed data table with lagged features.
#'
#' @importFrom collapse set_collapse replace_inf replace_na fvar fnth
#' @importFrom data.table setDTthreads setnafill
#' @importFrom qs2 qs_read
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
      if (!endsWith(dt, ".qs")) {
        stop(
          paste0(
            "If `dt` points to a file, provide full path to .qs file.\n"
          )
        )
      }
      dt <- qs2::qs_read(file.path(dt))
    }
    dt$time <- as.POSIXct(dt$time)
    # remove unnecessary columns
    query <- "^(site_id|time)\\.[0-9]+"
    dt <- dt[, !grepl(query, names(dt)), with = FALSE]

    # name cleaning
    dt <- stats::setNames(
      dt,
      sub("light_00000", "OTH_HMSWL_0_00000", names(dt))
    )
    dt <- stats::setNames(
      dt,
      sub("medium_00000", "OTH_HMSWM_0_00000", names(dt))
    )
    dt <- stats::setNames(
      dt,
      sub("heavy_00000", "OTH_HMSWH_0_00000", names(dt))
    )
    dt <- stats::setNames(
      dt,
      sub("population_", "POP_SEDAC_0_", names(dt))
    )

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
            names(dt),
            sprintf("%s$", geoscndf$variable[i]),
            geoscndf$code[i]
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
        dt,
        type = "nocb",
        nan = NA,
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
        maxiter = 10L,
        num.trees = 300L,
        num.threads = nthreads_imputation,
        mtry = 100L,
        sample.fraction = 0.1
      )

    imputed <- amadeus::calculate_temporal_dummies(imputed, "time")
    return(imputed)
    # lagged features: changing period (period[1] + 1 day)
    # period <- as.Date(period)
    # period[1] <- period[1] + as.difftime(1, units = "days")
    # period <- as.character(period)
    # index_lag <-
    #   sprintf("MET_%s", c("ATSFC", "ACPRC", "PRSFC", "SPHUM", "WNDSP"))
    # index_lag <- grep(paste(index_lag, collapse = "|"), names(dt))
    # target_lag <- imputed[, index_lag, with = FALSE]

    # output <- amadeus::calculate_lagged(target_lag, period, 1, "site_id")
    # return(output)
  }


################################################################################
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
#' @return If no existing predecessors are found, the function saves
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
        period_new[1],
        period_new[2],
        time_create
      )
    if (length(input_old) == 0) {
      qs2::qs_save(input_new, file = file.path(path_qs, name_qs))
      return(name_qs)
    } else {
      # vv <- list()
      qs2::qs_save(input_new, file = file.path(path_qs, name_qs))
      input_update <- list.files(path_qs, "*.*.qs$", full.names = TRUE)
      bound_large <-
        Reduce(
          function(x, y) {
            if (inherits(x, "data.frame")) {
              bound <- rbind(x, qs2::qs_read(y))
            } else {
              bound <- rbind(qs2::qs_read(x), qs2::qs_read(y))
            }
            return(bound)
          },
          input_update
        )
      return(bound_large)
    }
  }


################################################################################
#' Perform Principal Component Analysis
#' @keywords internal
#' @param data data.frame or data.table
#' @param num_comp integer(1). The number of components to retain as new
#' predictors. If `threshold` is defined, `num_comp` will be overridden.
#' @param threshold numeric(1). A fraction of the total variance that should
#' be covered by the components.
#' @param kernel logical(1). Whether to use a kernel PCA with
#' [`recipes::step_kpca()`]. Default is `FALSE`.
#' @seealso [`recipes::step_pca()`] [`recipes::step_kpca()`]
#' @importFrom recipes recipe bake prep step_normalize step_pca step_kpca
#' @return data.table with Principal Components sufficient to satisfy the
#' `threshold`.`
#' @export
reduce_pca <- function(
  data,
  num_comp = 5,
  threshold = NA,
  kernel = FALSE
) {
  stopifnot(inherits(data, "data.frame"))
  stopifnot(is.numeric(num_comp))

  data_rec <- recipes::recipe(~., data = data)
  data_pca <- data_rec |>
    recipes::step_normalize(recipes::all_numeric())

  if (kernel) {
    data_pca <- data_pca |>
      recipes::step_kpca(
        recipes::all_numeric(),
        num_comp = num_comp
      )
  } else {
    data_pca <- data_pca |>
      recipes::step_pca(
        recipes::all_numeric(),
        threshold = threshold,
        num_comp = num_comp
      )
  }

  data_prep <- recipes::prep(data_pca, data = data)
  data_pca <- recipes::bake(data_prep, new_data = data)

  return(list(pca = data_pca, recipe = data_prep))
}

################################################################################
#' Post-calculation Principal Component Analysis
#' @keywords Post-calculation
#' @description This function performs PCA on the input data frame to reduce
#' number of predictors.
#' @param data data.frame or data.table
#' @param locs_id The column name in the spatial object that represents the
#'   location identifier.
#' @param time_id The column name in the data frame that represents the time
#'   identifier.
#' @param yvar The target variable.
#' @param coords The column names that represent the XY coordinates. Default
#' is `c("lon", "lat")`.
#' @param num_comp integer(1). The number of components to retain as new
#' predictors.  If `threshold` is defined, `num_comp` will be overridden.
#' @param threshold numeric(1). A fraction of the total variance that should
#' be covered by the components.
#' @param pattern character(1). A regular expression pattern to match the
#' columns that should be included in the PCA.
#' @param groups character. A character vector of groups to perform PCA on.
#' Each character should be a regular expression pattern to match the columns
#' that should be included in the PCA. Default is `NULL`.
#' @param prefix character(1). A prefix to be added to the column names of the
#' Principal Components. Default is `NULL`.
#' @param kernel logical(1). Whether to use a kernel PCA with
#' [`recipes::step_kpca()`]. Default is `FALSE`.
#' @note  If `threshold` is defined, `num_comp` will be overridden.
#' @seealso [`reduce_pca()`] [`recipes::step_pca()`] [`recipes::step_kpca()`]
#' @importFrom data.table data.table
#' @return data.table with Principal Components sufficient to satisfy the
#' `threshold`, merged with `*_id` and `yvar` columns from original `data`.
#' @export
post_calc_pca <- function(
  data,
  locs_id = "site_id",
  time_id = "time",
  yvar = "Arithmetic.Mean",
  coords = c("lon", "lat"),
  num_comp = 5,
  threshold = NA,
  pattern = "FUGITIVE|STACK",
  groups = NULL,
  prefix = "PCA",
  kernel = FALSE
) {
  data <- data.table::data.table(data)
  chr_retaincols <- c(locs_id, time_id, yvar, coords)
  data_trim <- data[, chr_retaincols, with = FALSE]

  data_pca <- data[,
    grep(pattern, names(data)),
    with = FALSE
  ]

  if (is.null(groups)) {
    list_pca <- beethoven::reduce_pca(
      data = data_pca,
      threshold = threshold,
      num_comp = num_comp,
      kernel = kernel
    )
    return_pca <- list_pca$pca
    names(return_pca) <- paste0(prefix, "_", names(return_pca))
    list_recipe <- list_pca$recipe
  } else {
    list_pca <- list()
    list_recipe <- list()
    for (g in seq_along(groups)) {
      data_group <- data_pca[,
        grep(paste0("_", groups[g], "$"), names(data_pca)),
        with = FALSE
      ]
      group_pca <- beethoven::reduce_pca(
        data = data_group,
        threshold = threshold,
        num_comp = num_comp,
        kernel = kernel
      )
      names(group_pca$pca) <- paste0(
        prefix,
        "_",
        names(group_pca$pca),
        "_",
        groups[g]
      )
      list_pca <- c(list_pca, group_pca$pca)
      list_recipe <- c(list_recipe, list(group_pca$recipe))
    }
    return_pca <- do.call(cbind, list_pca)
  }

  stopifnot(nrow(data_trim) == nrow(return_pca))
  data_return <- data.table::data.table(cbind(data_trim, return_pca))

  return(list(pca = data_return, recipe = list_recipe))
}

################################################################################
#' Post-calculation column renaming
#' @keywords Post-calculation
#' @description This function renames the columns of the input `data` based on
#' the `prefix` and original column names.
#' @param data data.frame(1)
#' @param prefix character(1). The prefix to be added to the column names.
#' @param skip character. The column names to be skipped from renaming.
#' Default is `c("site_id", "time")`.
#' @return data.frame with renamed columns.
#' @export
post_calc_cols <- function(
  data,
  prefix = NULL,
  skip = c("site_id", "time")
) {
  stopifnot(inherits(data, "data.frame"))

  chr_names <- names(data)
  chr_edit <- chr_names[!chr_names %in% skip]

  list_split <- strsplit(chr_edit, "_")
  list_update <- lapply(
    list_split,
    function(x) {
      paste0(
        prefix,
        toupper(x[[1]]),
        "_",
        sprintf("%05d", as.integer(x[[length(x)]]))
      )
    }
  )
  chr_update <- unlist(list_update)
  names(data)[match(chr_edit, chr_names)] <- chr_update
  return(data)
}
################################################################################
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
  function(
    path = NULL,
    date = c("2018-01-01", "2018-01-01"),
    locs = NULL,
    locs_id = NULL,
    ...
  ) {
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
        c(
          "ACET",
          "ALD2",
          "ALK4",
          "BCPI",
          "BCPO",
          "BENZ",
          "C2H6",
          "C3H8",
          "CH4",
          "CO",
          "DST1",
          "DST2",
          "DST3",
          "DST4",
          "EOH",
          "H2O2",
          "HCHO",
          "HNO3",
          "HNO4",
          "ISOP",
          "MACR",
          "MEK",
          "MVK",
          "N2O5",
          "NH3",
          "NH4",
          "NIT",
          "NO",
          "NO2",
          "NOy",
          "OCPI",
          "OCPO",
          "PAN",
          "PM25_RH35_GCC",
          "PM25_RH35_GOCART",
          "PM25bc_RH35_GCC",
          "PM25du_RH35_GCC",
          "PM25ni_RH35_GCC",
          "PM25oc_RH35_GCC",
          "PM25soa_RH35_GCC",
          "PM25ss_RH35_GCC",
          "PM25su_RH35_GCC",
          "PRPE",
          "RCHO",
          "SALA",
          "SALC",
          "SO2",
          "SOAP",
          "SOAS",
          "TOLU",
          "XYLE"
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
        Reduce(
          function(dfa, dfb) dplyr::full_join(dfa, dfb, by = "ID"),
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
    rast_summary <- data.table::rbindlist(rast_summary, fill = TRUE)

    return(rast_summary)
  }


################################################################################
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
  ...
) {
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
    "Breakline Emphasis",
    "Systematic Subsample",
    "Median Statistic",
    "Minimum Statistic",
    "Mean Statistic",
    "Maximum Statistic",
    "Standard Deviation Statistic"
  )
  statistic_to <- c(
    "BRKL",
    "SSUB",
    "MEDN",
    "MINI",
    "MEAN",
    "MAXL",
    "STDV"
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
      paths,
      value = TRUE
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
      statistic_to,
      "_",
      sprintf("%05d", radius)
    )
  )
  #### return data.frame
  return(data.frame(sites_extracted))
}


################################################################################
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
      split(
        time_from,
        #ceiling(seq_along(time_from) / 29L))
        ceiling(as.integer(as.factor(time_from)) / 14L)
      )
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
      sites_extracted,
      by = c("site_id")
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
      time = regmatches(
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


################################################################################
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

################################################################################
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
  ) |>
    grep(
      pattern = paste0(
        "A",
        list[[index]],
        collapse = "|"
      ),
      value = TRUE
    )
  return(grep_files)
}


################################################################################
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
      stop(
        "preprocess should be one of process_modis_merge,
process_modis_swath, or process_blackmarble."
      )
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
      stop(
        "locs cannot be convertible to sf.
      Please convert locs into a sf object to proceed.\n"
      )
    }

    export_list <- c()
    package_list <-
      c(
        "sf",
        "terra",
        "exactextractr",
        "data.table",
        "stars",
        "dplyr",
        "parallelly",
        "rlang",
        "amadeus"
      )
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
            message(
              "The number of layers in the input raster do not match
                    the length of name_covariates.\n"
            )
          }

          res0 <-
            lapply(radiusindexlist, function(k) {
              name_radius <-
                sprintf("%s%05d", name_covariates, radius[k])
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
                  matrix(
                    -99999,
                    ncol = length(name_radius) + 1,
                    nrow = nrow(locs_input)
                  )
                )
                error_df <- stats::setNames(error_df, c(locs_id, name_radius))
                error_df[[locs_id]] <- unlist(locs_input[[locs_id]])
                error_df$time <- day_to_pick
                extracted <- error_df
              }
              return(extracted)
            })
          res <-
            Reduce(
              function(x, y) {
                dplyr::left_join(x, y, by = c(locs_id, "time"))
              },
              res0
            )
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


################################################################################
#' Calculate MODIS from preprocessed file
#' @param file character(1). File path of preprocessed GeoTIFF file.
#' @param site data.frame/sf. AQS sites. data.frame will be converted to sf.
#' @param site_id character(1). Unique identifier of `site`
#' @param radius numeric(1). Circular buffer radius. Distance unit is inherited
#'   from the sites' coordinate system.
#' @param colheader character. Column name header.
#' @param mark logical(1). Mark zero-padded (length of 5) radius in the column
#' name.
#' @keywords Calculation
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
      stringi::stri_extract_first_regex(
        chr_file1,
        pattern = "2[0-9]{3,3}\\-[0-1][0-9]\\-[0-3][0-9]"
      )
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


################################################################################
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
#' @keywords Calculation
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


################################################################################
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
#' @keywords Calculation
#' @importFrom collapse set_collapse qDT
#' @importFrom terra nearby distance buffer
#' @importFrom rlang sym
#' @importFrom Rdpack reprompt
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
          ~ sum(w_sedc * ., na.rm = TRUE)
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
################################################################################
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
    args_check <- beethoven::loadargs(path, dataset = dataset_name)

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


################################################################################
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
    assigned <- beethoven::post_calc_year_expand(
      period[1],
      period[2],
      time_available = available
    )
    return(assigned)
  }


################################################################################
# calculate over a list
#' Spatiotemporal covariate calculation
#' @keywords Calculation
#' @param domain vector of integer/character/Date.
#' Depending on temporal resolution of raw datasets.
#' Nullable; If `NULL`, it will be set to `c(1)`.
#' @param domain_name character(1). Name of the domain. Default is `"year"`.
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
          if (
            !is.null(args_process$covariate) &&
              any(names(args_process) %in% c("covariate"))
          ) {
            if (args_process$covariate == "nei") {
              args_process$county <- beethoven::process_counties()
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
                    beethoven::add_time_col(
                      res,
                      domain_each,
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
        domainlist,
        years_data,
        SIMPLIFY = FALSE
      )

    if (inherits(res_calc, "try-error")) {
      cat(paste0(attr(res_calc, "condition")$message, "\n"))
      stop("Results do not match expectations.")
    }
    res_calc <- lapply(res_calc, function(x) {
      if ("time" %in% names(x)) {
        if (nchar(x$time[1]) != 4) {
          x$time <- data.table::as.IDate(x$time)
        }
      }
      xconvt <- data.table::as.data.table(x)
      return(xconvt)
    })
    # res_calcdf <- if (length(res_calc) == 1) {
    #   data.table::as.data.table(res_calc[[1]])
    # } else if (domain_name %in% c("year", "date")) {
    #   data.table::rbindlist(res_calc, use.names = TRUE, fill = TRUE)
    # } else {
    #   reduce_merge(res_calc, by = c("site_id", "time"))
    # }
    return(res_calc)
  }


################################################################################
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

################################################################################
#' Injects arguments to parallelize MODIS/VIIRS data processing
#' @keywords Calculation
#' @note Soon to be deprecated per dropping future dependency.
#' @param locs A data frame containing the locations for which MODIS
#'   features need to be calculated.
#' @param injection **List** of dditional parameters to be passed to the
#'   `calculate_modis` function.
#' @return MODIS/VIIRS feature data.frame.
#' @seealso [`amadeus::calculate_modis_daily`], [`amadeus::calculate_modis`]
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
#'      name_covariates = "MOD_CLCVD_0_",
#'      preprocess = amadeus::process_modis_swath, radius = c(1000)))
#' }
#' @export
inject_modis_par <- function(locs, injection) {
  rlang::inject(
    amadeus::calculate_modis(
      locs = locs,
      locs_id = "site_id",
      !!!injection
    )
  )
}

################################################################################
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
    amadeus::calculate_modis(
      locs = locs,
      locs_id = "site_id",
      !!!injection
    )
  )
}


################################################################################
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
    beethoven::calc_geos_strict(
      locs = locs,
      locs_id = "site_id",
      win = c(-126, -62, 22, 52),
      snap = "out",
      !!!injection
    )
  )
}


################################################################################
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
#' @return A data frame containing the merged results of GMTED data
#'   for each location within different radii.
#' @importFrom rlang inject
#' @export
inject_gmted <- function(locs, variable, radii, injection) {
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
      }
    )

  radii_rep <- lapply(radii_rep, function(x) as.data.frame(x))
  radii_join <- beethoven::reduce_merge(radii_rep, "site_id")

  return(radii_join)
}


################################################################################
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
    all.x = TRUE,
    all.y = FALSE
  ) {
    list_check <- sapply(list_in, nrow)
    list_checkdiff <- diff(list_check)
    if (any(list_checkdiff > 0)) {
      all.y <- TRUE
    }
    for (i in seq_along(list_in)) {
      list_in[[i]] <- data.table::as.data.table(list_in[[i]])
    }

    Reduce(
      function(x, y) {
        if (is.null(by)) {
          by <- intersect(names(x), names(y))
        }
        data.table::merge.data.table(
          x,
          y,
          by = by,
          all.x = all.x,
          all.y = all.y
        )
      },
      list_in
    )
  }

################################################################################
#' Reduce and merge a list of data tables
#' @description
#' This function iteratively runs `reduce_merge` on a list of `data.table`
#' objects for merging large `data.tables` that may not fit into memory
#' with `reduce_merge` directly.
#' @param list_in A list of data tables to be merged.
#' @param by The columns to merge the data tables on.
#'   If `NULL`, the function will automatically detect the common column names.
#' @param all.x logical(1). Keeping all rows from the first input.
#' @param all.y logical(1). Keeping all rows from the second input.
#' @return A merged data table.
#' @keywords Post-calculation
#' @importFrom data.table as.data.table merge.data.table
#' @export
reduce_merge_iter <- function(
  list_in,
  by = c("site_id", "time"),
  all.x = TRUE,
  all.y = FALSE
) {
  list_check <- sapply(list_in, nrow)
  list_checkdiff <- diff(list_check)
  if (any(list_checkdiff > 0)) {
    all.y <- TRUE
  }
  for (i in seq_along(list_in)) {
    list_in[[i]] <- data.table::setDT(list_in[[i]])
  }

  dt_merge <- list_in[[1]]

  for (l in seq(2, length(list_in))) {
    if (is.null(by)) {
      merge_by <- intersect(names(dt_merge), names(list_in[[l]]))
    } else {
      merge_by <- by
    }
    dt_merge <- data.table::merge.data.table(
      dt_merge,
      list_in[[l]],
      by = merge_by,
      all.x = all.x,
      all.y = all.y
    )
  }

  return(dt_merge)
}


################################################################################
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


################################################################################
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
  }
################################################################################
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
#' @description This function loads the list object of arguments
#'   to be injected into the calculation functions defined at
#'   each target. The arguments are numeric or character, and some
#'   of these are function names. In this case, the internal function
#'  `unmarshal_function` is called to convert the function name
#'  to the actual function.
#' @keywords Utility
#' @param argfile character(1). Path to the argument file. RDS format.
#' @param dataset character(1). Dataset name.
#' @return A list of arguments stored in `dataset` slot of the
#'   argument file.
#' @importFrom qs2 qs_read
#' @export
loadargs <- function(argfile, dataset) {
  if (endsWith(argfile, ".rds")) {
    arglist <- readRDS(argfile)
  } else if (endsWith(argfile, ".qs")) {
    arglist <- qs2::qs_read(argfile)
  } else {
    stop("Invalid format.")
  }
  if (length(dataset) == 1) {
    check_args <- arglist[[dataset]]
  } else {
    check_args <- arglist[dataset]
  }
  namecheck <- grep("preprocess|_function", names(check_args))
  if (length(namecheck) > 0) {
    for (i in namecheck) {
      check_args[[i]] <- unmarshal_function(check_args[[i]])
    }
  }
  return(check_args)
}


################################################################################
# nolint start
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
#' @return A list of full file names of the MODIS files found
#'   in the specified path.
#'
#' @examples
#' \dontrun{
#' # Load MODIS files from the current directory
#' modis_files <-
#'   load_modis_files(
#'     ".",
#'     date = c("2018-01-01", "2018-01-31")
#'   )
#'
#' # Load MODIS files from a specific directory with a custom pattern
#' modis_files <-
#'   load_modis_files(
#'     "/path/to/files",
#'     pattern = "MOD.*hdf$",
#'     date = c("2018-01-01", "2018-01-31")
#'   )
#' }
#' @export
# nolint end
load_modis_files <- function(path, pattern = "hdf$", date = character(2)) {
  modis_files <-
    list.files(
      path,
      pattern = pattern,
      recursive = TRUE,
      full.names = TRUE
    )
  date_exp <-
    amadeus::generate_date_sequence(date[1], date[2], sub_hyphen = FALSE)
  date_exp <- strftime(date_exp, format = "%Y%j")
  modis_files <-
    grep(
      sprintf("(%s)", paste(paste0("A", date_exp), collapse = "|")),
      modis_files,
      value = TRUE
    )
  return(modis_files)
}

################################################################################
# nocov start
## wrapped in no coverage while looking for `fun_aqs`
#' Read AQS data
#' @keywords Utility
#' @param fun_aqs function to import AQS data.
#' Default is `amadeus::process_aqs`
#' @param export Export the file to qs. Default is FALSE.
#' @param ... Passed arguments to `fun_aqs`
#' @return Depending on `fun_aqs` specification.
#' @importFrom qs2 qs_save
#' @importFrom amadeus process_aqs
#' @export
read_locs <-
  function(
    fun_aqs = amadeus::process_aqs,
    export = FALSE,
    ...
  ) {
    aqs_read <- fun_aqs(...)
    if (export) {
      qs2::qs_save(aqs_read, file = "input/sf_feat_proc_aqs_sites.qs")
    }
    return(aqs_read)
  }
################################################################################
# nocov end

#' Unmarshal functions
#' @keywords Utility
#' @param pkg_func_str Character string specifying the package and function.
#' @return Function object.
#' @note The function name string must include two colons `::`.
#' Also, the package preceding the two colons should be loaded in the
#' current environment.
#' @description this function is developed to avoid
#'   random errors in compressing and decompressing R function objects
#'   with `qs2::qs_save` and `qs2::qs_read`. If you encounter such errors,
#'   please use this function with function name strings to save and load
#'   the function objects.
#' @export
#' @examples
#' unmarshal_function("amadeus::process_aqs")
unmarshal_function <-
  function(pkg_func_str) {
    stopifnot(grepl("::", pkg_func_str))
    pkg_func_split <- strsplit(pkg_func_str, "::")[[1]]
    pkg_name <- pkg_func_split[1]
    func_name <- pkg_func_split[2]
    get(func_name, envir = asNamespace(pkg_name))
  }


################################################################################
#' Read paths from a directory with a specific file extension
#' @keywords Utility
#' @param path The directory path from which to read the paths.
#' @param extension The file extension to match. Defaults to ".hdf".
#' @param target_dates A character vector of length 2 containing
#'  the start and end dates.
#' @param julian logical(1). If `TRUE`, the dates are in Julian format.
#' @return A character vector containing the full paths of the matching files.
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
################################################################################
#' Combine base learner prediction values with testing data.
#' @description
#' This function combines outcome data (observations) with the prediction
#' values from each base learner.
#' @keywords meta_learner
#' @param pred data.table(1). Predicted values from `fit_prediction`
#' @param test data.table(1). `data.table` with with testing dataset.
#' @param target_cols characters(1). Columns to retain from the full
#' data.frame. DO NOT include `yvar` variable in the `target_cols`.
#' @param yvar character(1). Outcome variable name.
#' @return a data.frame object, including the target columns from `data` and
#' the predictions for each base learner.
#' @keywords Utility
#' @export
attach_pred <-
  function(
    pred = NULL,
    test = NULL,
    target_cols = c("site_id", "time", "lon", "lat"),
    yvar = "Arithmetic.Mean"
  ) {
    stopifnot(!is.null(pred))
    stopifnot(!is.null(test))
    stopifnot(!is.null(target_cols))
    stopifnot(!is.null(yvar))
    if ("data.table" %in% class(pred)) {
      pred <- data.frame(pred)
    }

    stopifnot(all(target_cols %in% names(pred)))
    stopifnot(yvar %in% names(test))
    test_targets <- test[, which(names(test) %in% c(target_cols, yvar))]

    data_pred <- merge(test_targets, pred, by = target_cols)
    return(data_pred)
  }

################################################################################
#' Fit meta learner
#'
#' This function subsets the full data by column subsamples (rate=50%)
#' The optimal hyperparameter search is performed based on spatiotemporal
#' cross-validation schemes. As of version 0.4.5, users can define metric
#' used for selecting best hyperparameter set (default = "rmse").
#' @keywords meta_learner
#' @param data data.frame. Full data.frame of base learner predictions and AQS
#' spatiotemporal identifiers. [`attach_pred`].
#' @param c_subsample numeric(1). Rate of column resampling. Default is 0.5.
#' @param r_subsample numeric(1). The proportion of rows to be used. Default is
#' 1.0, which uses full dataset but setting is required to balance groups
#' generated with [`make_subdata`]
#' @param yvar character(1). Outcome variable name
#' @param target_cols characters(1). Columns from `data` to be retained during
#' column resampling. Default is c("site_id", "time", "Event.Type", "lon",
#' "lat").
#' @param args_generate_cv List of arguments to be passed to
#' `switch_generate_cv_rset` function.
#' @param tune_iter integer(1). Bayesian optimization iterations.
#' Default is 50.
#' @param trim_resamples logical(1). Default is TRUE, which replaces the actual
#' data.frames in splits column of `tune_results` object with NA.
#' Passed to [`fit_base_tune`].
#' @param return_best logical(1). If TRUE, the best tuned model is returned.
#' Passed to [`fit_base_tune`].
#' @param metric character(1). The metric to be used for selecting the best.
#' Must be one of "rmse", "rsq", "mae". Default = "rmse".
#' Passed to [`fit_base_tune`].
#' @seealso [`fit_base_tune`] [`make_subdata`]
#' @importFrom parsnip linear_reg
#' @importFrom workflows workflow add_variables add_model
#' @importFrom yardstick metric_set rmse mae
#' @importFrom tune tune tune_bayes select_best
#' @return List of 3, including the best-fit model, the best hyperparameters,
#' and the all performance records from `tune::tune_bayes()`.
#' Note that the meta learner function returns the best-fit model,
#' not predicted values.
#' @export
fit_meta_learner <-
  function(
    data,
    c_subsample = 0.5,
    r_subsample = 1.0,
    yvar = "Arithmetic.Mean",
    target_cols = c("site_id", "time", "lon", "lat", "Event.Type"),
    args_generate_cv = list(),
    tune_iter = 50L,
    trim_resamples = FALSE,
    return_best = TRUE,
    metric = "rmse"
  ) {
    stopifnot(!is.null(data))
    stopifnot(!is.null(yvar))
    stopifnot(!is.null(target_cols))

    # define model
    meta_model <- beethoven::switch_model(
      model_type = "elnet",
      device = "cpu"
    )

    # apply r_subsample % row subsampling
    chr_rowidx <- beethoven::make_subdata(
      data,
      p = r_subsample,
      ngroup_init = NULL
    )

    # subset data to c_subsample proportion of columns
    chr_id_names <- unique(c(target_cols, yvar))
    chr_meta_names <- setdiff(names(data), chr_id_names)
    chr_sample_cidx <- sample(
      chr_meta_names,
      floor(c_subsample * length(chr_meta_names))
    )
    chr_colidx <- c(chr_id_names, chr_sample_cidx)

    # sample of data with r_subsample rows, c_subsample columns
    dt_sample <- data.table::data.table(data)[
      chr_rowidx,
      chr_colidx,
      with = FALSE
    ]

    # define spatiotemporal folds
    args_generate_cv <-
      c(
        list(data = dt_sample, cv_mode = "spatiotemporal"),
        args_generate_cv
      )

    # generate row index
    cv_index <- beethoven::inject_match(
      beethoven::generate_cv_index_spt,
      args_generate_cv
    )

    # using cv_index, restore rset
    meta_vfold <-
      beethoven::convert_cv_index_rset(
        cv_index,
        dt_sample,
        cv_mode = "spatiotemporal"
      )

    # define recipe
    meta_recipe <-
      recipes::recipe(
        dt_sample[1, ]
      ) |>
      recipes::update_role(
        !!seq(length(chr_id_names), ncol(dt_sample))
      ) |>
      recipes::update_role(!!yvar, new_role = "outcome")

    # fit glmnet meta learner with `fit_base_tune`
    meta_wflist <-
      beethoven::fit_base_tune(
        data_full = data.table::data.table(dt_sample),
        recipe = meta_recipe,
        model = meta_model,
        resample = meta_vfold,
        tune_mode = "bayes",
        grid = NULL,
        iter_bayes = tune_iter,
        trim_resamples = trim_resamples,
        return_best = return_best,
        metric = metric
      )

    return(meta_wflist)
  }


################################################################################
#' Predict meta learner
#' @keywords meta_learner
#' @param meta_fitted Fitted meta learner model.
#' @param new_data data.frame. New data. Must have the same
#'   predictands and predictors as the training data.
#' @importFrom stats predict
#' @return Predicted values.
#' @export
predict_meta_learner <-
  function(
    meta_fitted,
    new_data
  ) {
    stats::predict(meta_fitted, new_data)
  }
################################################################################
#' Combine dynamically branched sublists based on common column names
#' @description
#' This function combines the sublists of dynamically branched targets object
#' as a preprocessor for `reduce_merge` function.
#' @keywords prediction
#' @param df_list list(1). List of data frames with common column names.
#' Typically the output of a dynamically branched target.
#' @return a list object, with data frames combined by common column names.
#' @keywords Utility
#' @export
reduce_list <- function(df_list) {
  # Create a named list to group data frames by their column names
  grouped_dfs <- list()

  # Iterate over each data frame in the list
  for (df in df_list) {
    # Get the column names of the current data frame
    col_names <- paste(sort(colnames(df)), collapse = ",")

    # Check if this set of column names already exists in the grouped list
    if (!col_names %in% names(grouped_dfs)) {
      grouped_dfs[[col_names]] <- list()
    }

    # Append the current data frame to the list for its column names
    grouped_dfs[[col_names]] <- append(grouped_dfs[[col_names]], list(df))
  }

  # Combine data frames with the same column names
  combined_list <- lapply(grouped_dfs, function(dfs) {
    do.call(rbind, dfs)
  })

  # Assign numeric names to the combined list
  names(combined_list) <- seq_along(combined_list)

  return(combined_list)
}
################################################################################
# please note that functions here are modified version of
# the original functions in the package amadeus (<0.2.0)

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
  function(path = NULL, date = c("2018-01-01", "2018-01-01"), ...) {
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
              rep(v, terra::nlyr(rast_summary)),
              "_",
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
        .f = ~ summary_byvar(fs = .x),
        .options = furrr::furrr_options(
          globals = c("other_args", "data_variables")
        )
      )
    rast_10d_summary <- Reduce(c, rast_10d_summary)
    return(rast_10d_summary)
  }


################################################################################
#' Process NARR Data (v2)
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
#' @return A SpatRaster object containing the processed NARR2 data.
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
#' @importFrom amadeus download_sanitize_path check_for_null_parameters
#' @importFrom amadeus generate_date_sequence
#' @importFrom terra rast time subset
#' @export
process_narr2 <- function(
  date = c("2023-09-01", "2023-09-01"),
  variable = NULL,
  path = NULL,
  ...
) {
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
    substr(date_sequence, 1, ifelse(all(nchar(ym_from) == 6), 6, 4))
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

  # mm-tests-0816 new variable abbreviation method
  # previous method resulted in "MET_NA..." names for
  # all variables
  variable_abbr <- sprintf(
    "MET_%s",
    toupper(
      gsub(
        "_",
        "",
        gsub(
          "\\.",
          "",
          variable
        )
      )
    )
  )

  #### initiate for loop
  data_full <- terra::rast()
  for (p in seq_along(data_paths_ym)) {
    #### import data
    data_year <- terra::rast(data_paths_ym[p])
    data_year_tinfo <- terra::time(data_year)
    time_processed <- as.POSIXlt(data_year_tinfo)
    time_this <- time_processed[1]
    cat(paste0(
      "Cleaning ",
      variable,
      " data for ",
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
        variable_abbr,
        "_",
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
        variable_abbr,
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
      ) %in%
        date_sequence
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


################################################################################
#' Export preprocessed GeoTIFF file of one day MODIS product files
#' @param path_in character. File paths of MODIS product files on a day
#' @param product_code character(1). Product code. e.g., "VNP46A2".
#' @param pat character. Filter of `path_in`.
#' @param subdataset character. Subdatasets to merge. It is ignored
#'   when `product_code` is one of `"MOD06_L2"` or `"VNP46A2"`.
#' @param dest character(1). Directory path to export the result file.
#' @return A GeoTIFF file is saved to
#'   `{dest}/{product_code}_processed_{date}.tif` and this path is returned.
#' @importFrom amadeus process_blackmarble process_blackmarble_corners
#' @importFrom amadeus process_modis_swath process_modis_merge
#' @importFrom stringi stri_extract_first_regex stri_sub
#' @importFrom terra writeRaster
#' @author Insang Song
#' @keywords Calculation
#' @export
#' @examples
#' \dontrun{
#' export_tif(c("A2018001.001.hdf", "A2018001.002.hdf"),
#'            product_code = "MOD11A1",
#'            pat = "A\\d{7,7}",
#'            subdataset = "16 day NDVI",
#'            dest = ".")
#' # mirai run
#' library(mirai)
#' basedir61 <- "/mnt/modis/raw/61"
#' product <- "MCD19A2"
#' paths <-
#'   list.files(
#'     file.path(basedir61, product),
#'     pattern = "*.*.hdf$",
#'     full.names = TRUE,
#'     recursive = TRUE
#'   )
#' pcode <- "MCD19A2"
#' pattern <- "A\\d{7,7}"
#' subdataset <- c("Optical_Depth_055")
#' dest = "mydir"
#'
#' run_mirai <-
#'   mirai_map(
#'     paths,
#'     .f = export_tif,
#'     .args = list(
#'        product_code = product,
#'        pat = pattern,
#'        subdataset = subdataset,
#'        dest = dest
#'     )
#'   )
#' run_mirai[.progress]
#' }
export_tif <- function(
  path_in,
  product_code,
  pat,
  subdataset = "",
  dest = "."
) {
  redate <- stringi::stri_extract_first_regex(path_in, pat)
  redate <- unique(redate)
  redate <- stringi::stri_sub(redate, 2, 8)
  redate <- as.Date(redate, format = "%Y%j")
  # path to export the result file(s)
  name_head <-
    paste0(product_code, "_processed_", redate, ".tif")
  name_head <- file.path(dest, name_head)

  if (!file.exists(name_head)) {
    flattened <-
      if (product_code == "VNP46A2") {
        amadeus::process_blackmarble(
          path = path_in,
          date = as.character(redate),
          subdataset = 3L,
          tile_df = amadeus::process_blackmarble_corners()
        )
      } else if (product_code == "MOD06_L2") {
        amadeus::process_modis_swath(
          path = path_in,
          date = as.character(redate),
          subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
          suffix = ":mod06:",
          resolution = 0.05
        )
      } else {
        amadeus::process_modis_merge(
          path = path_in,
          date = as.character(redate),
          subdataset = subdataset,
          fun_agg = "mean"
        )
      }

    terra::writeRaster(
      flattened,
      name_head,
      gdal = "COMPRESS=DEFLATE",
      overwrite = TRUE
    )
    message(paste0("Exported ", name_head))
  } else {
    message("The target file exists. Proceed to the next target.")
  }
  return(name_head)
}
################################################################################
#' Load county sf object
#' @keywords Calculation
#' @param year integer(1). Year of the county shapefile.
#' @param exclude character. State FIPS codes to exclude.
#' Default is c("02", "15", "60", "66", "68", "69", "72", "78").
#' @return sf object
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
      cnty[
        !cnty$STATEFP %in%
          c("02", "15", "60", "66", "68", "69", "72", "78"),
      ]
    return(cnty)
  }


################################################################################
#' Get Divisors
#' @keywords Miscellaneous
#' @param x integer(1). A positive integer.
#' @return A vector of divisors of x.
#' @export
divisor <-
  function(x) {
    stopifnot(!is.character(x))
    xv <- seq_len(as.integer(x))
    xv[which(x %% xv == 0)]
  }
################################################################################
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
  function(path = NULL, date = c("2018-01-01", "2018-01-01"), ...) {
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
              rep(v, terra::nlyr(rast_summary)),
              "_",
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
        .f = ~ summary_byvar(fs = .x),
        .options = furrr::furrr_options(
          globals = c("other_args", "data_variables")
        )
      )
    rast_10d_summary <- Reduce(c, rast_10d_summary)
    return(rast_10d_summary)
  }


################################################################################
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

################################################################################
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


################################################################################
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
    cv_mode = c("spatiotemporal", "spatial", "temporal"),
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
      ) |>
      # do we want to normalize the predictors?
      # if so, an additional definition of truly continuous variables is needed
      # recipes::step_normalize(recipes::all_numeric_predictors()) |>
      recipes::update_role(!!xvar) |>
      recipes::update_role(!!yvar, new_role = "outcome") #|>
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
          cv_index,
          dt_sample,
          cv_mode = cv_mode
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
      ) |>
      parsnip::set_engine("brulee", device = device) |>
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

################################################################################
# dt <- qs2::qs_read("output/dt_feat_design_imputed_061024.qs")
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
    #   dt_imputed |>
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
        index_cv,
        data_orig,
        ref_list = ref_list,
        cv_mode = cv_mode
      )

    base_recipe <-
      recipes::recipe(
        dt_imputed[1, ]
      ) |>
      # recipes::step_normalize(recipes::all_numeric_predictors()) |>
      recipes::update_role(tidyselect::all_of(xvar)) |>
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
      ) |>
      parsnip::set_engine("xgboost", device = device) |>
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

################################################################################
# dt <- qs2::qs_read("output/dt_feat_design_imputed_061024.qs")
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
        index_cv,
        data_orig,
        ref_list = ref_list,
        cv_mode = cv_mode
      )

    base_recipe <-
      recipes::recipe(
        dt_imputed[1, ]
      ) |>
      # recipes::step_normalize(recipes::all_numeric_predictors()) |>
      recipes::update_role(tidyselect::all_of(xvar)) |>
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
      ) |>
      parsnip::set_engine("lightgbm", device_type = device) |>
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


################################################################################
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
        penalty = 10^seq(-3, 5)
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
        index_cv,
        data_orig,
        ref_list = ref_list,
        cv_mode = cv_mode
      )

    base_recipe <-
      recipes::recipe(
        dt_imputed[1, ]
      ) |>
      # recipes::step_normalize(recipes::all_numeric_predictors()) |>
      recipes::update_role(tidyselect::all_of(xvar)) |>
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
      ) |>
      parsnip::set_engine("glmnet") |>
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
################################################################################
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


################################################################################
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
        as.list(seq_len(nest_length)) |>
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
                combined_lr[[1]],
                combined_lr[[2]],
                combined_lr[[3]],
                combined_lr[[4]],
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
        update(parsnip_spec, parameters = !!!as.list(tuned_best))
      )

    # fit the entire data
    model_fit <- parsnip::fit(model_best, data = df_full)
    pred <- predict(model_fit, data = df_full)
    return(pred)
  }

# nocov end
################################################################################
# nolint start
#' Generate argument list for raw data download
#'
#' @keywords Utility
#' @param char_period Character(2) vector specifying the time period.
#'  Default is c("2018-01-01", "2022-10-31").
#' @param char_input_dir Character string specifying the input path.
#' Default is "input".
#' @param nasa_earth_data_token Character string specifying the NASA Earth Data token.
#' @param mod06_filelist character(1). File path to a CSV file with MOD06 download
#'   URLs.
#' @param year_nlcd numeric(2). Numeric vector specifying the NLCD years.
#' Default is c(2019, 2021).
#' @param export logical(1). If TRUE, the list is saved to `path_export`.
#' Default is `TRUE`.
#' @param path_export Character string specifying the export path.
#' Default is "inst/targets/download_spec.qs".
#' @export
set_args_download <-
  function(
    char_period = c("2018-01-01", "2022-10-31"),
    char_input_dir = "input",
    nasa_earth_data_token = NULL,
    mod06_filelist = NULL,
    year_nlcd = c(2019, 2021),
    export = FALSE,
    path_export = "inst/targets/download_spec.qs"
  ) {
    # NULL NASA Earth Data token will warn users
    if (is.null(nasa_earth_data_token)) {
      warning(
        paste0(
          "Argument nasa_earth_data_token is NULL. ",
          "Please provide a NASA Earth Data token to make the downloading ",
          "process work properly."
        )
      )
    }

    # append input path
    ain <- function(x) {
      file.path(char_input_dir, x)
    }

    char_date_temp <- c("%s-01-01", "%s-12-31")
    time_periods <- as.numeric(substr(char_period, 1, 4))
    time_sequence <- seq(time_periods[1], time_periods[2])
    year_nei <- seq(2017, time_periods[2], 3)
    gmted_vars <-
      c(
        "Breakline Emphasis",
        "Systematic Subsample",
        "Median Statistic",
        "Minimum Statistic",
        "Mean Statistic",
        "Maximum Statistic",
        "Standard Deviation Statistic"
      )
    narr_variables_mono <-
      c(
        "air.sfc",
        "albedo",
        "apcp",
        "dswrf",
        "evap",
        "hcdc",
        "hpbl",
        "lcdc",
        "lhtfl",
        "mcdc",
        "pr_wtr",
        "prate",
        "pres.sfc",
        "shtfl",
        "snowc",
        "soilm",
        "tcdc",
        "ulwrf.sfc",
        "uwnd.10m",
        "vis",
        "vwnd.10m",
        "weasd"
      )
    narr_variables_plevels <-
      c("omega", "shum")

    list_download_config <-
      list(
        aqs = list(
          dataset_name = "aqs",
          directory_to_save = ain("aqs"),
          year = time_periods,
          unzip = TRUE,
          remove_zip = TRUE
        ),
        mod11 = lapply(
          time_sequence,
          function(t) {
            list(
              dataset_name = "modis",
              directory_to_save = ain("modis/raw"),
              product = "MOD11A1",
              date = sprintf(char_date_temp, as.character(t)),
              nasa_earth_data_token = nasa_earth_data_token
            )
          }
        ),
        mod06 = lapply(
          time_sequence,
          function(t) {
            list(
              dataset_name = "modis",
              directory_to_save = ain("modis/raw"),
              product = "MOD06_L2",
              date = sprintf(char_date_temp, as.character(t)),
              nasa_earth_data_token = nasa_earth_data_token,
              mod06_links = mod06_filelist
            )
          }
        ),
        mod09 = lapply(
          time_sequence,
          function(t) {
            list(
              dataset_name = "modis",
              directory_to_save = ain("modis/raw"),
              product = "MOD09GA",
              date = sprintf(char_date_temp, as.character(t)),
              nasa_earth_data_token = nasa_earth_data_token
            )
          }
        ),
        mcd19 = lapply(
          time_sequence,
          function(t) {
            list(
              dataset_name = "modis",
              directory_to_save = ain("modis/raw"),
              product = "MCD19A2",
              date = sprintf(char_date_temp, as.character(t)),
              nasa_earth_data_token = nasa_earth_data_token
            )
          }
        ),
        mod13 = lapply(
          time_sequence,
          function(t) {
            list(
              dataset_name = "modis",
              directory_to_save = ain("modis/raw"),
              product = "MOD13A2",
              date = sprintf(char_date_temp, as.character(t)),
              nasa_earth_data_token = nasa_earth_data_token
            )
          }
        ),
        viirs = lapply(
          time_sequence,
          function(t) {
            list(
              dataset_name = "modis",
              directory_to_save = ain("modis/raw"),
              product = "VNP46A2",
              date = sprintf(char_date_temp, as.character(t)),
              version = "5000",
              nasa_earth_data_token = nasa_earth_data_token
            )
          }
        ),
        geoscf_aqc = list(
          dataset_name = "geos",
          directory_to_save = ain("geos"),
          collection = "aqc_tavg_1hr_g1440x721_v1",
          date = char_period
        ),
        geoscf_chm = list(
          dataset_name = "geos",
          directory_to_save = ain("geos"),
          collection = "chm_tavg_1hr_g1440x721_v1",
          date = char_period
        ),
        hms = list(
          dataset_name = "smoke",
          directory_to_save = ain("HMS_Smoke"),
          data_format = "Shapefile",
          date = char_period,
          unzip = TRUE,
          remove_zip = TRUE
        ),
        gmted = lapply(
          gmted_vars,
          function(v) {
            list(
              dataset_name = "gmted",
              directory_to_save = ain("gmted"),
              statistic = v,
              resolution = "7.5 arc-seconds",
              unzip = TRUE,
              remove_zip = TRUE
            )
          }
        ),
        nei = lapply(
          year_nei,
          function(y) {
            list(
              dataset_name = "nei",
              directory_to_save = ain("nei"),
              year_target = y,
              unzip = TRUE
            )
          }
        ),
        tri = list(
          dataset_name = "tri",
          directory_to_save = ain("tri"),
          year = time_periods
        ),
        nlcd = lapply(
          year_nlcd,
          function(y) {
            list(
              dataset_name = "nlcd",
              directory_to_save = ain("nlcd"),
              year = y,
              unzip = TRUE,
              remove_zip = TRUE
            )
          }
        ),
        koppen = list(
          dataset_name = "koppen",
          directory_to_save = ain("koppen_geiger"),
          data_resolution = "0.0083",
          time_period = "Present",
          unzip = TRUE,
          remove_zip = TRUE
        ),
        ecoregions = list(
          dataset_name = "koppen",
          directory_to_save = ain("ecoregions"),
          unzip = TRUE,
          remove_zip = TRUE
        ),
        narr_monolevel = lapply(
          narr_variables_mono,
          function(v) {
            list(
              dataset_name = "narr",
              directory_to_save = ain("narr"),
              variables = v,
              year = time_periods
            )
          }
        ),
        narr_p_levels = lapply(
          narr_variables_plevels,
          function(v) {
            list(
              dataset_name = "narr",
              directory_to_save = ain("narr"),
              variables = v,
              year = time_periods
            )
          }
        ),
        groads = list(
          dataset_name = "sedac_groads",
          directory_to_save = ain("sedac_groads"),
          data_region = "Americas",
          data_format = "Geodatabase",
          unzip = TRUE,
          remove_zip = TRUE
        ),
        population = list(
          dataset_name = "sedac_population",
          directory_to_save = ain("sedac_population"),
          data_resolution = "30 second",
          data_format = "GeoTIFF",
          year = "2020",
          unzip = TRUE,
          remove_zip = TRUE
        )
      )

    if (export) {
      if (endsWith(path_export, "qs")) {
        qs2::qs_save(list_download_config, path_export)
        message("Download configuration is saved to ", path_export)
      } else if (endsWith(path_export, "rds")) {
        saveRDS(list_download_config, path_export)
        message("Download configuration is saved to ", path_export)
      } else {
        stop("Please provide a valid file extension: qs or rds.")
      }
    }
    return(list_download_config)
  }

################################################################################
#' Set arguments for the calculation process
#'
#' This function sets the arguments for the calculation process.
#' It takes several parameters including site ID, time ID, time period,
#' extent, user email, export path, and input path.
#' @keywords Utility
#' @param char_siteid Character string specifying the site ID.
#'   Default is "site_id".
#' @param char_timeid Character string specifying the time ID.
#'   Default is "time".
#' @param char_period Character vector specifying the time period.
#'   Default is c("2018-01-01", "2022-10-31").
#' @param num_extent Numeric vector specifying the extent.
#'   Default is c(-126, -62, 22, 52).
#' @param char_user_email Character string specifying the user email.
#'   Default is the current user's email with nih.gov domain.
#' @param export logical(1). If TRUE, the list for the calculation process
#'   is exported to `path_export`. Default is FALSE.
#' @param path_export Character string specifying the export path.
#'   Default is "inst/targets/calc_spec.qs".
#' @param char_input_dir Character string specifying the input path.
#'    Default is "input".
#' @param nthreads_nasa integer(1). Number of threads for NASA data.
#'    Default is 14L.
#' @param nthreads_tri integer(1). Number of threads for TRI data.
#'   Default is 5L.
#' @param nthreads_geoscf integer(1). Number of threads for GEOSCF data.
#'   Default is 10L.
#' @param nthreads_gmted integer(1). Number of threads for GMTED data.
#'   Default is 4L.
#' @param nthreads_narr integer(1). Number of threads for NARR data.
#'   Default is 24L.
#' @param nthreads_groads integer(1). Number of threads for GROADS data.
#'   Default is 3L.
#' @param nthreads_population integer(1). Number of threads for population data.
#'   Default is 3L.
#' @param nthreads_append integer(1). Number of threads for appending data.
#'   Default is 8L.
#' @param nthreads_impute integer(1). Number of threads for imputing data.
#'   Default is 64L.
#'
#' @note
#' The number of threads used is fixed as 1L
#' otherwise specified in `nthreads_*` arguments.
#' path_input should contain the following subdirectories:
#' - modis/raw/61/MOD11A1
#' - modis/raw/61/MOD06_L2
#' - modis/raw/61/MOD09GA
#' - modis/raw/61/MCD19A2
#' - modis/raw/61/MOD13A2
#' - modis/raw/5000/VNP46A2
#' - aqs
#' - nlcd
#' - geos/aqc_tavg_1hr_g1440x721_v1
#' - geos/chm_tavg_1hr_g1440x721_v1
#' - HMS_Smoke/data
#' - gmted
#' - nei
#' - narr
#' - HMS_Smoke
#' - koppen_geiger
#' - ecoregions
#' - sedac_groads
#' - sedac_population
#'
#' @return A list of arguments for common use
#'   in the calculation process. A *.qs or *.rds file defined in
#'  `path_export` is saved if `export` is TRUE.
#'
#' * char_siteid: Character string specifying the site ID.
#' * char_timeid: Character string specifying the time ID.
#' * char_period: Character vector specifying the time period.
#' * num_extent: Numeric vector specifying the extent.
#' * char_user_email: Character string specifying the user email.
#' * char_input_dir: Character string specifying the input path.
#' * nthreads_nasa: Number of threads for NASA data.
#' * nthreads_tri: Number of threads for TRI data.
#' * nthreads_geoscf: Number of threads for GEOS-CF data.
#' * nthreads_gmted: Number of threads for GMTED data.
#' * nthreads_narr: Number of threads for NARR data.
#' * nthreads_groads: Number of threads for SEDAC Groads data.
#' * nthreads_population: Number of threads for population data.
#' * nthreads_append: Number of threads for appending data.
#' * nthreads_impute: Number of threads for imputing data.
#' @author Insang Song
#' @importFrom qs2 qs_save
#' @importFrom tibble tribble
#' @export
set_args_calc <-
  function(
    char_siteid = "site_id",
    char_timeid = "time",
    char_period = c("2018-01-01", "2022-10-31"),
    num_extent = c(-126, -62, 22, 52),
    char_user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
    export = FALSE,
    path_export = "inst/targets/calc_spec.qs",
    char_input_dir = "input",
    nthreads_nasa = 14L,
    nthreads_tri = 5L,
    nthreads_geoscf = 10L,
    nthreads_gmted = 4L,
    nthreads_narr = 24L,
    nthreads_groads = 3L,
    nthreads_population = 3L,
    nthreads_append = 8L,
    nthreads_impute = 64L
  ) {
    list_common <-
      list(
        char_siteid = char_siteid,
        char_timeid = char_timeid,
        char_period = char_period,
        extent = num_extent,
        char_user_email = char_user_email,
        char_input_dir = char_input_dir,
        nthreads_nasa = nthreads_nasa,
        nthreads_tri = nthreads_tri,
        nthreads_geoscf = nthreads_geoscf,
        nthreads_gmted = nthreads_gmted,
        nthreads_narr = nthreads_narr,
        nthreads_groads = nthreads_groads,
        nthreads_population = nthreads_population,
        nthreads_append = nthreads_append,
        nthreads_impute = nthreads_impute
      )
    ain <- function(x, append = FALSE) {
      if (append) {
        file.path(char_input_dir, x, "data_files")
      } else {
        file.path(char_input_dir, x)
      }
    }
    if (export) {
      list_paths <-
        list(
          mod11 = load_modis_files(
            ain("modis/raw/61/MOD11A1"),
            date = list_common$char_period
          ),
          mod06 = load_modis_files(
            ain("modis/raw/61/MOD06_L2"),
            date = list_common$char_period
          ),
          mod09 = load_modis_files(
            ain("modis/raw/61/MOD09GA"),
            date = list_common$char_period
          ),
          mcd19 = load_modis_files(
            ain("modis/raw/61/MCD19A2"),
            date = list_common$char_period
          ),
          mod13 = load_modis_files(
            ain("modis/raw/61/MOD13A2"),
            date = list_common$char_period
          ),
          viirs = load_modis_files(
            ain("modis/raw/5000/VNP46A2"),
            "h5$",
            date = list_common$char_period
          )
        )

      list_calcspec <-
        list(
          aqs = list(
            path = ain("aqs", TRUE),
            date = list_common$char_period
          ),
          mod11 = list(
            from = list_paths$mod11,
            name_covariates = sprintf("MOD_SFCT%s_0_", c("D", "N")),
            subdataset = "^LST_",
            nthreads = nthreads_nasa,
            radius = c(1e3, 1e4, 5e4)
          ),
          mod06 = list(
            from = list_paths$mod06,
            name_covariates = sprintf("MOD_CLCV%s_0_", c("D", "N")),
            subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
            nthreads = nthreads_nasa,
            preprocess = "amadeus::process_modis_swath",
            radius = c(1e3, 1e4, 5e4)
          ),
          mod09 = list(
            from = list_paths$mod09,
            name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
            subdataset = "^sur_refl_",
            nthreads = nthreads_nasa,
            radius = c(1e3, 1e4, 5e4)
          ),
          mcd19_1km = list(
            from = list_paths$mcd19,
            name_covariates = sprintf("MOD_AD%dTA_0_", c(4, 5)),
            subdataset = "^Optical_Depth",
            nthreads = nthreads_nasa,
            radius = c(1e3, 1e4, 5e4)
          ),
          mcd19_5km = list(
            from = list_paths$mcd19,
            name_covariates = sprintf(
              "MOD_%sAN_0_",
              c("CSZ", "CVZ", "RAZ", "SCT", "GLN")
            ),
            subdataset = "cos|RelAZ|Angle",
            nthreads = nthreads_nasa,
            radius = c(1e3, 1e4, 5e4)
          ),
          mod13 = list(
            from = list_paths$mod13,
            name_covariates = "MOD_NDVIV_0_",
            subdataset = "(NDVI)",
            nthreads = nthreads_nasa,
            radius = c(1e3, 1e4, 5e4)
          ),
          viirs = list(
            from = list_paths$viirs,
            name_covariates = "MOD_LGHTN_0_",
            subdataset = 3,
            nthreads = nthreads_nasa,
            preprocess = "amadeus::process_blackmarble",
            radius = c(1e3, 1e4, 5e4)
          ),
          geoscf_aqc = list(
            date = list_common$char_period,
            path = ain("geos/aqc_tavg_1hr_g1440x721_v1"),
            nthreads = nthreads_geoscf
          ),
          geoscf_chm = list(
            date = list_common$char_period,
            path = ain("geos/chm_tavg_1hr_g1440x721_v1"),
            nthreads = nthreads_geoscf
          ),
          # base class covariates start here
          hms = list(
            path = ain("HMS_Smoke", TRUE),
            date = list_common$char_period,
            covariate = "hms"
          ),
          gmted = list(
            path = ain("gmted", TRUE),
            covariate = "gmted"
          ),
          nei = list(
            domain = c(2017, 2020),
            domain_name = "year",
            path = ain("nei", TRUE),
            covariate = "nei"
          ),
          tri = list(
            domain = seq(2018, 2022),
            domain_name = "year",
            path = ain("tri"),
            radius = c(1e3, 1e4, 5e4),
            covariate = "tri",
            nthreads = nthreads_tri
          ),
          nlcd = list(
            domain = c(2019, 2021),
            domain_name = "year",
            path = ain("nlcd", TRUE),
            covariate = "nlcd",
            mode = "exact",
            extent = NULL,
            radius = c(1e3, 1e4, 5e4),
            max_cells = 1e8,
            nthreads = 6L
          ),
          koppen = list(
            path = ain(
              "koppen_geiger/data_files/Beck_KG_V1_present_0p0083.tif"
            ),
            covariate = "koppen",
            nthreads = 1L
          ),
          ecoregions = list(
            path = ain("ecoregions/data_files/us_eco_l3_state_boundaries.shp"),
            covariate = "ecoregions",
            nthreads = 1L
          ),
          narr = list(
            path = ain("narr"),
            covariate = "narr",
            domain_reduced = c(
              "air.sfc",
              "albedo",
              "apcp",
              "dswrf",
              "evap",
              "hcdc",
              "hpbl",
              "lcdc",
              "lhtfl",
              "mcdc",
              "omega",
              "pr_wtr",
              "pres.sfc",
              "shtfl",
              "snowc",
              "soilm",
              "tcdc",
              "ulwrf.sfc",
              "uwnd.10m",
              "vis",
              "vwnd.10m",
              "weasd"
            ),
            domain_appt = c("prate", "shum"),
            domain = c(
              "air.sfc",
              "albedo",
              "apcp",
              "dswrf",
              "evap",
              "hcdc",
              "hpbl",
              "lcdc",
              "lhtfl",
              "mcdc",
              "omega",
              "pr_wtr",
              "prate",
              "pres.sfc",
              "shtfl",
              "shum",
              "snowc",
              "soilm",
              "tcdc",
              "ulwrf.sfc",
              "uwnd.10m",
              "vis",
              "vwnd.10m",
              "weasd"
            ),
            domain_name = "variable",
            date = list_common$char_period,
            process_function = "beethoven::process_narr2",
            calc_function = "beethoven::calc_narr2",
            nthreads = nthreads_narr
          ),
          groads = list(
            path = ain("sedac_groads/data_files/gROADS-v1-americas.gdb"),
            covariate = "groads",
            radius = c(1e3, 1e4, 5e4),
            nthreads = nthreads_groads
          ),
          population = list(
            path = ain(
              "sedac_population/data_files/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif"
            ),
            covariate = "population",
            fun = "mean",
            radius = c(1e3, 1e4, 5e4),
            nthreads = nthreads_population
          )
        )

      attr(list_calcspec, "description") <-
        tibble::tribble(
          ~dataset                                       ,
          ~description                                   ,
          "mod11"                                        ,
          "MODIS Land Surface Temperature Day/Night"     ,
          "mod06"                                        ,
          "MODIS Cloud Fraction Day/Night"               ,
          "mod09"                                        ,
          "MODIS Surface Reflectance"                    ,
          "mcd19_1km"                                    ,
          "MCD19A2 1km"                                  ,
          "mcd19_5km"                                    ,
          "MCD19A2 5km"                                  ,
          "mod13"                                        ,
          "MODIS Normalized Difference Vegetation Index" ,
          "viirs"                                        ,
          "VIIRS Nighttime Lights"                       ,
          "hms"                                          ,
          "NOAA Hazard Mapping System Smoke"             ,
          "geoscf_aqc"                                   ,
          "GEOS-CF AQC"                                  ,
          "geoscf_chm"                                   ,
          "GEOS-CF CHM"                                  ,
          "gmted"                                        ,
          "GMTED elevation"                              ,
          "nei"                                          ,
          "National Emission Inventory"                  ,
          "tri"                                          ,
          "Toxic Release Inventory"                      ,
          "nlcd"                                         ,
          "National Land Cover Database"                 ,
          "koppen"                                       ,
          "Koppen-Geiger Climate Classification"         ,
          "ecoregions"                                   ,
          "EPA Ecoregions"                               ,
          "narr"                                         ,
          "NARR"                                         ,
          "groads"                                       ,
          "SEDAC Global Roads"                           ,
          "population"                                   ,
          "SEDAC Population Density"
        )
      if (is.null(path_export)) {
        assign("arglist_calcspec", list_calcspec, envir = .GlobalEnv)
        return(list_common)
      } else {
        if (endsWith(path_export, "qs")) {
          qs2::qs_save(list_calcspec, path_export)
        } else if (endsWith(path_export, "rds")) {
          saveRDS(list_calcspec, path_export)
        } else {
          stop("Please provide a valid file extension: qs or rds.")
        }

        return(list_common)
      }
    }
    return(list_common)
  }

################################################################################
#' Configure Library Paths and Environment Variables for Beethoven Workflow
#'
#' This function sets up the library paths and environmental variables required
#' for running the Beethoven workflow in a containerized environment.
#' @keywords Utility
#' @param libpaths A character vector specifying the library paths to use.
#'   By default, it excludes user-specific and host paths from `.libPaths()`.
#' @param path A character string specifying the system `PATH` environment
#'   variable. Defaults to a container-friendly configuration with CUDA paths.
#' @param ld_library_path A character string specifying the `LD_LIBRARY_PATH`
#'   environment variable. Defaults to `"/usr/local/cuda/lib64"`.
#' @param cuda_home A character string specifying the `CUDA_HOME` environment
#'   variable. Defaults to `"/usr/local/cuda"`.
#' @return NULL
#' @export
sys_beethoven <- function(
  libpaths = .libPaths(
    grep(
      paste0("biotools|", Sys.getenv("USER")),
      .libPaths(),
      value = TRUE,
      invert = TRUE
    )
  ),
  path = paste0(
    "/usr/local/cuda/bin:",
    "/usr/local/nvidia/bin:",
    "/usr/local/cuda/bin:",
    "/usr/local/sbin:",
    "/usr/local/bin:",
    "/usr/sbin:",
    "/usr/bin:",
    "/sbin:",
    "/bin"
  ),
  ld_library_path = "/usr/local/cuda/lib64",
  cuda_home = "/usr/local/cuda"
) {
  ##### Set
  # Exclude user-specific and host paths from available library paths.
  .libPaths(libpaths)

  # Set environmental variables relative to container paths.
  Sys.setenv(
    "PATH" = path,
    "LD_LIBRARY_PATH" = ld_library_path,
    "CUDA_HOME" = cuda_home
  )
}
# nolint end
