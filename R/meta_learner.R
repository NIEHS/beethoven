#' Combine prediction values from base learners
#' @description
#' This function combines outcome data (observations) with the prediction
#' values from each base learner.
#' @keywords meta_learner
#' @param data data.frame(1). Full data.
#' @param pred list(1). List with base learner prediction values.
#' @param position numeric(1). Position of the prediction values in the list
#' `pred`. Default is 1.
#' @param target_cols characters(1). Columns to retain from the full
#' data.frame.
#' @param yvar character(1). Outcome variable name.
#' @return a data.frame object, including the target columns from `data` and
#' the predictions for each base learner.
#' @keywords Utility
#' @export
attach_pred <-
  function(
    data,
    pred,
    position = 1,
    target_cols = c("site_id", "time", "Event.Type", "lon", "lat"),
    yvar = "Arithmetic.Mean"
  ) {
    stopifnot(!is.null(data))
    stopifnot(!is.null(target_cols))
    stopifnot(!is.null(yvar))
    if ("data.table" %in% class(data)) data <- data.frame(data)
    if (!(yvar %in% target_cols)) {
      target_cols <- c(target_cols, yvar)
    }
    data_targets <- data[, which(names(data) %in% target_cols)]

    pred_merge <- do.call(
      cbind,
      (lapply(pred, function(x) x[[position]][, 1]))
    )
    colnames(pred_merge) <- gsub("fit_learner_(base|meta)_", "", names(pred))

    stopifnot(nrow(data_targets) == nrow(pred_merge))
    data_pred <- cbind(data_targets, pred_merge)

    return(data_pred)
  }


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
      ) %>%
      recipes::update_role(
        !!seq(length(chr_id_names), ncol(dt_sample))
      ) %>%
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
