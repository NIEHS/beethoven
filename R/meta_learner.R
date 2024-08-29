#' Combine prediction values from base learners
#' 
#' This function combines outcome data (observations) with the prediction
#' values from each base learner.
#' @keywords meta_learner
#' @param data data.frame(1). Full data.
#' @param pred list(1). List with base learner prediction values.
#' @param target_cols characters(1). Columns to retain from the full
#' data.frame.
#' @param yvar character(1). Outcome variable name.
#' @return a data.frame object, including the target columns from `data` and
#' the predictions for each base learner.
#' @export
attach_pred <-
  function(
    data,
    pred,
    target_cols = c("site_id", "time", "Event.Type", "lon", "lat"),
    yvar = "Arithmetic.Mean"
  ) {
    stopifnot(!is.null(data))
    stopifnot(!is.null(target_cols))
    stopifnot(!is.null(yvar))
    if (!(yvar %in% target_cols)) {
      target_cols <- c(target_cols, yvar)
    }
    data_targets <- data[, which(names(data) %in% target_cols)]

    pred_merge <- do.call(cbind, (lapply(pred, function(x) x[[1]][, 1])))
    colnames(pred_merge) <- unlist(lapply(pred, pred_colname))

    stopifnot(nrow(data_targets) == nrow(pred_merge))
    data_pred <- cbind(data_targets, pred_merge)

    return(data_pred)
  }


#' Assign column name to base learner prediction based on hyperparameters.
#' @keywords meta_learner
#' @param pred Base learner outcomes.
#' @return a character for column name.
#' @export
pred_colname <-
  function(
    pred
  ) {
    id <- substr(setdiff(names(pred[[2]]), ".config"), 1, 3)
    value <- as.character(pred[[2]][1, 1:(ncol(pred[[2]]) - 1)])
    pred_colname <- paste(paste0(id, value), collapse = "_")
    # random value appended to hyperparameter in case multiple models
    # use same hyperparameter combinations
    pred_colname_randomid <-
      paste(pred_colname, sample(1e4:1e5 - 1, 1), sep = "_")
    return(pred_colname_randomid)
}


#' Fit meta learner
#'
#' This function subsets the full data by column subsamples (rate=50%)
#' The optimal hyperparameter search is performed based on spatial,
#' temporal, and spatiotemporal cross-validation schemes.
#' As of version 0.4.0, the function relies on RMSE to select the
#' best hyperparameter set.
#' @keywords meta_learner
#' @param data data.frame. Full data.
#' @param p_col_sel numeric(1). Rate of column resampling. Default is 0.5.
#' @param rset rset object. Specification of training/test sets.
#' @param yvar character(1). Outcome variable name
#' @param xvar character. Feature names.
#' @param tune_iter integer(1). Bayesian optimization iterations.
#' Default is 50.
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
    p_col_sel = 0.5,
    rset = NULL,
    yvar = "Arithmetic.Mean",
    xvar = character(0),
    tune_iter = 50L
  ) {

    # define model
    meta_model <-
      parsnip::linear_reg(
        engine = "glmnet",
        mode = "regression",
        penalty = tune::tune(),
        mixture = tune::tune()
      )

    # define recipe
    meta_recipe <-
      recipes::recipe(
        data[1, ]
      ) %>%
      recipes::update_role(!!xvar) %>%
      recipes::update_role(!!yvar, new_role = "outcome")

    # define workflow from recipe and model
    meta_workflow <-
      workflows::workflow() %>%
      workflows::add_recipe(
        meta_recipe
      ) %>%
      workflows::add_model(meta_model)

    # tune hyperparameters per Bayesian optimization
    meta_tuned <-
      tune::tune_bayes(
        object = meta_workflow,
        resamples = rset,
        iter = tune_iter,
        control = tune::control_bayes(
          verbose = TRUE,
          save_pred = FALSE,
          save_workflow = TRUE
        ),
        metrics = yardstick::metric_set(
          yardstick::rmse, yardstick::mae, yardstick::rsq
        )
      )

    meta_wfparam <-
      tune::select_best(
        meta_tuned,
        metric = c("rmse", "rsq", "mae")
      )

    # finalize workflow with the best tuned hyperparameters
    meta_wfresult <- tune::finalize_workflow(meta_workflow, meta_wfparam)
    # Best-fit model
    meta_wf_fit_best <- parsnip::fit(meta_wfresult, data = data)

    meta_wflist <-
      list(
        meta_fitted = meta_wf_fit_best,
        meta_parameter = meta_wfparam,
        best_performance = meta_tuned
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
