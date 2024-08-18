# nocov start
#' Fit meta learner
#'
#' This function subsets the full data by column subsamples (rate=50%)
#' The optimal hyperparameter search is performed based on spatial,
#' temporal, and spatiotemporal cross-validation schemes and continuous
#' ranked probability score (CRPS) with Bayesian optimization.
#' @keywords meta_learner
#' @param data data.frame. Full data.
#' @param p_col_sel numeric(1). Rate of column resampling. Default is 0.5.
#' @param rset rset object. Specification of training/test sets.
#' @param yvar character(1). Outcome variable name
#' @param xvar character. Feature names.
#' @param tune_iter integer(1). Bayesian optimization iterations.
#' @return best model object.
#' @export
fit_meta_learner <-
  function(
    data,
    p_col_sel = 0.5,
    rset = NULL,
    yvar = "pm2.5",
    xvar = character(0),
    tune_iter = 50L
  ) {

    main_glmnet <-
      parsnip::linear_reg(
        engine = "glmnet",
        mode = "regression",
        penalty = tune::tune(),
        mixture = tune::tune()
      )

    # define
    wf_glmnet <-
      workflows::workflow() %>%
      workflows::add_variables(
        outcomes = yvar,
        predictors = xvar,
      ) %>%
      workflows::add_model(main_glmnet)

    sel_glmnet <-
      tune::tune_bayes(
        object = wf_glmnet,
        resamples = rset,
        iter = tune_iter
      ) %>%
      tune::select_best()

    #
    metrics <-
      yardstick::metric_set(
        yardstick::rmse, yardstick::mae
      )

  }


# nocov end
