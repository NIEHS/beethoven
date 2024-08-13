

#' Fit meta learner
#'
#' This function subsets the full data by column subsamples (rate=50%)
#' The optimal hyperparameter search is performed based on spatial,
#' temporal, and spatiotemporal cross-validation schemes and continuous
#' ranked probability score (CRPS) with Bayesian optimization.
#'
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


# Step 1: Create a Function to Calculate CRPS
crps_metric <- function(data, truth, estimate, ...) {
  truth <- data[[truth]]
  estimate <- data[[estimate]]

  # Calculate CRPS for each observation
  crps_values <- scoringRules::crps_sample(truth, estimate)

  # Return the mean CRPS
  mean(crps_values)
}


crps <- function(data, ...) {
  UseMethod("crps")
}

# Step 2: Define the Custom Metric for yardstick
crps_yardstick <- yardstick::new_numeric_metric(
  fn = crps_metric,
  direction = "maximize"
)

crps.data.frame <- function(data, truth, estimate, ...) {
  truth <- rlang::eval_tidy(rlang::enquo(truth), data)
  estimate <- rlang::eval_tidy(rlang::enquo(estimate), data)

  result <- crps_metric(truth, estimate)

  tibble::tibble(
    .metric = "crps",
    .estimator = "standard",
    .estimate = result
  )
}

# crps.data.frame <- function(data, truth, estimate, ...) {
#   truth <- rlang::eval_tidy(rlang::enquo(truth), data)
#   estimate <- rlang::eval_tidy(rlang::enquo(estimate), data)

#   yardstick::numeric_metric_summarizer(
#     name = "crps",
#     fn = crps_yardstick,
#     data = data,
#     truth = !!rlang::enquo(truth),
#     estimate = !!rlang::enquo(estimate),
#     ...
#   )
# }

# Step 3: Use the Custom Metric in Your Workflow
# Example data
# set.seed(123)
# data1 <- data.frame(
#   actual = rnorm(100, 10, 2),
#   pred = rnorm(100, 8, 1)
# )

# # Evaluate the custom CRPS metric
# data1 %>%
#   crps(truth = "actual", estimate = "pred")

# crps_metric(data1, "actual", "pred")
