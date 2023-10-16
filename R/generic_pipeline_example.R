# Generic script to apply a base learner to data and then a meta learner
#
#' define a base learner dummy: this illustrates one way to wrap a learning
#' algorithm and return an object that makes predictions
#'
#' @param response the dependent variable, numeric, vector
#' @param covariate independent variables, design matrix, numeric
#' @param obs_locs geospatial locations as sf object
#' @param model_attr option model attributes or metadata
#' @export

generic_base_learner <- function(response, covariate, obs_locs, model_attr) {
  # check that all required parameters are present
  if (any(is.na(model_attr))) print("Null attributes")
  # fit the learner to the data
  params <- c(length(response), length(covariate), length(model_attr))
  # return a function that makes predictions?
  predict_fun <- function(pred_locs) {
    # apply the learner to the data
    preds <- stats::rnorm(1, mean = length(params) + length(pred_locs))
    return(preds)
  }
  return(predict_fun)
}


# a generic meta learner that takes as input the set of fitted base predictors
# and a set of locations at which to make predictions
#' Title Generic_Meta_Learner
#'
#' @param base_predictor_list list of the base predictor output
#' @param pred_loc geospatial information on the locations for predicting
#'
#' @return
#' mean and variance of the meta learner at prediction locations
#' @export
#'
#' @examples NULL
generic_meta_learner <- function(base_predictor_list, pred_loc) {
  # check that inputs are as expected
  if (any(is.na(base_predictor_list)) || any(is.na(pred_loc))) print("Null input")
  # for each prediction location, compute the set of predictions then apply meta
  n_predictors <- length(base_predictor_list)
  n_pred_locs <- length(pred_loc)
  meta_pred_vec <- rep(0, n_pred_locs)
  # can be parallelized
  for (loc_idx in 1:n_pred_locs) {
    p_loc <- pred_loc[loc_idx]
    pred_vec <- rep(0, n_predictors)
    for (p_idx in 1:n_predictors) {
      base_learner_predictor_i <- base_predictor_list[[p_idx]]
      pred_vec[p_idx] <- base_learner_predictor_i(p_loc)
    }
    # apply meta_learner to base learner predictions
    meta_pred_vec[loc_idx] <- pred_vec[1]
  }
  return(meta_pred_vec)
}


# a function that takes a set of base learners and a meta learner,
# fits the base learners, and then makes predictions with the meta learner
# at the prediction locations
#' Title Build_Pipeline
#'
#' @param base_learner_list list of the base predictor output
#' @param base_attr_list list of the base predictor attributes
#' @param meta_learner list of meta learner attributes
#' @param response  the dependent variable, numeric, vector
#' @param covariate  independent variables, design matrix, numeric
#' @param obs_locs geospatial locations as sf object
#' @param pred_locs geospatial information on the locations for predicting
#'
#' @return put it all together
#' @export
#'
#' @examples NULL
build_pipeline <- function(
    base_learner_list,
    base_attr_list,
    meta_learner,
    response,
    covariate,
    obs_locs,
    pred_locs) {
  base_predictor_list <- list()
  # each entry in base learner list is a function
  for (bl_idx in 1:length(base_learner_list)) {
    curr_base_learner <- base_learner_list[[bl_idx]]
    model_attr <- base_attr_list[[bl_idx]]
    curr_base_predictor <- curr_base_learner(
      response = response,
      covariate = covariate,
      obs_locs = obs_locs,
      model_attr = model_attr
    )
    base_predictor_list <- c(base_predictor_list, curr_base_predictor)
  }
  predictions <- sapply(pred_locs,
    FUN = function(p_loc) meta_learner(base_predictor_list, p_loc)
  )
  return(predictions)
}


# creating a list of generic base learners
base_learner_list <- list(generic_base_learner, generic_base_learner)
# populating the model attributes:  depends on the type of model used
base_learner_attr <- list(
  list("bins" = 10, "threshold" = .1),
  list("NN_depth" = 4, "NN_width" = 100, "learn_rate" = .1)
)
# simulating some made up data
covariate <- stats::rnorm(100)
obs_locs <- seq(0, 10, length.out = 100)
response <- covariate * 5 + stats::runif(100)
# specifying the locations for predictions
pred_locs <- -5:15
# building the pipeline and outputting predictions
build_pipeline(
  base_learner_list,
  base_learner_attr,
  generic_meta_learner,
  response,
  covariate,
  obs_locs,
  pred_locs
)
