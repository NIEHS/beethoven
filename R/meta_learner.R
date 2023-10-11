#' meta_learner_fit
#' Fit our meta learner. It takes predictions of other models such as
#' kriging, GLM, machine learning models as input and fits a new model
#' @param base_predictor_list - a list where each
#' @param meta_learner_obj an exported S3/S4/function that is the meta learner
#' @param train_loc sf geospatial information on training locations
#' @param kfolds integer, index of k-folds for cross-validation. This should be
#' produced with regards to spatial and/or temporal considerations
#' @return meta_fit_obj object of meta learner
#' @export
#' @examples NULL
meta_learner_fit <- function(base_predictor_list, meta_learner_obj,
                             train_loc, kfolds) {

  # Use the exported meta learner to fit the model
  meta_fit_obj <- meta_learner_obj(base_predictor_list, train_loc, kfolds)
  return(meta_fit_obj)
}



#' meta_learner_predict - take the meta_fit_obj and prediction location info
#' to create meta_learner predictions
#'
#' @param meta_fit_obj S3/S4 object from meta_learner_fit
#' @param pred_grid sf or dataframe grid of prediction locations
#' @return meta_pred_nc NetCDF (nc) file of the final meta learner predictions
#' @export
#'
#' @examples
meta_learner_predict <- function(meta_fit_obj, pred_loc) {

  #Use the predict method
  meta_pred_vec <- predict(meta_fit_obj, new = pred_loc)
  # Call the vec2nc function to create a NetCDF (nc) file from the
  # vector predictions
  meta_pred_nc <- vec2nc(meta_pred_vec)
  return(meta_pred_nc)

}
