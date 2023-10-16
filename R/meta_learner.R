#' meta_learner_fit
#' Fit a BART (Bayesian Additive Regression Tree) meta learner. It takes
#' predictions of other models such as kriging, GLM, machine learning models as
#' input and fits a BART Model
#' @param base_predictor_list - P x 1 list where P = p is a base predictor
#' vector (numeric). Each predictor vector should be the same length and
#' named.
#' @param y dependent variable
#' @param kfolds integer, index of k-folds for cross-validation. This should be
#' produced with regards to spatial and/or temporal considerations
#' @return meta_fit_obj object of meta learner
#' @export
#' @examples NULL
meta_learner_fit <- function(base_predictor_list,
                             kfolds, y) {
  # check lengths of each base predictor #add a test for names
  if (sapply(base_predictor_list, length, simplify = TRUE) |>
    stats::var() != 0) {
    print("WARNING: base predictors need to be the same length")
  }
  # convert list to data.frame
  x.design <- as.data.frame(base_predictor_list)

  # Unique k-folds (typically 5 or 10)
  n.k <- length(unique(kfolds))
  # Pre-allocate list of meta objects
  meta_fit_obj <- vector(mode = "list", length = n.k)
  for (i in 1:n.k) {
    # get the training and test sets
    x.tr <- x.design[i != n.k, ]
    x.te <- x.design[i == n.k, ]
    y.tr <- y[i != n.k]
    # Fit the BART model
    meta_fit_obj[[i]] <- BART::mc.wbart(x.tr, y.tr, x.test = x.te)
  }
  return(meta_fit_obj)
}



#' meta_learner_predict - take the list of BART fit objects and prediction
#' location info to create meta_learner predictions
#'
#' @param meta_fit_obj list of BART objects from meta_learner_fit
#' @param pred_loc sf or dataframe grid of prediction locations
#' @return meta_pred_nc NetCDF (nc) file of the final meta learner predictions
#' @export
#'
#' @examples NULL
meta_learner_predict <- function(meta_fit_obj, pred_loc) {
  # Use the predict method
  meta_pred_vec <- predict(meta_fit_obj, new = pred_loc)
  # Call the vec2nc function to create a NetCDF (nc) file from the
  # vector predictions
  meta_pred_nc <- vec2nc(meta_pred_vec)
  return(meta_pred_nc)
}


#' vec2nc takes a vector or matrix of predictions, some spatial information,
#' and create a NetCDF
#' @param pred_vec vector/matrix of predictions
#' @param pred_bbox an sf bounding box defining the domain of the NetCDF
#' @param field_names vector of field names for each layer in the NetCDF
#' @return nc_out a NetCDF file
#' @export
#'
#' @examples NULL
vec2nc <- function(pred_vec, pred_bbox, field_names) {
  nc_out <- pred_vec + pred_bbox
  names(nc_out) <- field_names
  return(nc_out)
}
