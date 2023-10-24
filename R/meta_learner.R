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
    stop("Error in meta_learner_fit: 
         Base predictors need to be the same length")
  }
  # convert list to data.frame
  x_design <- as.data.frame(base_predictor_list)

  # Unique k-folds (typically 5 or 10)
  nk <- length(unique(kfolds))
  # Pre-allocate list of meta objects
  meta_fit_obj <- vector(mode = "list", length = nk)
  for (i in 1:nk) {
    # get the training and test sets
    x_tr <- x_design[kfolds != i, ]
    x_te <- x_design[kfolds == i, ]
    y_tr <- y[kfolds != i]
    # Fit the BART model
    meta_fit_obj[[i]] <- BART::mc.wbart(x.train = x_tr,
                                        y.train = y_tr,
                                        x.test = x_te)
  }
  return(meta_fit_obj)
}



#' meta_learner_predict - take the list of BART fit objects and prediction
#' location info to create meta_learner predictions. NOTE that the BART 
#' meta learner is not explicitly a spatiotemporal model. The S-T comes in 
#' from the covariate being S-T based
#'
#' @param meta_fit_obj list of BART objects from meta_learner_fit
#' @param cov_pred dataframe of covariates at prediction locations
#' @param pred_rast The terra rast file for the cov_pred locations. If it is 
#' not NULL, then the predictions willl be returned as a NetCDF (nc). If
#' NULL, then the predictions will be return as a dataframe
#' @return meta_pred_nc NetCDF (nc) file of the final meta learner predictions
#' @export
#'
#' @examples NULL
meta_learner_predict <- function(meta_fit_obj, cov_pred, pred_rast = NULL) {
  
  # Use the predict method
  meta_pred_vec <- predict(meta_fit_obj, new = cov_pred)
  
  # If the output is to be a netCDF (nc file)
  # Create a terra::rast file
  meta_pred <- terra::rast(pred_loc)
  sf::crs(meta_pred_nc) <- crs
  return(meta_pred)
}
