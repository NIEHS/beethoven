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
#' location info to create meta_learner predictions. The BART
#' meta learner is not explicitly a S-T model, but the input covariates are
#' S-T based. Therefore, the cov_pred input should be either an sf::sf-point or
#' a terra::rast file format
#'
#' @param meta_fit_obj list of BART objects from meta_learner_fit
#' @param cov_pred dataframe of covariates at prediction locations
#' @note  The predictions can be a rast or sf, which depends on the same
#' respective format of the covariance matrix input - cov_pred
#' @return meta_pred file of the final meta learner predictions, can be rast
#' or sf file
#' @export
#'
#' @examples NULL
#' @references https://rspatial.github.io/terra/reference/predict.html
meta_learner_predict <- function(meta_fit_obj, cov_pred) {

  # Check prediction output type
  valid_file_formats <- c("rast", "sf")

  cov_pred_format <- class(cov_pred)[[1]]

  # Check if the file extension is one of the expected formats
  if (!(cov_pred_format %in% valid_file_formats)) {
    stop("Invalid Metalearner Predictor Matrix file format. 
         Expected one of: ", paste(valid_file_formats, collapse = ", "))
  }


  # Use the predict method, which is valid in terra for both
  # rast based covariates and sf based covariates

  meta_pred <- predict(model = meta_fit_obj, object = cov_pred)


  return(meta_pred)
}
