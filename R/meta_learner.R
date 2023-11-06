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
  
  # Unnamed base_predictor_list is not accepted
  if (is.null(names(base_predictor_list)) ||
    any(is.na(names(base_predictor_list)))) {
      stop("base_predictor_list should be a named list.\n")
  }

  # check lengths of each base predictor #add a test for names
  if (sapply(base_predictor_list, length, simplify = TRUE) |>
        stats::var() != 0) {
    stop("Error in meta_learner_fit:
         Base predictors need to be the same length")
  }

  # check that length of base predictors is the same than y
  if(lengths(base_predictor_list)[1] != length(y)) {
    stop("Error in meta_learner_fit:
         Predictors and response are not the same length")
  }

  # check that length of kfolds is the same than y
  if(length(kfolds) != length(y)) {
    stop("Error in meta_learner_fit:
         kfolds vector and response are not the same length")
  }

  # check that base_predictor_list only contains only numeric
  if (any(sapply(base_predictor_list, class) != "numeric")) {
    stop("Error in meta_learner_fit:
         Some of base predictors are not numeric")
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
    meta_fit_obj[[i]] <- BART::wbart(
      x.train = x_tr,
      y.train = y_tr,
      x.test = x_te
    )
  }
  return(meta_fit_obj)
}


#' meta_learner_predict - take the list of BART fit objects and 
#' predictions of baselearners to create meta_learner predictions. The BART
#' meta learner is not explicitly a S-T model, but the input covariates 
#' (outputs of each base learner) are S-T based.
#'
#' @param meta_fit list of BART objects from meta_learner_fit
#' @param base_outputs stdt object (see convert_spacetime_data.R): list with datatable containing lat, lon, time and the covariates
#' (outputs of each base learner) at prediction locations and crs. 
#' @param nthreads integer(1). Number of threads used in BART::predict.wbart
#' @note  The predictions can be a rast or sf, which depends on the same
#' respective format of the covariance matrix input - cov_pred
#' @return meta_pred: the final meta learner predictions
#' @importFrom BART predict.wbart
#' @importFrom data.table .SD
#' @importFrom data.table .SDcols
#' @importFrom data.table as.data.table
#' @export
#'
#' @examples NULL
#' @references https://rspatial.github.io/terra/reference/predict.html
meta_learner_predict <- function(meta_fit, base_outputs_stdt, nthreads = 2) {

  if (!(identical(class(base_outputs_stdt), c("list", "stdt")))) {
    stop("Error: param base_outputs_stdt is not in stdt format.")
  }

  base_outputs <- base_outputs_stdt$stdt

  if (any(!(colnames(meta_fit[[1]]$varcount) %in% colnames(base_outputs)))) {
    stop("Error: baselearners list incomplete or with wrong names")
  }

  # extract baselearners predictions used in metalearner
  base_name_index <- seq(1, ncol(base_outputs$stdt))
  # changed to integer indices
  # as we impose the fixed column order in stdt objects.
  spt_name_index <- grep("(lon|lat|time)", colnames(base_outputs$stdt))
  base_name_index <- base_name_index[-spt_name_index]
  mat_pred <- as.matrix(base_outputs[, .SD, .SDcols = base_name_index])

  # pre-allocate
  meta_pred <- matrix(nrow = nrow(mat_pred), ncol = length(meta_fit))

  iter_pred <- function(
    meta_fit_in = meta_fit,
    mat_pred_in,
    meta_pred_in = meta_pred,
    nthreads_in = nthreads) {

    for (i in seq_along(meta_fit_in)) {
      meta_pred_in[, i] <- BART:::predict.wbart(
        object = meta_fit_in[[i]],
        newdata = mat_pred_in,
        mc.cores = nthreads_in) |>
        apply(2, mean)
    }
    meta_pred_out <- apply(meta_pred_in, 1, mean)
    return(meta_pred_out)
  }

  meta_pred_out <- iter_pred(mat_pred_in = mat_pred)
  meta_pred_out <- meta_pred_out |>
    matrix(ncol = 1) |>
    data.table::as.data.table()
  names(meta_pred_out) <- "meta_pred"
  meta_pred_out <- cbind(base_outputs[, spt_name_index], meta_pred_out)
  meta_pred_out <- list("stdt" = meta_pred_out,
                        "crs_dt" = base_outputs_stdt$crs_dt)
  class(meta_pred_out) <- c("list", "stdt")

  return(meta_pred_out)
}
