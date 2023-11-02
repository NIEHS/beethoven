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

convert_stobj_to_stdt <- function(stobj) {
  
  format <- class(stobj)[[1]]
  
  if (format == "data.frame" || format == "data.table") {
    if (any(!(c("lon", "lat", "time") %in% colnames(stobj)))) {
      stop("stobj does not contain lon, lat, time columns")
    }
    stdt <- data.table::as.data.table(stobj) 
  }
  
  if (format == "sf" || format == "sftime") {
    if (any(!(c("geometry", "time") %in% colnames(stobj)))) {
      stop("stobj does not contain geometry and time columns")
    }
    crs <- crs(stobj)
    stobj$lon <- sf::st_coordinates(st_obj[,1]) 
    stobj$lat <- sf::st_coordinates(st_obj[,2]) 
    stdt <- as.data.frame(stobj)
    stdt <- stdt[, geometry := NULL]
  }
  
  if (format == "SpatVector") {
    if (!("time") %in% names(stobj)) {
      stop("stobj does not contain time column")
    }
    crs <- crs(stobj)
    stdf <- as.data.frame(stobj, geom = "XY")
    names(stdf)[names(stdf) == 'x'] <- "lon"
    names(stdf)[names(stdf) == 'y'] <- "lat"
    stdt <- as.data.table(stdf)
  }
  
  if (format == "SpatRasterDataset") {
    crs <- crs(stobj)
    stdf <- as.data.frame(stobj[1], xy=T)
    colnames(stdf)[1] <- "lon"
    colnames(stdf)[2] <- "lat"
    # -- tranform from wide to long format
    stdf <- stdf %>% pivot_longer(cols=3:ncol(stdf),
                                           names_to='time',
                                           values_to=names(stobj)[1])

    for (var in names(stobj)[2:length(names(stobj))]){
      # test that the ts is identical to the ts of the 1st variable
      if (!(identical(names(stobj[var]), names(stobj[1])))) {
        stop("Error in SpatRastDataset: timeserie is different for at least 
             2 variables - or not ordered for one of these.")
      }
      df_var <- as.data.frame(stobj[var], xy=T)
      # -- tranform from wide to long format
      df_var <- df_var %>% pivot_longer(cols=3:ncol(df_var),
                                     names_to='time',
                                     values_to=var)
      stdf[, var] <- df_var[, var]
    }
    stdt <- as.data.table(stdf)
  }
  
  return(list("crs" = crs, "stdt" = stdt))
} 


#' meta_learner_predict - take the list of BART fit objects and prediction
#' location info to create meta_learner predictions. The BART
#' meta learner is not explicitly a S-T model, but the input covariates 
#' (outputs of each base learner) are S-T based. Therefore, the base_outputs 
#' input should be either an sf::sf-point or a terra::rast file format
#'
#' @param meta_fit list of BART objects from meta_learner_fit
#' @param base_outputs spatial data format containing the covariates (outputs of each base learner)
#' at prediction locations. Can be a SpatRaster, a SpatVector or an sf object.
#' @param nthreads integer(1). Number of threads used in BART::predict.wbart
#' @note  The predictions can be a rast or sf, which depends on the same
#' respective format of the covariance matrix input - cov_pred
#' @return meta_pred file of the final meta learner predictions, can be rast
#' or sf file
#' @export
#'
#' @examples NULL
#' @references https://rspatial.github.io/terra/reference/predict.html
meta_learner_predict <- function(meta_fit, base_outputs, nthreads = 2) {

  # Check prediction output type
  pred_format <- class(base_outputs)[[1]]
  valid_file_formats <- c("SpatRaster", "SpatVector", "sf")

  if (!any(pred_format %in% valid_file_formats)) {
    stop("Invalid Metalearner Predictor Matrix file format.
          Expected one of: ", paste(valid_file_formats, collapse = ", "))
  }

  # matrix where values are predicted
  mat_pred <- switch(pred_format,
    SpatRaster = as.matrix(base_outputs),
    SpatVector = as.matrix(as.data.frame(base_outputs)),
    sf = as.matrix(base_outputs)[,-ncol(base_outputs)])
  # pre-allocate
  meta_pred <- matrix(nrow = nrow(mat_pred), ncol = length(meta_fit))

  # return(mat_pred)
  # approach: convert SpatRaster and sf to N-by-K matrices
  # where K denotes the number of base learners
  # we assume df_pred is pre-cleaned and only includes the 
  # base learner predictions
  # then putting them into predict function.
  # SpatRaster: convert each layer to column vector then cbind
  #    double check if the vector conversion results are
  #    column or row ordered
  # sf: select fields then as.matrix

  # structure assumption:
  # multilayer SpatRaster -- row-order conversion in as.data.frame
  # multicolumn SpatVector -- long format
  # multicolumn sf -- long format

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

  # conserve the input object class
  # a more succinct way is possible...
  temp_pred <- base_outputs

  if (pred_format == "SpatRaster") {
    # numeric vector to raster
    meta_pred_out <- iter_pred(mat_pred_in = mat_pred)
    # meta_pred_out <- matrix(meta_pred_out, 
    #nrow = dim(base_outputs)[1], byrow = TRUE)
    result_pred <- terra::setValues(temp_pred[[1]], meta_pred_out)

  } else if (pred_format == "SpatVector") {

    meta_pred_out <- iter_pred(mat_pred_in = mat_pred) |>
      data.frame()
    names(meta_pred_out) <- "meta_pred_pm2.5"
    # meta_pred_out <- meta_pred_out |>
    #   matrix(ncol = 1) |>
    #   as.data.frame()
    temp_pred[] <- NULL
    result_pred <- cbind(temp_pred, meta_pred_out)
    
  } else if (pred_format == "sf") {

    mat_pred <- sf::st_drop_geometry(base_outputs)
    mat_pred <- as.matrix(mat_pred)
    meta_pred_out <- iter_pred(mat_pred_in = mat_pred)
    meta_pred_out <- meta_pred_out |>
      matrix(ncol = 1) |>
      as.data.frame()
    names(meta_pred_out) <- "meta_pred_pm2.5"
    temp_pred[] <- NULL
    result_pred <- cbind(temp_pred, meta_pred_out)

  } else {
    stop("Invalid Metalearner Predictor Matrix file format.
         Expected one of: ", paste(valid_file_formats, collapse = ", "))
  }

  return(result_pred)
}
