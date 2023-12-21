### Base learner fit
## Design: generic function and switch to each function type
## Potential improvement: integration to tidymodels

#' Check torch installation and load
#' @param default_device character(1). "cpu" or "cuda"
#' @returns NULL
#' @author Insang Song
#' @importFrom torch torch_is_installed
#' @importFrom torch torch_device
#' @importFrom torch cuda_is_available
#' @importFrom torch backends_mps_is_available
#' @export
check_and_load_torch <- function(
  default_device = c("cpu", "cuda", "mps")
) {
  default_device <- match.arg(default_device)

  if (torch::cuda_is_available()) {
    default_device <- "cuda"
  }
  if (torch::backends_mps_is_available() && default_device == "cpu") {
    message("Your device is capable of MPS, but default_device is set cpu.\n")
  }
  library(torch)
  torch::torch_is_installed()
  torch::torch_device(default_device)
}


#' Data preparation for base learners
#' @param learner
#' @param data stdt.
#' @param dependent_name Name of dependent variable. Default is "pm2.5"
#' @param independent_name character. Names of independent variables.
#' @returns A list of two matrices (except for cnn) or
#'  multidimensional arrays (cnn) depending on learners
#' @author Insang Song
#' @importFrom data.table as.data.table
#' @importFrom torch torch_tensor
#' @importFrom torch torch_reshape
#' @export
prep_base_learner <- function(
  learner = c("cnn", "randomforest", "xgboost"),
  data,
  dependent_name,
  independent_name
) {
  learner <- match.arg(learner)
  if (learner == "cnn") {
    check_and_load_torch()
  }

  ## data sorting: stdt is supposed to be sorted already

  ## ~cnn
  ymat <- as.matrix(data[, independent_name])
  xmat <- as.matrix(data[, dependent_name])

  ## cnn
  ymat <-
  torch::torch_tensor(
    torch::torch_reshape(
      data[, dependent_name],
      ...
    )
  )
  xmat <-
  torch::torch_tensor(
    torch::torch_reshape(
      data[, independent_name],
      ...
    )
  )

  res <- list(
    dependent = ymat,
    independent = xmat
  )
  return(res)
}



#' Fit base learner
#' @param learner character(1). Currently one of 'randomforest', 'xgboost',
#' and 'cnn'
#' @param covars stdt. See \link{convert_stobj_to_stdt}
#' @param dependent_name character(1). Name of the dependent variable.
#' @param independent_name character(1). Names of independent variables.
#' @param cv_mode character(1). One of
#' 'lolo (leave-one-location-out)',
#' 'loto (leave-one-time-out)',
#' 'lolto (leave-one-location-time-out)',
#' 'lblo (leave-block-location-out)',
#' 'lbto (leave-block-time-out)',
#' 'lblto (leave-block-location-time-out)', and
#' 'random (full random selection)'
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @param return_full_object logical(1).
#' TRUE will return the object whose class is dependent on the package in use,
#' whereas FALSE will only return the prediction with
#' spatial/temporal information. Full object is potentially helpful for
#' debugging.
#' @return data.frame
#' @author Insang Song
#' @description This function fits a selected base learner,
#' which is supposed to be one of random forest, XGBoost,
#' and convolutional neural network,
#' using one of spatiotemporal cross-validation approaches.
#' @export
fit_base_learner <- function(
    covars,
    learner = c("randomforest", "xgboost", "cnn"),
    dependent_name = "pm2.5",
    independent_name,
    cv_mode = c("lolo", "loto", "lolto", "random", "lblo", "lbto", "lblto"),
    cv_fold = 5L,
    return_full_object = FALSE) {
  learner <- tolower(learner)
  learner <- match.arg(learner)
  cv_mode <- match.arg(cv_mode)
  # type check



  # switch
  switch(learner,
    randomforest = fit_base_learner_ranger(),
    xgboost = fit_base_learner_xgboost(),
    cnn = fit_base_learner_cnn()
    # add more options with learner argument ...
  )
}

#' Fit random forests with ranger
#' @param covars stdt. See \link{convert_stobj_to_stdt}
#' @param dependent_name character(1). Name of the dependent variable.
#' @param independent_name character(1). Names of independent variables.
#' @param cv_mode character(1). One of 'lolo (leave-one-location-out)',
#' 'loto (leave-one-time-out)',
#' 'lolto (leave-one-location-time-out)',
#' 'lblo (leave-block-location-out)',
#' 'lbto (leave-block-time-out)',
#' 'random (full random selection)'
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @return data.frame
#' @author Insang Song
#' @export

fit_base_learner_ranger <- function(
    covars,
    dependent_name,
    independent_name,
    cv_mode,
    cv_fold) {
}



#' Fit convolutional neural networks with neural network library
#' @param covars stdt. See \link{convert_stobj_to_stdt}
#' @param dependent_name character(1). Name of the dependent variable.
#' @param independent_name character(1). Names of independent variables.
#' @param cv_mode character(1). One of 'lolo (leave-one-location-out)',
#' 'loto (leave-one-time-out)',
#' 'lolto (leave-one-location-time-out)',
#' 'lblo (leave-block-location-out)',
#' 'lbto (leave-block-time-out)',
#' 'random (full random selection)'
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @return data.frame
#' @author Insang Song
#' @description This function uses torch as a backend.
#' Torch currently supports CPU, CUDA, and Metal (Apple Silicon graphics),
#' thus users can leverage available computational assets
#' to accelerate the model fitting.
#' @export

fit_base_learner_cnn <- function(
    covars,
    dependent_name,
    independent_name,
    cv_mode,
    cv_fold) {
  # ...
  # keras or torch ...
  # check if MPS (for Apple Silicon) / CUDA is available ...
  # fit model (spt-array and 3d-pooling)
  # cv
}



#' Fit XGBoost model
#' @param covars stdt. See \link{convert_stobj_to_stdt}
#' @param dependent_name character(1). Name of the dependent variable.
#' @param independent_name character(1). Names of independent variables.
#' @param cv_mode character(1). One of 'lolo (leave-one-location-out)',
#' 'loto (leave-one-time-out)',
#' 'lolto (leave-one-location-time-out)',
#' 'lblo (leave-block-location-out)',
#' 'lbto (leave-block-time-out)',
#' 'random (full random selection)'
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @return data.frame
#' @author Insang Song
#' @export

fit_base_learner_xgboost <- function(
    covars,
    dependent_name,
    independent_name,
    cv_mode,
    cv_fold) {
}
