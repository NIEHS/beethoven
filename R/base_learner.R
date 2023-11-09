### Base learner fit
## Design: generic function and switch to each function type
## Other else: integrating tidymodels + spatialsample for spatiotemporal CV

#' Fit base learner
#' @param learner character(1). Currently one of 'randomforest', 'xgboost',
#' and 'cnn'
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
#' @param sp_index character(1). Name of the unique spatial identifier
#' @param t_index character(2). Default is 'time'.
#' Name of the unique time identifier
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
  sp_index = "Site.ID",
  t_index = "time",
  return_full_object = FALSE
) {
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
#' @param sp_index character(1). Name of the unique spatial identifier
#' @param t_index character(2). Default is 'time'.
#' Name of the unique time identifier
#' @return data.frame
#' @author Insang Song
#' @import ranger
#' @import spatialsample
#' @export

fit_base_learner_ranger <- function() {
  ranger::ranger()

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
#' @param sp_index character(1). Name of the unique spatial identifier
#' @param t_index character(2). Default is 'time'.
#' Name of the unique time identifier
#' @return data.frame
#' @author Insang Song
#' @description This function uses torch as a backend.
#' Torch currently supports CPU, CUDA, and Metal (Apple Silicon graphics),
#' thus users can leverage available computational assets
#' to accelerate the model fitting.
#' @import torch
#' @import reticulate
#' @import spatialsample
#' @export

fit_base_learner_cnn <- function() {
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
#' @param sp_index character(1). Name of the unique spatial identifier
#' @param t_index character(2). Default is 'time'.
#' Name of the unique time identifier
#' @return data.frame
#' @author Insang Song
#' @importFrom xgboost xgb.train
#' @import spatialsample
#' @export

fit_base_learner_xgboost <- function() {
  xgboost::xgb.train()
}