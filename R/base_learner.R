### Base learner fit
## Design: generic function and switch to each function type
## Other else: integrating tidymodels + CAST for spatiotemporal CV



#' Fit base learner
#' @param learner character(1). Currently one of 'randomforest', 'xgboost',
#' and 'cnn'
#' @param covars
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
#' @description 
#' @importFrom 
#' @export

sf:::.

fit_base_learner <- function(
  covars,
  learner = c("randomforest", "xgboost", "cnn"),
  dependent_name = "pm2.5",
  independent_name,
  cv_mode = c("lolo", "loto", "lolto", "random", "lblo", "lbto"),
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
    cnn = fit_base_learner_keras()
    # add more options with learner argument ...
  )
}

#' Fit random forests with ranger
#' @param covars
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
#' @author 
#' @description fit base learner using covariate data.frame/matrix
#' @import ranger
#' @export

fit_base_learner_ranger <- function(){
    ranger::ranger()

}



#' Fit convolutional neural networks with keras
#' @param covars
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
#' @description 
#' @import torch
#' @export

fit_base_learner_torch <- function(){
  # ...
  # keras or torch ...
  # check if CUDA is available ...
  # fit model (spt-array and 3d-pooling)
  # cv

}



#' Fit XGBoost model
#' @param covars
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
#' @description 
#' @importFrom xgboost xgb.train
#' @export

fit_base_learner_xgboost <- function(){
  xgboost::xgb.train()
}
