### Base learner fit
## Design: generic function and switch to each function type
## Potential improvement: integration to tidymodels
## TODO: validation set in training set

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

  if (!require(torch)) {
    install.packages("torch")
    library(torch)
  }
  if (!torch::cuda_is_available()) {
    if (default_device == "cuda") {
      message("There is no device found to use CUDA. Trying other devices...")
    }
  }
  if (!torch::backends_mps_is_available()) {
    if (default_device == "mps") {
      message("MPS is not available in your system. Setting cpu as default.")
      default_device <- "cpu"
    }
  }
  torch::torch_is_installed()
  torch::torch_device(default_device)
}


#' Data preparation for base learners
#' @param learner character(1). One of 'cnn', 'randomforest', 'xgboost'
#' @param data stdt. see \link{\code{convert_stobj_to_stdt}}
#' @param dependent_name Name of dependent variable. Default is "pm2.5"
#' @param independent_name character. Names of independent variables.
#' @returns A list of two matrices (except for cnn) or
#'  multidimensional arrays (cnn) depending on learners
#' @author Insang Song
#' @importFrom data.table as.data.table
#' @importFrom torch torch_tensor
#' @importFrom torch torch_reshape
#' @export
base_learner_prep <- function(
  learner = c("cnn", "randomforest", "xgboost"),
  data,
  dependent_name = "pm2.5",
  independent_name
) {
  learner <- match.arg(learner)
  # read data.table from stdt
  data <- data.frame(data[[1]])

  if (learner == "cnn") {
    ## data sorting: stdt is supposed to be sorted already
    check_and_load_torch()

    # dimensions
    dim_s <- dim(unique(data[, 1:2]))[1]
    dim_t <- dim(unique(data[, 3]))[1]
    dim_p <- length(independent_name)

    ymat <-
      torch::torch_tensor(
        torch::torch_reshape(
          matrix(data[, dependent_name], ncol = 1L),
          list(dim_s, dim_t, dim_p)
        )
      )
    xmat <-
      torch::torch_tensor(
        torch::torch_reshape(
          data[, independent_name],
          list(dim_s, dim_t, dim_p)
        )
      )
  } else {
    ymat <- matrix(data[, dependent_name], ncol = 1L)
    xmat <- as.matrix(data[, independent_name])
  }

  res <- list(
    ymat = ymat,
    xmat = xmat
  )
  return(res)
}


#' Split training-test data for base learner fitting
#' @param learner character(1). Currently one of 'randomforest', 'xgboost',
#' and 'cnn'
#' @param ymat data.frame or matrix. Dependent variable.
#' @param xmat data.frame or matrix. Independent variables.
#' @param cv_index integer. Index per cross-validation method.
#' See \code{?generate_cv_index} for details.
#' @param fun base_learner_fit_*
#' @param ... Arguments passed to the argument \code{fun}
#' @returns List of 4 with xtrain, ytrain, xtest, and ytest.
#' @author Insang Song
#' @export
base_learner_cv_fit <- function(
  learner,
  ymat,
  xmat,
  cv_index,
  fun,
  ...
) {

  cviter <- sort(unique(cv_index))
  cvlist <- vector("list", length = length(cviter))

  for (iter in cviter) {
    # row
    xtrain <- xmat[cviter != iter, ]
    xtest <- xmat[cviter == iter, ]
    ytrain <- ymat[cviter != iter, ]
    ytest <- ymat[cviter == iter, ]

    # # to matrix
    # xtrain <- as.matrix(xtrain)
    # xtest <- as.matrix(xtest)
    # ytrain <- as.matrix(ytrain)
    # ytest <- as.matrix(ytest)

    train_fitted <-
      fun(ymat = ytrain,
          xmat = xtrain,
          ...)
    test_fitted <-
      switch(learner,
        randomforest = predict(train_fitted, data.frame(cbind(ytest, xtest))),
        xgboost = predict(train_fitted, xtest),
        cnn = stop("cnn prediction is not yet implemented.\n")
      )
    # predict(train_fitted, ytest, xtest)
    cvlist[[iter]] <-
      list(trained = train_fitted,
           tested = test_fitted)
  }

  return(cvlist)

}


#' Fit base learner
#' @param data stdt. See \link{\code{convert_stobj_to_stdt}}
#' @param learner character(1). Currently one of 'randomforest', 'xgboost',
#' and 'cnn'
#' @param dependent_name character(1). Name of the dependent variable.
#' @param independent_name character(1). Names of independent variables.
#' @param cv_mode character(1). One of
#' \code{'lolo'} (leave-one-location-out),
#' \code{'loto'} (leave-one-time-out),
#' \code{'lolto'} (leave-one-location-time-out),
#' \code{'lblo'} (leave-block-location-out),
#' \code{'lbto'} (leave-block-time-out),
#' \code{'lblto'} (leave-block-location-time-out), and
#' \code{'random'} (full random selection)
#' @param cv_fold integer(1). Number of folds for cross-validation.
#' @param sp_fold integer(1). Number of subfolds for spatial blocks.
#' @param t_fold integer(1). Number of subfolds for temporal blocks.
#' @param blocks integer(2)/sf/SpatVector object.
#' @param block_id character(1). The unique identifier of each block.
#' @param return_full_object logical(1).
#' \code{TRUE} will return the object whose class is dependent on
#' the package in use, whereas \code{FALSE} will only return the prediction
#' with spatial/temporal information. Full object is potentially helpful for
#' debugging.
#' @param ... Arguments passed to \code{base_learner_fit_*}.
#' @seealso
#' \link{base_learner_cv_fit}
#' \link{base_learner_fit_xgboost}
#' \link{base_learner_fit_ranger}
#' \link{base_learner_fit_cnn}
#' \link[xgboost]{xgboost}, \link[ranger]{ranger}
#' @returns List of length \code{cv_fold}, \code{sp_fold * t_fold}, or
#' \code{prod(blocks)} (when \code{blocks} is a numeric object of length 2) /
#' \code{nrow(blocks)} (\code{blocks} is sf or SpatVector).
#' @author Insang Song
#' @description This function fits a selected base learner, which is supposed
#' to be one of random forest, XGBoost, and convolutional neural network,
#' using one of spatiotemporal cross-validation approaches.
#' @export
base_learner_fit <- function(
    data,
    learner = c("randomforest", "xgboost", "cnn"),
    dependent_name = "pm2.5",
    independent_name = NULL,
    cv_mode = c("lolo", "loto", "lolto", "random", "lblo", "lbto", "lblto"),
    cv_fold = 5L,
    sp_fold = NULL,
    t_fold = NULL,
    blocks = NULL,
    block_id = NULL,
    return_full_object = FALSE,
    ...) {
  if (is.null(independent_name)) {
    stop("independent_name cannot be null.\n")
  }
  if (!is_stdt(data)) {
    stop("data should be stdt. See ?convert_stobj_to_stdt.\n")
  }

  learner <- tolower(learner)
  learner <- match.arg(learner)
  cv_mode <- match.arg(cv_mode)

  cv_index <-
    generate_cv_index(covars = data,
                      cv_mode = cv_mode,
                      cv_fold = cv_fold,
                      sp_fold = sp_fold,
                      t_fold = t_fold,
                      blocks = blocks,
                      block_id = block_id)

  data_prep <-
    base_learner_prep(
      learner = learner,
      data = data,
      dependent_name = dependent_name,
      independent_name = independent_name
    )

  # switch and assign actual function to run
  run_foo <-
    switch(learner,
           randomforest = base_learner_fit_ranger,
           xgboost = base_learner_fit_xgboost,
           cnn = base_learner_fit_cnn)
  # cv fit and test
  cv_res <-
    base_learner_cv_fit(
      learner = learner,
      ymat = data_prep$ymat,
      xmat = data_prep$xmat,
      cv_index = cv_index,
      fun = run_foo,
      ...
    )
  return(cv_res)
}

#' Fit random forests with ranger
#' @param ymat data.frame or matrix. Dependent variable.
#' @param xmat data.frame or matrix. Independent variables.
#' @param ... Arguments passed to \code{ranger::ranger}
#' @return ranger object.
#' @author Insang Song
#' @importFrom ranger ranger
#' @export
base_learner_fit_ranger <- function(
  ymat,
  xmat,
  ...
) {
  ranger::ranger(y = ymat, x = xmat, ...)
}


#nocov start
#' Fit convolutional neural networks with neural network library
#' @param ymat torch::torch_tensor. Dependent variable.
#' @param xmat torch::torch_tensor. Independent variables.
#' @param ... Arguments passed to fitting function
#' @return torch-compatible object
#' @author Insang Song
#' @description This function uses torch as a backend.
#' Torch currently supports CPU, CUDA, and Metal (Apple Silicon graphics),
#' thus users can leverage available computational assets
#' to accelerate the model fitting.
#' @export
base_learner_fit_cnn <- function(
    ymat,
    xmat,
    ...) {
  # check if MPS (for Apple Silicon) / CUDA is available ...
  # fit model (spt-array and 3d-pooling)
  return(NULL)
}
#nocov end

#' Fit XGBoost model
#' @param ymat data.frame or matrix. Dependent variable.
#' @param xmat data.frame or matrix. Independent variables.
#' @param ... Arguments passed to \code{xgboost::xgboost}
#' @return xgboost object
#' @author Insang Song
#' @importFrom xgboost xgb.train
#' @export
base_learner_fit_xgboost <- function(
    ymat,
    xmat,
    ...) {
  xgboost::xgboost(data = xmat, label = ymat, ...)
}



#' Return cvfold outcome values
#' @param data stdt. See \link{\code{convert_stobj_to_stdt}}
#' @param cv_index integer. Index per cross-validation method.
#' See \link{\code{generate_cv_index}}
#' @param dependent_name character(1). Name of the dependent variable.
#' @author Insang Song
#' @returns List length of \code{max(cv_index)}
#' with numeric vectors
#' @export
base_learner_cv_outcome <-
  function(
    data,
    cv_index,
    dependent_name
  ) {
    ys <- data.frame(data$stdt)
    ys <- ys[, dependent_name]
    cv_index_l <- split(cv_index, seq_len(max(cv_index)))
    ycvfolds <- lapply(cv_index_l, function(x) ys[cv_index == x])
    return(ycvfolds)
  }