library(testthat)
source("/ddn/gs1/home/songi2/projects/beethoven/R/base_learner.R")

# Test case 1: Random Forest learner
test_that("base_learner_tune returns tuned random forest model", {
  # Create dummy data
  data <- list(
    list(xtrain = matrix(rgamma(150, 1, 0.3), nrow = 50, dimnames = list(NULL, c("c1", "c2", "c3"))), ytrain = matrix(sample(c(0, 1), 50, replace = TRUE), ncol = 1)),
    list(xtrain = matrix(rnorm(150, 4, 2.2), nrow = 50, dimnames = list(NULL, c("c1", "c2", "c3"))), ytrain = matrix(sample(c(0, 1), 50, replace = TRUE), ncol= 1))
  )

  # Define tuning specifications
  tunespec <- data.frame(
    mtry = c(2),
    min.node.size = c(1, 2)
  )

  tuned <- mirai::mirai({
    rex <- lapply(dat, function(d) {
      tune_res <- vector("list", length = nrow(tunespec))
      for (idx in seq_len(nrow(tunespec))) {
        tune_args <- tunespec[idx, ]
        tune_args <- as.list(tune_args)
        tune_args <- c(tune_args, list(ymat = d$ytrain, xmat = d$xtrain))

        tune_fit <- rlang::inject(foo(!!!tune_args))
        tune_res[[idx]] <- tune_fit$predictions
      }
      return(tune_res)
    })
    return(rex)
  }, dat = data, tunespec = tunespec, foo = base_learner_fit_ranger)

  tunedx <- mirai::call_mirai(tuned)$data



  # Call the function
  tuned_model <- base_learner_tune(data, learner = "randomforest", tunespec = tunespec)
  
  # Perform assertions
  expect_true(is.data.frame(tuned_model))
  expect_equal(nrow(tuned_model), nrow(tunespec) * length(data))
})

# Test case 2: XGBoost learner
test_that("base_learner_tune returns tuned XGBoost model", {
  # Create dummy data
  data <- list(
    list(xtrain = matrix(1:10, nrow = 5), ytrain = c(1, 0, 1, 0, 1)),
    list(xtrain = matrix(11:20, nrow = 5), ytrain = c(0, 1, 0, 1, 0))
  )
  
  # Define tuning specifications
  tunespec <- data.frame(
    max_depth = c(2, 3),
    eta = c(0.1, 0.2)
  )
  
  # Call the function
  tuned_model <- base_learner_tune(data, learner = "xgboost", tunespec = tunespec)
  
  # Perform assertions
  expect_true(is.data.frame(tuned_model))
  expect_equal(nrow(tuned_model), nrow(tunespec) * length(data))
})

# Test case 3: CNN learner
test_that("base_learner_tune throws an error for CNN learner", {
  # Create dummy data
  data <- list(
    list(xtrain = matrix(1:10, nrow = 5), ytrain = c(1, 0, 1, 0, 1)),
    list(xtrain = matrix(11:20, nrow = 5), ytrain = c(0, 1, 0, 1, 0))
  )
  
  # Define tuning specifications
  tunespec <- data.frame(
    filters = c(16, 32),
    kernel_size = c(3, 5)
  )
  
  # Call the function and expect an error
  expect_error(base_learner_tune(data, learner = "cnn", tunespec = tunespec))
})