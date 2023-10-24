#' @author SETgroup
#' @description
#' @title meta learner unit test
#'
#'


test_that("the meta learner abides", {

##### Test Data
  
  # Test data
  response <- 3 + rnorm(100)
  kfolds <- sample(rep(1:5, length.out = length(response)))
  predictor_list <- list(
    runif(length(response), min = 1, max = 10),
    rnorm(length(response)),
    rnorm(length(response))
  )
  names(predictor_list) <- c("var1", "var2", "var3")

##### Fit learner
  meta_learner_output <- meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  )

##### Tests on the meta learning fitting (i.e. estimation)  
  
  # test the output of the meta-learner fit is a list
  expect_type(meta_learner_output, "list")

  # test that the meta-learner fit test-mean does not produce NA
  expect_true(any(is.na(meta_learner_output[[1]]$yhat.test.mean)))

  # test that the meta-learner fit test does not produce NA
  expect_true(any(is.na(meta_learner_output[[1]]$yhat.test)))

  # test that the meta-learner fit train-mean does not produce NA
  expect_true(any(is.na(meta_learner_output[[1]]$yhat.train.mean)))

  # test that the meta-learner fit train set does not produce NA
  expect_true(any(is.na(meta_learner_output[[1]]$yhat.train)))

  # test that it throws an error when base learners are different length
  predictor_list <- list(
    runif(length(response), min = 1, max = 10),
    rnorm(length(response) - 1),
    rnorm(length(response))
  )
  names(predictor_list) <- c("var1", "var2", "var3")

  expect_error(meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  ), "Error in meta_learner_fit: 
         Base predictors need to be the same length")
  
  
##### Tests on the meta learner prediction 
  # Test locations as data frame 
  covariate.pred <- matrix(1:(10^2),
              nrow = 10)
  
  # Call the meta learner prediction function
  model_output <- meta_learner_predict(meta_learner_output, locs)
  
  # the test is running on the object named "output"
  expect_type(model_output, "ncdf4")
  

})



