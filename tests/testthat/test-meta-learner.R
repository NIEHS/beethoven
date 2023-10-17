#' @author SETgroup
#' @description
#' @title meta learner unit test
#'
#'


test_that("the meta learner fitting abides", {

  # Test data
  response <- 3 + rnorm(100)
  kfolds <- sample(rep(1:5, length.out = length(response)))
  predictor_list <- list(
    runif(length(response), min = 1, max = 10),
    rnorm(length(response)),
    rnorm(length(response))
  )
  names(predictor_list) <- c("var1", "var2", "var3")

  # Fit learner
  meta_learner_output <- meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  )

  # test the output of the meta-learner fit is a list
  expect_type(meta_learner_output, "list")

  # test that the meta-learner fit test-mean does not produce NA
  expect_true(sum(is.na(meta_learner_output[[1]]$yhat.test.mean)) == 0)

  # test that the meta-learner fit test does not produce NA
  expect_true(sum(is.na(meta_learner_output[[1]]$yhat.test)) == 0)

  # test that the meta-learner fit train-mean does not produce NA
  expect_true(sum(is.na(meta_learner_output[[1]]$yhat.train.mean)) == 0)

  # test that the meta-learner fit train set does not produce NA
  expect_true(sum(is.na(meta_learner_output[[1]]$yhat.train)) == 0)

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

})



test_that("the meta learner prediction abides", {
  skip()
  withr::local_package("ncdf4")
  withr::local_package("RNetCDF")
  withr::local_package("BART")

  aqs_sftime <- sf::st_read("../testdata/aqs-test-data.gpkg") |>
    sftime::st_as_sftime()

  # Test data
  response <- aqs_sftime$Arithmetic.Mean
  covariate <- matrix(data = rnorm(length(response) * 3), ncol = 3)

  locs <- runif(length(response))

  # generate simple BART object
  bart_obj <- mc.wbart(covariate, response)

  # Call the meta learner prediction function
  model_output <- meta_learner_predict(bart_obj, locs)

  # the test is running on the object named "output"
  expect_type(model_output, "ncdf4")

})
