#' @author SETgroup
#' @description
#' @title meta learner unit test
#'
#'


test_that("the meta learner fitting", {

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

  # test the output of the metalearner is a list
  expect_type(meta_learner_output, "list")

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



test_that("the meta learner output is netcdf", {
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
