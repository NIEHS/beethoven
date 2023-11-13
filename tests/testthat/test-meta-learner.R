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
  meta_model <- meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  )
  
  # test the output of the meta-learner fit is a list
  expect_type(meta_model, "list")
  
  # test that the meta-learner fit test-mean does not produce NA
  expect_true(sum(is.na(meta_model[[1]]$yhat.test.mean)) == 0)
  
  # test that the meta-learner fit test does not produce NA
  expect_true(sum(is.na(meta_model[[1]]$yhat.test)) == 0)
  
  # test that the meta-learner fit train-mean does not produce NA
  expect_true(sum(is.na(meta_model[[1]]$yhat.train.mean)) == 0)
  
  # test that the meta-learner fit train set does not produce NA
  expect_true(sum(is.na(meta_model[[1]]$yhat.train)) == 0)
  
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
  
  # test that it throws an error when base learners 
  # and response are different length
  predictor_list <- list(
    runif(length(response), min = 1, max = 10),
    rnorm(length(response)),
    rnorm(length(response))
  )
  names(predictor_list) <- c("var1", "var2", "var3")
  response <- 3 + rnorm(99)
  
  expect_error(meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  ), "Error in meta_learner_fit:
         Predictors and response are not the same length")
  
  # test that it throws an error when kfolds
  # and response are different length
  response <- 3 + rnorm(100)
  kfolds <- sample(rep(1:5, length.out = length(response) - 1))
  
  expect_error(meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  ), "Error in meta_learner_fit:
         kfolds vector and response are not the same length")
  
  # test that it throws an error when some of predictors are not numeric
  kfolds <- sample(rep(1:5, length.out = length(response)))
  predictor_list[[3]] <-
    c(1, rep("I love Roquefort cheese", length(response) - 1))
  
  expect_error(meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  ), "Error in meta_learner_fit:
         Some of base predictors are not numeric")
})



test_that("the meta learner prediction abides", {
  response <- 3 + rnorm(100)
  kfolds <- sample(rep(1:5, length.out = length(response)))
  predictor_list <- list(
    "baselearner1" = runif(100, min = 1, max = 10),
    "baselearner2" = rnorm(100),
    "baselearner3" = pi + rnorm(100)
  )
  meta_model <- meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  )
  
  # new data to predict
  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  df$baselearner1 <- runif(50, min = 1, max = 10)
  df$baselearner2 <- pi + rnorm(50)
  df$baselearner3 <- rnorm(50)
  base_outputs <- data.table::as.data.table(df)
  base_outputs_stdt <- list("stdt" = base_outputs, 
                            "crs_dt" = "EPSG:4326") 
  class(base_outputs_stdt) <- c("list", "stdt")
  
  expect_no_error(meta_learner_predict(meta_model, base_outputs_stdt))
  model_output <- meta_learner_predict(meta_model, base_outputs_stdt)
  expect_identical(class(model_output), c("list", "stdt"))
  expect_true(all(c("lon", "lat", "time") %in% colnames(model_output$stdt)))
  
  # check that datatable input is not changed by the function
  expect_identical(base_outputs_stdt$stdt, data.table::as.data.table(df))
  
  # check it does not work when one baselearner is missing
  base_outputs[, baselearner3 := NULL]
  base_outputs_stdt <- list("stdt" = base_outputs, 
                            "crs_dt" = "EPSG:4326") 
  class(base_outputs_stdt) <- c("list", "stdt")
  expect_error(meta_learner_predict(meta_model, base_outputs_stdt), 
               "Error: baselearners list incomplete or with wrong names")
  
  # check it does not work when baselearner names are not the same
  names(predictor_list) <- c("var1", "var2", "var3")
  meta_model <- meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  )
  base_outputs <- data.table::as.data.table(df)
  base_outputs_stdt <- list("stdt" = base_outputs, 
                            "crs_dt" = "EPSG:4326") 
  class(base_outputs_stdt) <- c("list", "stdt")
  expect_error(meta_learner_predict(meta_model, base_outputs_stdt), 
               "Error: baselearners list incomplete or with wrong names")
  
})
