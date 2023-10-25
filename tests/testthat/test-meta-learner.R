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
    runif(length(response), mi
          n = 1, max = 10),
    rnorm(length(response) - 1),
    pi + rnorm(length(response))
  )
  names(predictor_list) <- c("var1", "var2", "var3")

  expect_error(meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  ), "Error in meta_learner_fit: 
         Base predictors need to be the same length")
  
  
##### Tests on the meta learner prediction 
  
  ### Test locations as sf
  
  #Predictor list
  predictor_list <- list(
    runif(100, min = 1, max = 10),
    rnorm(100),
    pi + rnorm(100)
  )
  names(predictor_list) <- c("var1", "var2", "var3")
  
  #Convert predictors to sf - requires multiple steps
  x_df <- as.data.frame(predictor_list) # convert to data frame
  lon <- seq(-112, -101, length.out = 10) # create lon sequence
  lat <-  seq(33.5, 40.9, length.out = 10) # create lat sequence
  lon_lat <- expand.grid(lon, lat) # expand to regular grid 
  x_df$longitude <- lon_lat$Var1 # add lon/lat to dataframe
  x_df$latitude <- lon_lat$Var2
  cov_pred_sf <- sf::st_as_sf(x_df,coords = c("longitude","latitude")) #convert
  sf::st_crs(cov_pred_sf) <- sf::st_crs("EPSG:4326") # add coordinate ref sys
  
  # Get meta learner prediction
  model_output <- meta_learner_predict(meta_learner_output,
                                       cov_pred = cov_pred_sf)
  
  # the test is running on the object named "output"
  expect_type(model_output, "ncdf4")
  

})



