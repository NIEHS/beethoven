#' @author SETgroup
#' @description
#' @title meta learner unit test
#'
#'


test_that("the convert_stobj_to_stdt works well", {

  # create space-time SpatVector
  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <-  seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  df$var1 <- 1:50
  df$var2 <- 51:100
  stobj <- vect(df, geom=c("lon", "lat"), crs="EPSG:4326", keepgeom=FALSE)
  
  # create SpatRastDataset from 2 SpatRast (i.e. 2 variables) 
  # with 3 layers (i.e. 3 timestamps)
  var1 <- rast(extent = c(-112, -101, 33.5, 40.9), ncol = 5, nrow = 5, crs = "epsg:4326")
  values(var1) <- seq(-5, 19)
  add(var1) <- c(var1**2, var1**3)
  names(var1) <- c("2023-11-01", "2023-11-02", "2023-11-03")

  var2 <- rast(extent = c(-112, -101, 33.5, 40.9), ncol = 5, nrow = 5, crs = "epsg:4326")
  values(var2) <- seq(-15, 9)
  add(var2) <- c(var2**2, var2**3)
  names(var2) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  
  stobj <- terra::sds(var1, var2)
  names(stobj) <- c("var1", "var2")
  
  expect_no_error(convert_stobj_to_stdt(stobj))
  
  
})

test_that("the meta learner fitting abides", {
  skip()
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
  predictor_list <- list(
    runif(length(response), min = 1, max = 10),
    rnorm(length(response)),
    c(1, rep("I love Roquefort cheese", length(response) - 1))
  )
  expect_error(meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  ), "Error in meta_learner_fit:
         Some of base predictors are not numeric")
})







test_that("the meta learner prediction abides", {
  
  ### Test locations as sf
  
  response <- 3 + rnorm(100)
  kfolds <- sample(rep(1:5, length.out = length(response)))
  
  #Predictor list
  predictor_list <- list(
    runif(100, min = 1, max = 10),
    rnorm(100),
    pi + rnorm(100)
  )
  names(predictor_list) <- c("var1", "var2", "var3")
  
  # Fit learner
  meta_model <- meta_learner_fit(
    base_predictor_list = predictor_list,
    kfolds = kfolds, y = response
  )
  
  # Convert predictors to sf - requires multiple steps
  x_df <- as.data.frame(predictor_list) # convert to data frame
  lon <- seq(-112, -101, length.out = 10) # create lon sequence
  lat <-  seq(33.5, 40.9, length.out = 10) # create lat sequence
  lon_lat <- expand.grid(lon, lat) # expand to regular grid
  x_df$longitude <- lon_lat$Var1 # add lon/lat to dataframe
  x_df$latitude <- lon_lat$Var2
  cov_pred_sf <- sf::st_as_sf(x_df, coords = c("longitude", "latitude"))
  sf::st_crs(cov_pred_sf) <- sf::st_crs("EPSG:4326") # add coordinate ref sys
  
  
  # Get meta learner prediction
  model_output <- meta_learner_predict(meta_model,
                                       base_outputs = cov_pred_sf)
  # Testthat model output is an sf
  expect_true(class(model_output)[1] == "sf")
  
  ## Test locations as a SpatVector
  # Get meta learner prediction
  cov_SpatVect <- terra::vect(cov_pred_sf)
  model_output <- meta_learner_predict(meta_model,
                                       base_outputs = cov_SpatVect)
  expect_true(class(model_output)[1] == "SpatVector")
  
  # Test locations as a SpatRaster
  svnames <- names(cov_SpatVect)
  cov_SpatRast <- lapply(split(svnames, svnames),
                         function(x) {
                           terra::rasterize(cov_SpatVect, 
                                            terra::rast(cov_SpatVect), 
                                            field = x)}) |>
    Reduce(f = c, x = _)
  names(cov_SpatRast) <- svnames
  model_output <- meta_learner_predict(meta_model,
                                       base_outputs = cov_SpatRast)
  # Testthat model output is an SpatRaster when the input is SpatRaster
  expect_true(class(model_output)[1] == "SpatRaster")
  
})


