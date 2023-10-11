#' @author Ranadeep Daw and Eva Marques and Kyle Messier
#' @description
#' @title testing subsuite for the meta learner model
#' unit testing: is the model output netcdf?
#' 
#'

test_that("the meta learner output is netcdf", {
  withr::local_package("ncdf4")
  withr::local_package("RNetCDF")
  withr::local_package("BART")
  
  aqs_sftime <- sf::st_read("../testdata/aqs-test-data.gpkg") |>
    sftime::st_as_sftime()
  
  # Test data
  response <- aqs_sftime$Arithmetic.Mean
  covariate <- matrix(data = rnorm(length(response)*3),ncol = 3) 

  locs <- runif(length(response))
  
  # generate simple BART object
  bart.obj <- mc.wbart(covariate, response)
  
  # Call the meta learner prediction function
  model.output <- meta_learner_predict(bart.obj,locs)
  
  # the test is running on the object named "output"
  expect_is(model.output, 'ncdf4')
})