#' @author Kyle P Messier
#' @description
#' unit testing that the dependent variable is read and converted to the expected sf/sftime class
#' 
#' 
#' 
#'

# Define a test case
test_that("generic_base_learner returns valid predictions", {

  # Load the function you want to test
#source("R/generic_pipeline_example.R")
  
  aqs.sftime <- sf::st_read("../testdata/aqs-test-data.gpkg") |>
    sftime::st_as_sftime()
  
  # Test data
  response <- aqs.sftime$Arithmetic.Mean
  covariate <- rnorm(length(response))
  obs_locs <- runif(length(response))
  model_attr <- NA
  
  # Call the function
  learner <- generic_base_learner(response, covariate, obs_locs, model_attr)
  
  # Check if the learner is a function
  expect_type(learner, "closure")

  
  # Add more specific expectations based on your requirements
  # For example, check if predictions have the correct length or values.
  
})

