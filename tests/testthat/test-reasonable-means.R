#' @author Insang Song
#' @description
#' unit testing for the model output has reasonable means
#' reasonable means should be ranged between min(obs)*(1/3) to max(obs)*3
#' 
#'
test_that("Predicted means are within a proper range", {
  library(stars)
  library(sf)
  library(dplyr)

  # 1. read netcdf output file and observation file
    # dummy path. correct path should be added to pass
  path_results = "./output/model_output.nc"
  path_observation = "./input/aqs_pm25_cleaned_2018_2022.csv"
  model_results <- stars::read_stars(path_results)
  observations <- read.csv(path_observation)

  # 2. transform stars to sf
  model_results <- sf::st_as_sf(model_results)

  # 3. main function
  # model_output: model output object in stars
  # model_mean_name: character(1). the name of layer where mean values are stored
  # observation: a data.frame with observations
  # observation_mean_name: character(1). field name of observations in observation object.
  # tolerance_factor: numeric(1). denominator(min) multiplier(max)
  check_means_are_valid <- function(
    model_output = model_results,
    model_mean_name = "prediction_mean",
    observation = observations,
    observation_mean_name = "value",
    tolerance_factor = 3
  ) {
    # clean observation values
    obs_values <- observations[[observation_mean_name]]
    obs_min <- min(obs_values)
    obs_max <- max(obs_values)

    threshold_lower <- obs_min * (1 / tolerance_factor)
    threshold_upper <- obs_max * tolerance_factor

    # clean output
    model_output <- model_output[model_mean_name]
    # "flatten" the mean layer
    vec_output <- unlist(model_output)
    # undetermined: whether or not NA values are allowed in the model output
    min_vec_output <- min(vec_output)
    max_vec_output <- max(vec_output)
    
    # evaluate if the output means are within the bounds
    checked <- (min_vec_output >= threshold_lower && max_vec_output <= threshold_upper)
    return(checked)
  }

  ismeanvalid = check_means_are_valid()

  expect_equal(ismeanvalid, TRUE)
})
