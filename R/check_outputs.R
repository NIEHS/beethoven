## A suite of functions to check output data
## Last edited 10/05/2023
## Insang Song

#' Check output locations are in the reference spatial domain of the mainland US
#'
#' @param model_output sf/sftime object of the model output
#' @param spatial_domain sf/sftime object of spatial domain
#' @return A logical vector of length nrow(model_output)
#' @author Insang Song
#' @export
check_output_locations_are_valid <- function(
  model_output,
  spatial_domain
) {
  if (is.na(sf::st_crs(model_output)) || is.na(sf::st_crs(spatial_domain))) {
    stop("All inputs should have appropriate CRS.\n")
  }
  # check if two inputs have the same crs, 
  # then transform spatial domain if the two crs are different
  if (!identical(sf::st_crs(model_output), sf::st_crs(spatial_domain))) {
    spatial_domain <- sf::st_transform(spatial_domain, sf::st_crs(model_output))
  }

  model_output <- sf::st_geometry(model_output)
  # evaluate if the model output is within the spatial domain
  # sparse argument chooses if return will be a list (TRUE) or a matrix (FALSE)
  checked <- as.vector(sf::st_within(model_output, spatial_domain, sparse = FALSE))
  return(checked)
}



#' Check if the output prediction mean values are in the valid range
#' 
#' @param model_output sf/sftime object of model output
#' @param model_mean_name character(1). the name of layer where mean values are stored
#' @param observation a data.frame with observations
#' @param observation_mean_name character(1). field name of observations in observation object.
#' @param tolerance_factor numeric(1). denominator(min) multiplier(max)
#' @return Logical value indicating the mean values are within the range or not.
#' @author Insang Song
#' @export
check_means_are_valid <- function(
  model_output,
  model_mean_name = "prediction_mean",
  observation,
  observation_mean_name = "value",
  tolerance_factor = 3
) {
  # clean observation values
  obs_values <- observation[[observation_mean_name]]
  obs_min <- min(obs_values)
  obs_max <- max(obs_values)

  threshold_lower <- obs_min * (1 / tolerance_factor)
  threshold_upper <- obs_max * tolerance_factor

  # clean output
  model_output <- model_output[[model_mean_name]]
  # "flatten" the mean layer
  vec_output <- unlist(model_output)
  # undetermined: whether or not NA values are allowed in the model output
  min_vec_output <- min(vec_output)
  max_vec_output <- max(vec_output)
  # evaluate if the output means are within the bounds
  checked <- (min_vec_output >= threshold_lower && max_vec_output <= threshold_upper)
  return(checked)
}



#' Check if the output is with the valid coordinate reference system
#' 
#' @param model_output sf/sftime object of model output.
#' @param crs_list a character/integer vector of acceptable CRS. Default is c("EPSG:4326", "EPSG:5070")
#' @return A logical value indicating the model is compliant to one of elements in crs_list.
#' @author Insang Song
#' @export
check_crs_is_valid <- function(
  model_output,
  crs_list = c("EPSG:4326", "EPSG:5070")
) {
  crs_output <- sf::st_crs(model_output)
  checked <- sapply(crs_list, function(x) crs_output == sf::st_crs(x))
  checked <- any(checked)
  return(checked)
}




#' Check if the output covariates are complete (TODO)
#' 
#' @param model_output sf/sftime object of model output.
#' @param fields_to_check character(varying). Field names where completeness will be checked.
#' @param report_fields_na logical(1). If any fields have NA values, report the field names which gave the errors. Default is FALSE.
#' @return A logical (when report_fields_na is FALSE; TRUE means there are NA values at least one data value) or a list (when report_fields_na is TRUE) object. A list includes the list of fields that include NAs.
#' @export
#' @author Insang Song
check_data_completeness <- function(
  model_output,
  fields_to_check,
  report_fields_na = FALSE
) {
  if (!any(is(model_output, "sf"), is(model_output, "sftime"))) {
    stop("Input should be sf/sftime object")
  }
  # 1. subset fields 
  model_output_sub <- model_output[,fields_to_check]
  model_output_sub <- sf::st_drop_geometry(model_output_sub)

  # 2. na evaluation
  model_output_field_nas <- sapply(model_output_sub, 
    function(cl) { any(is.na(cl)) })

  # 3. return results
  result_eval <- any(model_output_field_nas)
  result_field_names <- names(model_output_sub)[model_output_field_nas]

  if (report_fields_na) {
    cat(paste(
      "Some fields in the model_output are incomplete. Please check:\n",
        paste0(result_field_names, collapse = ", "),
        "\n"))
    result_eval <- list(
      NA_present = result_eval,
      NA_fields = result_field_names
    )
  }
  return(result_eval)
}
