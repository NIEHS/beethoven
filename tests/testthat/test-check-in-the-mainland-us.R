#' @author Insang Song
#' @description
#' unit testing for the model output is inside the mainland US
#' 
#' 
#'
test_that("Output locations are in the mainland US", {
  library(stars)
  library(sf)
  library(dplyr)

  # 1. read netcdf output file and mainland US polygons
  path_mainland = "./tests/testdata/US-mainland-boundary.gpkg"
    # dummy path. correct path should be added to pass
  path_results = "./output/model_output.nc"
  model_results <- stars::read_stars(path_results)
  mainland <- sf::read_sf(path_mainland)

  # 2. transform stars to sf
  model_results <- sf::st_as_sf(model_results)

  # 3. main function
  check_output_locations_are_valid <- function(
    model_output = model_results,
    spatial_domain = mainland
  ) {
    if (is.na(st_crs(model_output)) || is.na(st_crs(spatial_domain))) {
        stop("All inputs should have appropriate CRS.\n")
    }
    # check if two inputs have the same crs, 
    # then transform spatial domain if the two crs are different
    if (!identical(st_crs(model_output), st_crs(spatial_domain))) {
        spatial_domain = st_transform(spatial_domain, st_crs(model_output))
    }
    
    model_output <- sf::st_geometry(model_output)
    # evaluate if the model output is within the spatial domain
    # sparse argument chooses if return will be a list (TRUE) or a matrix (FALSE)
    checked <- as.vector(st_within(model_output, spatial_domain, sparse = FALSE))
    return(checked)
  }

  iswithin = check_output_locations_are_valid()

  # we expect all elements in the vector are TRUE
  expect_equal(any(!iswithin), FALSE)
})
