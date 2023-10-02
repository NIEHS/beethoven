#' @author Insang Song
#' @description
#' unit testing for the output has a valid CRS
#' list of valid CRS: EPSG:5070, EPSG:4326
#' 
#'
test_that("Output CRS is valid", {
    withr::local_package("stars")
    withr::local_package("sf")

    # 1. read netcdf output file
    path_results = "../../output/model_output.nc"
    model_results <- stars::read_stars(path_results)

    # 2. main evaluation
    iscrsvalid = check_crs_is_valid(model_results)

    expect_equal(iscrsvalid, TRUE)

    model_results_2163 = st_transform(model_results, "EPSG:2163")
    crsnotvalid = check_crs_is_valid(model_results_2163)

    expect_equal(crsnotvalid, FALSE)

})


#' @author Insang Song, Eva Marques
#' @description
#' unit testing for the model output has reasonable means
#' reasonable means should be ranged between min(obs)*(1/3) to max(obs)*3
#' *** the definition of "valid range" is still in discussion.
#'      the function and test will be subject to changes. ***
test_that("Predicted means are within a proper range", {
    withr::local_package("stars")
    withr::local_package("sf")
    withr::local_package("dplyr")

    # 1. read netcdf output file and observation file
    # dummy path. correct path should be added to pass
    path_results = "../../output/model_output.nc"
    path_observation = "./input/aqs_pm25_cleaned_2018_2022.csv"
    model_results <- stars::read_stars(path_results)
    observations <- read.csv(path_observation)

    # 2. transform stars to sf
    model_results <- sf::st_as_sf(model_results)

    # 3. main evaluation
    ismeanvalid = check_means_are_valid(model_results, observation = observations)

    expect_equal(ismeanvalid, TRUE)
})



#' @author Insang Song
#' @description
#' unit testing for the model output is inside the mainland US
#' 
test_that("Output locations are in the mainland US", {
    withr::local_package("stars")
    withr::local_package("sf")
    withr::local_package("dplyr")

    # 1. read netcdf output file and mainland US polygons
    path_mainland = "../testdata/US-mainland-boundary.gpkg"
    # dummy path. correct path should be added to pass
    path_results = "../../output/model_output.nc"
    model_results <- stars::read_stars(path_results)
    mainland <- sf::read_sf(path_mainland)

    # 2. transform stars to sf
    model_results <- sf::st_as_sf(model_results)

    # 3. main evaluation
    iswithin = check_output_locations_are_valid(model_results, mainland)

    # we expect all elements in the vector are TRUE
    expect_equal(any(!iswithin), FALSE)
})




#' @author Insang Song
#' @description
#' unit testing for no NAs in covariates
#' 
test_that("No covariates have NAs", {
    withr::local_package("stars")
    withr::local_package("sf")

    # dummy path. correct path should be added to pass
    path_results = "../../output/model_output.nc"
    model_results <- stars::read_stars(path_results)
    mainland <- sf::read_sf(path_mainland)

    # 2. transform stars to sf
    model_results <- sf::st_as_sf(model_results)

    # 3. main evaluation
    iswithin = check_output_locations_are_valid(model_results, mainland)

    # we expect all elements in the vector are TRUE
    expect_equal(any(!iswithin), FALSE)
})



#' @author Kyle P Messier, Insang Song
#' @description
#' unit testing for MERRA2 covariates
#' test that the covariate calculation does not have unexpected NA
#' 
#' We switched to the generic check_data_completeness. This test is subject to change
#'
test_that("MERRA2 covariate is not NA", {
  skip_on_ci()
  withr::local_package(terra)
  withr::local_package(dplyr)
  
  aqs.sftime <- terra::vect("../testdata/aqs-test-data.gpkg")
  merra = terra::rast("../testdata/merra2_tavg2_US_mainland_20220820_daily.tif")

  extract_with_buffer.flat <- function(
          points, surf, radius, id, qsegs, func = mean, kernel = NULL, bandwidth = NULL
      ) {
      # generate buffers
      bufs = terra::buffer(points, width = radius, quadsegs = qsegs)
      # crop raster
      bufs_extent = terra::ext(bufs)
      surf_cropped = terra::crop(surf, bufs_extent)
      name_surf_val = names(surf)
      # extract raster values
      surf_at_bufs = terra::extract(surf_cropped, bufs)
      surf_at_bufs_summary = 
          surf_at_bufs |> 
              group_by(ID) |> 
              summarize(across(all_of(name_surf_val), ~mean(.x, na.rm = TRUE))) |> 
              ungroup()
      return(surf_at_bufs_summary)
  }

  MERRA2.var <- extract_with_buffer.flat(aqs.sftime, merra, 2e4L, "ID.Code", 90L)

  expect_equal(any(is.na(as.vector(MERRA2.var))), FALSE)
})
