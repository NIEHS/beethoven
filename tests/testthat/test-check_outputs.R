#' @author Insang Song
#' @description
#' unit testing for the output has a valid CRS
#'
#' list of valid CRS: EPSG:5070, EPSG:4ß326
#'
testthat::test_that("Output CRS is valid", {
  skip_on_ci()
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  # 1. read netcdf output file
  path_results <- "../testdata/test_nc_output.nc"
  model_results <- sf::read_sf(path_results)

  # 2. main evaluation
  iscrsvalid <- check_crs_is_valid(model_results)
  testthat::expect_equal(iscrsvalid, TRUE)

  model_results_2163 <- sf::st_transform(model_results, "EPSG:2163")
  crsnotvalid <- check_crs_is_valid(model_results_2163)
  testthat::expect_equal(crsnotvalid, FALSE)
})


#' @author Insang Song, Eva Marques
#' @description
#' unit testing for the model output has reasonable means
#' reasonable means should be ranged between min(obs)*(1/3) to max(obs)*3
#' *** the definition of "valid range" is still in discussion.
#'      the function and test will be subject to changes. ***
testthat::test_that("Predicted means are within a proper range", {
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  # 1. read netcdf output file and observation file
  path_results <- "../testdata/test_nc_output.nc"
  path_observation <- "../testdata/daily_88101_2018-2022.rds"
  model_results <- sf::read_sf(path_results)
  observations <- base::readRDS(path_observation)
  model_results$prediction_mean <- stats::rgamma(nrow(model_results), 8, 0.3)

  # 2. main evaluation
  ismeanvalid <- check_means_are_valid(model_results, observation = observations, observation_mean_name = "Arithmetic.Mean")
  testthat::expect_equal(ismeanvalid, TRUE)
})



#' @author Insang Song
#' @description
#' unit testing for the model output is inside the mainland US
#' 
testthat::test_that("Output locations are in the mainland US", {
    skip_on_ci()
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  # 1. read netcdf output file and mainland US polygons
  path_mainland <- "../testdata/US-mainland-boundary.gpkg"
  path_results <- "../testdata/test_nc_output.nc"
  model_results <- sf::read_sf(path_results)
  mainland <- sf::read_sf(path_mainland)

  # 2. main evaluation
  iswithin <- check_output_locations_are_valid(model_results, mainland)
  # we expect all elements in the vector are TRUE
  testthat::expect_equal(any(!iswithin), FALSE)
})


#' @author Insang Song
#' @description
#' unit testing for no NAs in covariates
#' 
testthat::test_that("No covariates have NAs", {
    skip_on_ci()
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_results <- "../testdata/test_nc_output.nc"
  path_mainland <- "../testdata/US-mainland-boundary.gpkg"

  model_results <- sf::read_sf(path_results)
  mainland <- sf::read_sf(path_mainland)

  # 2. main evaluation
  iswithin <- check_output_locations_are_valid(model_results, mainland)
  # we expect all elements in the vector are TRUE
  testthat::expect_equal(any(!iswithin), FALSE)
})



#' @author Kyle P Messier, Insang Song
#' @description
#' unit testing for MERRA2 covariates
#' test that the covariate calculation does not have unexpected NA
#' 
#' We switched to the generic check_data_completeness. This test is subject to change
#'
testthat::test_that("MERRA2 covariate is not NA", {
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_results <- "../testdata/test_nc_output.nc"
  model_results <- sf::read_sf(path_results)
    
  MERRA_name <- "merra_tavg_sim"

  # extract_with_buffer.flat <- function(
  #         points, surf, radius, id, qsegs, func = mean, kernel = NULL, bandwidth = NULL
  #     ) {
  #     # generate buffers
  #     bufs <- terra::buffer(points, width = radius, quadsegs = qsegs)
  #     # crop raster
  #     bufs_extent <- terra::ext(bufs)
  #     surf_cropped <- terra::crop(surf, bufs_extent)
  #     name_surf_val <- names(surf)
  #     # extract raster values
  #     surf_at_bufs <- terra::extract(surf_cropped, bufs)
  #     surf_at_bufs_summary <-
  #         surf_at_bufs |>
  #             group_by(ID) |>
  #             summarize(across(all_of(name_surf_val), ~mean(.x, na.rm = TRUE))) |> 
  #             ungroup()
  #     return(surf_at_bufs_summary)
  # }
  #   MERRA2.var <- extract_with_buffer.flat(aqs.sftime, merra, 2e4L, "ID.Code", 90L)
  MERRA2_check <- check_data_completeness(model_results, MERRA_name)
  testthat::expect_equal(MERRA2_check, FALSE)

  MERRA2_check_T <- check_data_completeness(model_results, MERRA_name, TRUE)
  testthat::expect_equal(methods::is(MERRA2_check_T, "list"), TRUE)

  MERRA2_null <- character(0)
  testthat::expect_error(check_data_completeness(MERRA2_null, MERRA_name, TRUE))
})
