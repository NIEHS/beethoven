#' @author Insang Song
#' @description
#' unit testing for the output has a valid CRS
#'
#' list of valid CRS: EPSG:5070, EPSG:4326
#'
testthat::test_that("Output CRS is valid", {
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  # 1. read netcdf output file
  path_results <- "../testdata/test_nc_output.nc"
  model_results <- sf::read_sf(path_results)

  # 2. main evaluation
  iscrsvalid <- check_crs_is_valid(model_results)
  testthat::expect_equal(iscrsvalid, TRUE)

  # terra case:
  # due to WKT2/WKT version discrepancy and file export
  # behavior in GDAL, the system-default EPSG:4326 (ensemble)
  # is different from what we get at importing (EPSG:4326 plain)
  # In this test, we assume that the crs is EPSG:4326,
  # then "EPSG:4326" is explicitly declared in crs argument in vect.
  # TODO: follow issues in WKT2/CRS handling in different file formats
  # Will consider "OGC:CRS84" (non-3D GCS)
  model_results_t <- terra::vect(path_results, crs = "EPSG:4326")
  iscrsvalid_t <- check_crs_is_valid(model_results_t)
  testthat::expect_equal(iscrsvalid_t, TRUE)

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
  ismeanvalid <-
    check_means_are_valid(
      model_results,
      observation = observations,
      observation_mean_name = "Arithmetic.Mean"
    )
  testthat::expect_equal(ismeanvalid, TRUE)
})

#' @author Kyle Messier, Insang Song, Eva Marques
#' @description
#' unit testing for the model output has positive variances
testthat::test_that("Predicted variances are positive", {
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))
  # 1. expect to pass when ALL variances are positive
  prediction_variance <- list(NA, NA)
  names(prediction_variance) <- c("prediction_variance", "crs_dt")
  prediction_variance[[1]] <- stats::rgamma(100, 8, 0.3)
  prediction_variance[[2]] <- "EPSG:4326"
  isvarpositive <-
    check_variances_are_valid(
      prediction_variance
    )
  testthat::expect_equal(isvarpositive, TRUE)
  # 2. expect to fail when ANY variances are negative
  prediction_variance[[1]][1] <- -1
  isvarpositive <-
    check_variances_are_valid(
      prediction_variance
    )
  testthat::expect_equal(isvarpositive, FALSE)
})

#' @author Insang Song
#' @description
#' unit testing for the model output is inside the mainland US
testthat::test_that("Output locations are in the mainland US", {
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  # 1. read netcdf output file and mainland US polygons
  path_mainland <-
    testthat::test_path("..", "testdata", "US-mainland-boundary.gpkg")
  path_results <-
    testthat::test_path("..", "testdata", "test_nc_output.nc")
  model_results <- sf::read_sf(path_results)
  mainland <- sf::read_sf(path_mainland)

  # 2. main evaluation
  iswithin <- check_output_locs_are_valid(model_results, mainland)
  # we expect all elements in the vector are TRUE
  testthat::expect_equal(any(!iswithin), FALSE)

  model_res_na <- model_results
  sf::st_crs(model_res_na) <- NA
  testthat::expect_error(
    check_output_locs_are_valid(model_res_na, mainland)
  )
})


#' @author Insang Song
#' @description
#' unit testing for no NAs in covariates
#'
testthat::test_that("No covariates have NAs", {
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_results <- "../testdata/test_nc_output.nc"
  path_mainland <- "../testdata/US-mainland-boundary.gpkg"

  model_results <- sf::read_sf(path_results)
  mainland <- sf::read_sf(path_mainland)

  # 2. main evaluation
  iswithin <- check_output_locs_are_valid(model_results, mainland)
  # we expect all elements in the vector are TRUE
  testthat::expect_equal(any(!iswithin), FALSE)
})



#' @author Kyle P Messier, Insang Song
#' @description
#' unit testing for MERRA2 covariates
#' test that the covariate calculation does not have unexpected NA
#'
#' We switched to the generic check_data_completeness.
#' This test is subject to change
#'
testthat::test_that("MERRA2 covariate is not NA", {
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  path_results <- "../testdata/test_nc_output.nc"
  model_results <- sf::read_sf(path_results)

  merra_name <- "merra_tavg_sim"

  merra2_check <- check_data_completeness(model_results, merra_name)
  testthat::expect_equal(merra2_check, FALSE)

  merra2_check_true <-
    check_data_completeness(
                            model_results,
                            merra_name,
                            TRUE)
  testthat::expect_equal(methods::is(merra2_check_true, "list"), TRUE)

  merra2_null <- character(0)
  testthat::expect_error({
    check_data_completeness(
                            merra2_null,
                            merra_name,
                            TRUE)
  })
})
