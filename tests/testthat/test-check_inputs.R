#' @author Kyle P Messier, Insang Song
#' @description
#' unit testing for checking raster bounds
testthat::test_that("raster is bounded within project domain", {
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  # read and get US with 300km buffer bounding box
  example_raster <-
    stars::read_stars(
      "../testdata/merra2_tavg2_US_mainland_20220820_daily.tif"
    )
  reference_polygon <- sf::st_read("../testdata/US-mainland-boundary.gpkg")
  sf::st_crs(example_raster) <- sf::st_crs("EPSG:4326")

  suppressWarnings(
    iswithin <-
      check_input_raster_in_extent(example_raster, reference_polygon)
  )
  # Add checks for raster data is at least a subset of US+buffer bbox
  testthat::expect_equal(iswithin, 1)

  suppressWarnings(
    iswithin <-
      check_input_raster_in_extent(
                                   example_raster,
                                   terra::vect(reference_polygon))
  )
  testthat::expect_equal(iswithin, 1)

})
