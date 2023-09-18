#' @author Kyle P Messier, Insang Song
#' @description
#' unit testing for checking raster bounds
#' 
#' 
#'
test_that("raster is bounded within project domain", {

  # read and get US with 300km buffer bounding box  
  us_bbox <- sf::st_read("/Volumes/songi2/projects/NRTAPModel/tests/testdata/US-mainland-boundary.gpkg") |> 
    sf::st_transform("EPSG:4326") |>
    sf::st_buffer(3e5) |> 
    sf::st_bbox() |>
    sf::st_as_sfc()
  
  point_target = sf::st_read("/Volumes/songi2/projects/NRTAPModel/tests/testdata/aqs-test-data.gpkg") |>
    sf::st_transform("EPSG:4326") |>
    sf::st_bbox() |>
    sf::st_as_sfc()

  iswithin = sf::st_covered_by(point_target, us_bbox)
  iswithin = length(iswithin[[1]])

    # Add checks for raster data is at least a subset of US+buffer bbox
  expect_equal(iswithin, 1)
})
