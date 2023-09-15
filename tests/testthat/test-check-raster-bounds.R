#' @author Kyle P Messier, Insang Song
#' @description
#' unit testing for checking raster bounds
#' 
#' 
#'
test_that("raster is bounded within project domain", {

  # read and get US with 300km buffer bounding box  
  US <- sf::st_read("../testdata/US-mainland-boundary.gpkg") |> 
    sf::st_buffer(3e5) |> 
    sf::st_bbox()
  
  
  # Add checks for raster data is at least a subset of US+buffer bbox
  expect_equal()
})
