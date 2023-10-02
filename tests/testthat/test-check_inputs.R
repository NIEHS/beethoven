#' @author Kyle P Messier, Insang Song
#' @description
#' unit testing for checking raster bounds
test_that("raster is bounded within project domain", {
    # read and get US with 300km buffer bounding box  
    example_raster = stars::read_stars("../testdata/merra2_tavg2_US_mainland_20220820_daily.tif")
    reference_polygon = sf::st_read("../testdata/US-mainland-boundary.gpkg")

    iswithin = check_input_raster_in_extent(example_raster, reference_polygon)

    # Add checks for raster data is at least a subset of US+buffer bbox
    expect_equal(iswithin, 1)
})
