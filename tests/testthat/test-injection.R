testthat::test_that("reduce_merge joins all relevant data.frames", {
  withr::local_package("dplyr")
  withr::local_package("rlang")

  # Create a list of data.frames
  data_frames <- list(
    data.frame(x = 1:3, y = 4:6),
    data.frame(x = 4:6, y = 7:9)
  )

  # Create example data tables
  dt1 <- data.frame(a = 1:3, b = 4:6)
  dt2 <- data.frame(a = 2:4, c = 7:9)
  dt3 <- data.frame(a = 3:5, d = 10:12)

  # Merge the data tables
  # by = NULL will automatically detect the common column names
  testthat::expect_no_error(
    reduce_merge(list(dt1, dt2, dt3), by = NULL)
  )
  rmerged <- reduce_merge(list(dt1, dt2, dt3), by = "a")
  testthat::expect_s3_class(rmerged, "data.frame")
  testthat::expect_equal(ncol(rmerged), 4)
})


testthat::test_that("inject_match only passes the matching arguments", {
  withr::local_package("terra")
  withr::local_package("rlang")

  # Generate a point spatial vector object
  point_data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  point_spatial_vector <- terra::vect(point_data, geom = c("x", "y"))

  # Generate a polygon spatial vector object
  polygon_data <-
    data.frame(
      id = c(1, 2, 3),
      area = c(10, 20, 30),
      geometry =
      c("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))",
        "POLYGON ((1 1, 1 2, 2 2, 2 1, 1 1))",
        "POLYGON ((2 2, 2 3, 3 3, 3 2, 2 2))"
      )
    )
  polygon_spatial_vector <- terra::vect(polygon_data, geom = "geometry")

  # define a list with invalid arguments
  push_args <- list(
    x = point_spatial_vector,
    y = polygon_spatial_vector,
    z = "invalid"
  )

  testthat::expect_no_error(
    imatched <- inject_match(
      f = terra::intersect,
      args = push_args
    )
  )
  testthat::expect_s4_class(imatched, "SpatVector")

})
