test_that("Check extract_nlcd_ratio works", {
  
  point_wa <- cbind(lon = -119.88, lat = 47.709, dem = 3) # washington state
  point_nc <- cbind(lon = -78.836, lat = 35.893, dem = 10) # north carolina
  point_ak <- cbind(lon = -155.997, lat = 69.3884, dem = 100) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, dem = 15) # france 
  eg_data <- rbind(point_wa, point_nc, point_ak, point_fr) %>%
    as.data.frame() %>%
    vect(., crs="EPSG:4326")
  
  # CHECK INPUT (error message)
  # -- buf_radius is numeric
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data, buf_radius = "1000"),
    "buf_radius is not a numeric.")
  # -- buf_radius has likely value
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data, buf_radius = -3),
    "buf_radius has not a likely value.")
  # -- year is numeric
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data, year = "2019"),
    "year is not a numeric.")
  # -- year has likely value
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data, year = 20192),
    "NLCD data not available for this year.")
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data, year = 2020),
    "NLCD data not available for this year.")
  # -- data_vect is a SpatVector
  expect_error(calc_nlcd_ratio(data_vect = 12), 
               "data_vect is not a terra::SpatVector")
  
  # CHECK OUTPUT
  year <- 2021
  buf_radius <- 100
  expect_no_error(calc_nlcd_ratio(data_vect = eg_data,
                                       year = year, 
                                       buf_radius = buf_radius))
  output <- calc_nlcd_ratio(data_vect = eg_data,
                                 year = year, 
                                 buf_radius = buf_radius)
  # -- returns a SpatVector 
  expect_equal(class(output)[1], "SpatVector")
  # -- crs is the same than input 
  expect_true(terra::same.crs(eg_data, output))
  # -- out-of-mainland-US points removed (France and Alaska)
  expect_equal(nrow(output), 2)
  # -- initial names are still in the output SpatVector
  expect_true(all(names(eg_data) %in% names(output)))
  # -- check the value of some of the points in the US 
  expect_equal(output$frac_LID_2021_100m[1], 0.1770684, tolerance = 1e-7)
  expect_equal(output$frac_SHB_2021_100m[2], 0.2429427, tolerance = 1e-7)
  # -- class fraction rows should sum to 1 
  expect_equal(rowSums(as.data.frame(output[, 2:ncol(output)])), 
               rep(1, 2),
               tolerance = 1e-7 
               )
})
