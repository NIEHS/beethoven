withr::local_package("spData")

test_that("Check extract_nlcd_ratio works", {
  point_us1 <- cbind(lon = -114.7, lat = 38.9, dem = 40)
  point_us2 <- cbind(lon = -114, lat = 39, dem = 15)
  point_ak <- cbind(lon = -155.997, lat = 69.3884, dem = 100) # alaska
  point_fr <- cbind(lon = 2.957, lat = 43.976, dem = 15) # france
  eg_data <- rbind(point_us1, point_us2, point_ak, point_fr) %>%
    as.data.frame() %>%
    vect(., crs = "EPSG:4326")
  getwd()
  path_testdata <- "../testdata/"
  
  # CHECK INPUT (error message)
  # -- buf_radius is numeric
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data,
                    buf_radius = "1000",
                    nlcd_path = path_testdata),
    "buf_radius is not a numeric."
  )
  # -- buf_radius has likely value
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data,
                    buf_radius = -3,
                    nlcd_path = path_testdata),
    "buf_radius has not a likely value."
  )
  # -- year is numeric
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data,
                    year = "2019",
                    nlcd_path = path_testdata),
    "year is not a numeric."
  )
  # -- year has likely value
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data,
                    year = 20192,
                    nlcd_path = path_testdata),
    "NLCD data not available for this year."
  )
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data,
                    year = 2020,
                    nlcd_path = path_testdata),
    "NLCD data not available for this year."
  )
  # -- data_vect is a SpatVector
  expect_error(
    calc_nlcd_ratio(data_vect = 12,
                    nlcd_path = path_testdata),
    "data_vect is not a terra::SpatVector."
  )
  # -- nlcd_path is not a character
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data,
                    nlcd_path = 2),
    "nlcd_path is not a character."
  )
  # -- nlcd_path does not exist
  nice_sentence <- "That's one small step for a man, a giant leap for mankind."
  expect_error(
    calc_nlcd_ratio(data_vect = eg_data,
                    nlcd_path = nice_sentence),
    "nlcd_path does not exist."
  )

  # CHECK OUTPUT
  year <- 2021
  buf_radius <- 3000
  expect_no_error(calc_nlcd_ratio(
    data_vect = eg_data,
    year = year,
    buf_radius = buf_radius,
    nlcd_path = path_testdata
  ))
  output <- calc_nlcd_ratio(
    data_vect = eg_data,
    year = year,
    buf_radius = buf_radius,
    nlcd_path = path_testdata
  )
  # -- returns a SpatVector
  expect_equal(class(output)[1], "SpatVector")
  # -- crs is the same than input
  expect_true(terra::same.crs(eg_data, output))
  # -- out-of-mainland-US points removed (France and Alaska)
  expect_equal(nrow(output), 2)
  # -- initial names are still in the output SpatVector
  expect_true(all(names(eg_data) %in% names(output)))
  # -- check the value of some of the points in the US
  expect_equal(output$frac_EFO_2021_3000m[1], 0.7940682, tolerance = 1e-7)
  expect_equal(output$frac_SHB_2021_3000m[2], 0.9987249, tolerance = 1e-7)
  # -- class fraction rows should sum to 1
  expect_equal(rowSums(as.data.frame(output[, 2:ncol(output)])),
    rep(1, 2),
    tolerance = 1e-7
  )
})
