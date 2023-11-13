test_that("convert_stobj_to_stdt works well", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("tidyr")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  lon <- seq(-112, -101, length.out = 5) # create lon sequence
  lat <- seq(33.5, 40.9, length.out = 5) # create lat sequence
  df <- expand.grid("lon" = lon, "lat" = lat) # expand to regular grid
  df <- rbind(df, df)
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  df$var1 <- 1:50
  df$var2 <- 51:100

  # test that is fails if stobj is not from an accepted st class
  expect_error(
    convert_stobj_to_stdt(stobj = "I love cheese"),
    "Error: stobj class not accepted"
  )


  # test with dataframe / datatable objects
  # 1) it should work
  expect_no_error(convert_stobj_to_stdt(df))
  expect_no_error(convert_stobj_to_stdt(data.table::as.data.table(df)))
  expect_true(is.na(convert_stobj_to_stdt(df)$crs_stdt))
  expect_equal(class(convert_stobj_to_stdt(df)$stdt)[[1]], "data.table")
  expect_false(any(!(c("lon", "lat", "time") %in%
    colnames(convert_stobj_to_stdt(df)$stdt))))
  # 2) it should fail because time column is missing
  df$time <- NULL
  expect_error(
    convert_stobj_to_stdt(df),
    "Error: stobj does not contain lon, lat, time columns"
  )

  # test with sf / sftime objects
  # 1) it should work
  df$time <- c(rep("2023-11-02", 25), rep("2023-11-03", 25))
  stobj <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = "EPSG:4326")
  expect_no_error(convert_stobj_to_stdt(stobj))
  stdt <- convert_stobj_to_stdt(stobj)$stdt
  crsdt <- convert_stobj_to_stdt(stobj)$crs_stdt
  expect_equal(class(stdt)[[1]], "data.table")
  expect_equal(class(crsdt), "character")
  expect_true(terra::same.crs(crsdt, "EPSG:4326"))
  expect_false(any(!(c("lon", "lat", "time") %in% colnames(stdt))))
  expect_equal(
    stdt[lon == -112 & lat == 35.35 & time == "2023-11-02", var1],
    6
  )
  expect_equal(
    stdt[lon == -112 & lat == 35.35 & time == "2023-11-02", var2],
    56
  )
  df$time <- as.Date(df$time)
  stobj <- sftime::st_as_sftime(df,
    coords = c("lon", "lat"),
    time_column_name = "time",
    crs = "EPSG:4326"
  )
  expect_no_error(convert_stobj_to_stdt(stobj))
  expect_equal(class(convert_stobj_to_stdt(stobj)$crs_stdt), "character")
  # 2) it should fail because time columns is misspelled
  stobj$time <- stobj$time
  stobj$time <- NULL
  expect_error(
    convert_stobj_to_stdt(stobj),
    "Error: stobj does not contain geometry and time columns"
  )

  # test with SpatVector objects
  # 1) it should work
  stobj <- terra::vect(df, geom = c("lon", "lat"), crs = "EPSG:4326", keepgeom = FALSE)
  expect_no_error(convert_stobj_to_stdt(stobj))
  stdt <- convert_stobj_to_stdt(stobj)$stdt
  expect_equal(class(stdt)[[1]], "data.table")
  expect_equal(class(convert_stobj_to_stdt(stobj)$crs_stdt), "character")
  expect_true(terra::same.crs(convert_stobj_to_stdt(stobj)$crs_stdt, "EPSG:4326"))
  expect_false(any(!(c("lon", "lat", "time") %in% colnames(stdt))))
  expect_equal(
    stdt[lon == -112 & lat == 35.35 & time == "2023-11-02", var1],
    6
  )
  expect_equal(
    stdt[lon == -112 & lat == 35.35 & time == "2023-11-02", var2],
    56
  )

  # test with SpatRastDataset created from 2 SpatRast (i.e. 2 variables)
  # with 3 layers (i.e. 3 timestamps)
  # 1) it should work
  var1 <- terra::rast(extent = c(-112, -101, 33.5, 40.9), 
               ncol = 5, 
               nrow = 5, 
               crs = "epsg:4326")
  terra::values(var1) <- seq(-5, 19)
  terra::add(var1) <- c(var1**2, var1**3)
  var1 <- rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "epsg:4326"
  )
  values(var1) <- seq(-5, 19)
  add(var1) <- c(var1**2, var1**3)
  names(var1) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  var2 <- rast(
    extent = c(-112, -101, 33.5, 40.9),
    ncol = 5,
    nrow = 5,
    crs = "epsg:4326"
  )
  values(var2) <- seq(-15, 9)
  add(var2) <- c(var2**2, var2**3)
  names(var2) <- c("2023-11-01", "2023-11-02", "2023-11-03")
  stobj <- terra::sds(var1, var2)
  names(stobj) <- c("var1", "var2")
  expect_no_error(convert_stobj_to_stdt(stobj))
  stdt_converted <- convert_stobj_to_stdt(stobj)
  expect_equal(class(stdt_converted$stdt)[[1]], "data.table")
  expect_equal(class(stdt_converted$crs_stdt), "character")
  expect_true(terra::same.crs(stdt_converted$crs_stdt, "EPSG:4326"))
  expect_false(any(!(c("lon", "lat", "time") %in% colnames(stdt_converted$stdt))))
  expect_equal(stdt_converted$stdt[lon == -106.5 & lat == stdt_converted$stdt$lat[37] & time == "2023-11-02", var1],
               49)
  expect_equal(stdt_converted$stdt[lon == -106.5 & lat == stdt_converted$stdt$lat[37] & time == "2023-11-02", var2], 
               9)
})
