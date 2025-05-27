################################################################################
##### unit and integration tests for argument injection functions
##### main files: R/injection.R, R/calculate.R

################################################################################
##### feature_raw_download
testthat::test_that("feature_raw_download", {
  withr::local_package("rlang")
  withr::local_package("qs")

  # expect error for non-existent file path
  testthat::expect_error(
    beethoven::feature_raw_download(
      path = "does/not/exist.qs",
      dataset_name = "smoke"
    )
  )

  # expect error for non-qs file path
  frd_error_file <- file.path(tempdir(), "frd_error_file.csv")
  file.create(frd_error_file)
  testthat::expect_error(
    beethoven::feature_raw_download(
      path = frd_error_file,
      dataset_name = "smoke"
    )
  )

  # create temporary directory
  input_dir <- file.path(tempdir(), "input")
  dir.create(input_dir)

  # set download arguments
  testthat::expect_no_error(
    beethoven::set_args_download(
      char_period = c("2018-01-01", "2018-01-01"),
      char_input_dir = input_dir,
      nasa_earth_data_token = "token",
      export = TRUE,
      path_export = file.path(input_dir, "download_args.qs")
    )
  )
  testthat::expect_true(file.exists(file.path(input_dir, "download_args.qs")))

  # download hms features
  testthat::expect_no_error(
    beethoven::feature_raw_download(
      path = file.path(input_dir, "download_args.qs"),
      dataset_name = "hms"
    )
  )
  # expect 5 files (.txt file and .dbf, .prf, .shp and .shx)
  testthat::expect_equal(
    length(list.files(paste0(input_dir, "/HMS_Smoke"), recursive = TRUE)),
    5
  )
  unlink(paste0(input_dir, "/HMS_Smoke"), recursive = TRUE)

  # download hms and population features
  testthat::expect_no_error(
    beethoven::feature_raw_download(
      path = file.path(input_dir, "download_args.qs"),
      dataset_name = c("hms", "aqs")
    )
  )
  # expect 8 files (5 hms files plus aqs .txt, .csv, and download_args.qs file)
  testthat::expect_equal(
    length(list.files(input_dir, recursive = TRUE)),
    8
  )
  # expect aqs and hms folders
  testthat::expect_true(
    all(c("aqs", "HMS_Smoke") %in% list.files(input_dir))
  )

  # expect error when qs file does not contain proper arguments
  testthat::expect_error(
    beethoven::feature_raw_download(
      path = testthat::test_path(
        "..",
        "testdata",
        "injection",
        "error_args.qs"
      ),
      dataset = "hms"
    )
  )

  unlink(input_dir, recursive = TRUE)
})


################################################################################
##### set_target_years
testthat::test_that("set_target_years", {
  # expect no errors for integer years
  testthat::expect_no_error(
    years_integer <- beethoven::set_target_years(
      period = c(2018, 2022),
      available = c(2018, 2022)
    )
  )
  # expect 5 years
  testthat::expect_equal(length(years_integer), 5)

  # expect no errors for character years
  testthat::expect_no_error(
    years_char <- beethoven::set_target_years(
      period = c("2018", "2022"),
      available = c(2018, 2022)
    )
  )
  # expect 5 years
  testthat::expect_equal(length(years_char), 5)

  # expect no errors for dates
  testthat::expect_no_error(
    years_date <- beethoven::set_target_years(
      period = c("2018-01-01", "2022-12-31"),
      available = c(2018, 2022)
    )
  )
  # expect 5 years
  testthat::expect_equal(length(years_date), 5)
})


################################################################################
##### inject_calc
testthat::test_that("inject_calc (hms + nei)", {
  withr::local_package("rlang")

  # sample location
  loc <- data.frame(lon = -78.8277, lat = 35.95013, site_id = "A1")

  # expect no errors (hms + 1 radius; 1 thread)
  testthat::expect_no_error(
    hms_injected1 <- beethoven::inject_calculate(
      covariate = "hms",
      locs = loc,
      injection = list(
        path = testthat::test_path(
          "..",
          "testdata",
          "injection",
          "hms",
          "data_files"
        ),
        date = c("2018-01-01", "2018-01-01"),
        covariate = "hms"
      )
    )
  )
  # expect list
  testthat::expect_true(is.list(hms_injected1))
  # expect sub-list is a data.frame
  testthat::expect_s3_class(hms_injected1$`1`, "data.frame")
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(hms_injected1$`1`)))
  # expect 1 row and 6 columns
  testthat::expect_equal(dim(hms_injected1$`1`), c(1, 6))

  # expect no errors (hms + 2 radii; 2 threads)
  testthat::expect_no_error(
    hms_injected2 <- beethoven::inject_calculate(
      covariate = "hms",
      locs = loc,
      injection = list(
        path = testthat::test_path(
          "..",
          "testdata",
          "injection",
          "hms",
          "data_files"
        ),
        date = c("2018-01-01", "2018-01-01"),
        covariate = "hms",
        radius = c(0, 100)
      )
    )
  )
  # expect list
  testthat::expect_true(is.list(hms_injected2))
  # expect sub-list is a data.frame
  testthat::expect_s3_class(hms_injected2$`1`, "data.frame")
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(hms_injected2$`1`)))
  # expect 1 row and 11 columns
  testthat::expect_equal(dim(hms_injected2$`1`), c(1, 9))

  # sample location as sf
  loc_sf <- sf::st_as_sf(
    loc,
    coords = c("lon", "lat"),
    crs = "EPSG:4326"
  )

  testthat::skip_on_ci()
  # expect no errors (nei)
  testthat::expect_no_error(
    nei_injected <- beethoven::inject_calculate(
      covariate = "nei",
      locs = loc_sf,
      injection = list(
        covariate = "nei",
        path = testthat::test_path("..", "testdata", "injection", "nei"),
        domain = 2017,
        domain_name = "year"
      )
    )
  )
  # expect list
  testthat::expect_true(is.list(nei_injected))
  # expect sub-list is a data.frame
  testthat::expect_s3_class(nei_injected$`1`, "data.frame")
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(nei_injected$`1`)))
  # expect 1 row and 5 columns
  testthat::expect_equal(dim(nei_injected$`1`), c(1, 5))
})


################################################################################
##### inject_modis_par
testthat::test_that("inject_modis_par (MOD11A1)", {
  withr::local_package("rlang")

  # sample location
  loc <- sf::st_as_sf(
    data.frame(lon = -90, lat = 39.5, site_id = "site_id"),
    coords = c("lon", "lat"),
    crs = "EPSG:4326"
  )

  # identify MOD11A1 files
  mod11a1_files <- list.files(
    testthat::test_path("..", "testdata", "injection", "modis", "mod11a1"),
    full.names = TRUE
  )

  # expect no error with MOD11A1 files
  testthat::expect_warning(
    modis_injected_par <- beethoven::inject_modis_par(
      locs = loc,
      injection = list(
        from = mod11a1_files,
        subdataset = "(LST_)",
        name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
        preprocess = amadeus::process_modis_merge,
        radius = c(100)
      )
    )
  )
  # expect a data.frame
  testthat::expect_s3_class(modis_injected_par, "data.frame")
  # expect 1 row and 4 columns
  testthat::expect_equal(dim(modis_injected_par), c(1, 4))
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(modis_injected_par)))
  # expect "time" column
  testthat::expect_true("time" %in% names(modis_injected_par))
})


################################################################################
##### inject_modis
testthat::test_that("inject_modis (MOD11A1)", {
  withr::local_package("rlang")

  # sample location
  loc <- sf::st_as_sf(
    data.frame(lon = -90, lat = 39.5, site_id = "site_id"),
    coords = c("lon", "lat"),
    crs = "EPSG:4326"
  )

  # identify MOD11A1 files
  mod11a1_files <- list.files(
    testthat::test_path("..", "testdata", "injection", "modis", "mod11a1"),
    full.names = TRUE
  )

  # expect no error with MOD11A1 files
  testthat::expect_warning(
    modis_injected <- beethoven::inject_modis(
      locs = loc,
      injection = list(
        from = mod11a1_files,
        subdataset = "(LST_)",
        name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
        preprocess = amadeus::process_modis_merge,
        radius = c(100)
      )
    )
  )
  # expect a data.frame
  testthat::expect_s3_class(modis_injected, "data.frame")
  # expect 1 row and 4 columns
  testthat::expect_equal(dim(modis_injected), c(1, 4))
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(modis_injected)))
  # expect "time" column
  testthat::expect_true("time" %in% names(modis_injected))
})


################################################################################
##### inject_geos
testthat::test_that("inject_geos (chm, aqc, and combined)", {
  withr::local_package("rlang")

  # sample location
  loc <- sf::st_as_sf(
    data.frame(lon = -78.8277, lat = 35.95013, site_id = "A1"),
    coords = c("lon", "lat")
  )

  # aqc
  aqc_injection <- list(
    path = testthat::test_path("..", "testdata", "injection", "geos", "aqc"),
    date = c("2018-01-01", "2018-01-01")
  )
  # expected warning due to less than 24 data files
  testthat::expect_warning(
    calc_aqc <- beethoven::inject_geos(
      locs = loc,
      injection = aqc_injection
    )
  )
  # expect a data.frame
  testthat::expect_s3_class(calc_aqc, "data.frame")
  # expect 1 row and 6 columns
  testthat::expect_equal(dim(calc_aqc), c(1, 6))
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(calc_aqc)))

  # chm
  chm_injection <- list(
    path = testthat::test_path("..", "testdata", "injection", "geos", "chm"),
    date = c("2018-01-01", "2018-01-01")
  )
  # expected warning due to less than 24 data files
  testthat::expect_warning(
    calc_chm <- beethoven::inject_geos(
      locs = loc,
      injection = chm_injection
    )
  )
  # expect a data.frame
  testthat::expect_s3_class(calc_chm, "data.frame")
  # expect 1 row and 53 columns
  testthat::expect_equal(dim(calc_chm), c(1, 53))
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(calc_chm)))

  # aqc (direct file path)
  aqc_direct <- list(
    path = list.files(
      testthat::test_path("..", "testdata", "injection", "geos", "aqc"),
      full.names = TRUE
    ),
    date = c("2018-01-01", "2018-01-01")
  )
  # expected warning due to less than 24 data files
  testthat::expect_warning(
    calc_direct <- beethoven::inject_geos(
      locs = loc,
      injection = aqc_direct
    )
  )
  # expect a data.frame
  testthat::expect_s3_class(calc_direct, "data.frame")
  # expect 2 row and 6 columns
  testthat::expect_equal(dim(calc_direct), c(1, 6))
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(calc_direct)))

  # expect error when two collections are in the same folder
  testthat::expect_error(
    beethoven::inject_geos(
      locs = loc,
      injection = list(
        path = testthat::test_path(
          "..",
          "testdata",
          "injection",
          "geos",
          "error"
        ),
        date = c("2018-01-01", "2018-01-01")
      )
    )
  )
})


################################################################################
##### inject_gmted
testthat::test_that("inject_gmted (breakline emphasis)", {
  withr::local_package("rlang")

  # sample location
  loc <- data.frame(lon = -78.8277, lat = 35.95013, site_id = "A1")

  # breakline emphasis
  gmted_injection <- list(
    path = testthat::test_path(
      "..",
      "testdata",
      "injection",
      "gmted",
      "be75_grd"
    )
  )
  # expect no error with breakline emphasis data
  testthat::expect_no_error(
    calc_be <- beethoven::inject_gmted(
      locs = loc,
      variable = "Breakline Emphasis",
      radii = c(10, 100),
      injection = gmted_injection
    )
  )
  # expect a data.frame
  testthat::expect_s3_class(calc_be, "data.frame")
  # expect 1 row and 3 columns
  testthat::expect_equal(dim(calc_be), c(1, 3))
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(calc_be)))

  # expect error with missing variable
  # NOTE: direct test of calc_gmted_direct
  testthat::expect_error(
    beethoven::calc_gmted_direct(
      locs = loc,
      locs_id = "site_id",
      radii = c(10, 100),
      path = gmted_injection$path,
    )
  )
})


################################################################################
##### reduce_merge
testthat::test_that("reduce_merge joins all relevant data.frames", {
  withr::local_package("rlang")

  # Create example data tables
  dt1 <- data.frame(a = 1:3, b = 4:6)
  dt2 <- data.frame(a = 2:4, c = 7:9)
  dt3 <- data.frame(a = 3:5, d = 10:12)

  # Merge the data tables
  # by = NULL will automatically detect the common column names
  testthat::expect_no_error(
    beethoven::reduce_merge(list(dt1, dt2, dt3), by = NULL)
  )
  rmerged <- beethoven::reduce_merge(list(dt1, dt2, dt3), by = "a")
  testthat::expect_s3_class(rmerged, "data.frame")
  testthat::expect_equal(ncol(rmerged), 4)
})

testthat::test_that("reduce_merge handles different number of rows", {
  withr::local_package("rlang")

  # Create example data tables
  dt4 <- data.frame(a = 1:3, b = 4:6)
  dt5 <- data.frame(a = 2:9, c = 7:14)

  # Merge the data tables
  # by = NULL will automatically detect the common column names
  testthat::expect_no_error(
    beethoven::reduce_merge(list(dt4, dt5), by = NULL)
  )
  rmerged <- beethoven::reduce_merge(list(dt4, dt5), by = "a")
  testthat::expect_s3_class(rmerged, "data.frame")
  testthat::expect_equal(ncol(rmerged), 3)
})


################################################################################
##### inject_match
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
      geometry = c(
        "POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))",
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
    imatched <- beethoven::inject_match(
      f = terra::intersect,
      args = push_args
    )
  )
  testthat::expect_s4_class(imatched, "SpatVector")
})


################################################################################
##### inject_nlcd
testthat::test_that("inject_nlcd (2021)", {
  testthat::skip_on_ci()
  withr::local_package("rlang")

  # sample location
  loc <- terra::vect(
    data.frame(lon = -78.85, lat = 36.09, site_id = "A1"),
    crs = "EPSG:4326"
  )

  # expect no error with year 2021
  testthat::expect_no_error(
    nlcd_2021 <- beethoven::inject_nlcd(
      year = 2021,
      radius = 100,
      from = amadeus::process_nlcd(
        path = testthat::test_path("..", "testdata", "injection", "nlcd2"),
        year = 2021
      ),
      locs = loc,
      locs_id = "site_id"
    )
  )
  # expect data.frame
  testthat::expect_s3_class(nlcd_2021, "data.frame")
  # expect 1 row and 4 columns
  testthat::expect_equal(dim(nlcd_2021), c(1, 6))
})
