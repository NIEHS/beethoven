################################################################################
##### unit and integration tests for covariate calculation functions
##### main files: R/calculate.R

################################################################################
##### par_narr
testthat::test_that("par_narr (weasd + omega)", {
  testthat::skip_on_ci()
  withr::local_package("rlang")

  # sample location
  # $geometry is included to ensure it is properly dropped from the locations
  loc <- terra::vect(
    data.frame(
      lon = -78.8277,
      lat = 35.95013,
      site_id = "A1",
      geometry = "POINT (-78.8277 35.95013)"
    ),
    crs = "EPSG:4326"
  )

  # expect no error (weasd; no geometry)
  testthat::expect_no_error(
    par_weasd <- par_narr(
      domain = "weasd",
      path = testthat::test_path(
        "..", "testdata", "calculate", "narr", "weasd"
      ),
      date = c("2018-01-01", "2018-01-01"),
      locs = loc
    )
  )
  # expect a list
  testthat::expect_true(is.list(par_weasd))
  # expect list has one element
  testthat::expect_length(par_weasd, 1)
  # expect first element is a data.frame
  testthat::expect_s3_class(par_weasd[[1]], "data.frame")
  # expect 1 row and 3 columns
  testthat::expect_equal(dim(par_weasd[[1]]), c(1, 3))
  # expect proper column name setting
  # 0 length refers to "_NA_" (improperly named column) not found
  # in any of the column names
  testthat::expect_length(grep("_NA_", names(par_weasd)), 0)
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(par_weasd)))
  # expect "geometry" excluded from names
  testthat::expect_false("geometry" %in% names(par_weasd))


  # expect no error (omega)
  testthat::expect_no_error(
    par_omega <- par_narr(
      domain = "omega",
      path = testthat::test_path(
        "..", "testdata", "calculate", "narr", "omega"
      ),
      date = c("2018-01-01", "2018-01-01"),
      locs = loc
    )
  )
  # expect a list
  testthat::expect_true(is.list(par_omega))
  # expect list has one element
  testthat::expect_length(par_weasd, 1)
  # expect first element is a data.frame
  testthat::expect_s3_class(par_omega[[1]], "data.frame")
  # expect 1 row and 31 columns
  testthat::expect_equal(dim(par_omega[[1]]), c(1, 31))
  # expect proper column name setting
  # 0 length refers to "_NA_" (improperly named column) not found
  # in any of the column names
  testthat::expect_length(grep("_NA_", names(par_omega)), 0)
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(par_omega)))
  # expect "geometry" dropped from names
  testthat::expect_false("geometry" %in% names(par_omega))


  # expect error with non-existent path
  testthat::expect_error(
    par_narr(
      domain = "omega",
      path = "dOeS/nOt/ExIsT",
      date = c("2018-01-01", "2018-01-01"),
      locs = loc,
      nthreads = 1
    )
  )

})

################################################################################
##### query_modis_files
testthat::test_that("query_modis_files", {

  path <- testthat::test_path("..", "testdata", "calculate", "modis")
  list <- list(
    c("2018001"), c("2018002")
  )

  # expect no error
  testthat::expect_no_error(
    files1 <- query_modis_files(path, list, index = 1)
  )
  # expect 23 files
  testthat::expect_length(files1, 23)

  # expect no error
  testthat::expect_no_error(
    files2 <- query_modis_files(path, list, index = 1)
  )
  # expect 23 files
  testthat::expect_length(files2, 23)
})

################################################################################
##### calculate_modis
testthat::test_that("calculate_modis (MOD11A1)", {
  
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
    modis_calculate_df <- beethoven::calculate_modis(
      from = mod11a1_files,
      locs = loc,
      subdataset = "(LST_)",
      name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
      preprocess = amadeus::process_modis_merge,
      radius = c(100),
      package_list_add = c("terra"),
      export_list_add = c("terra")
    )
  )
  # expect a data.frame
  testthat::expect_s3_class(modis_calculate_df, "data.frame")
  # expect 1 row and 4 columns
  testthat::expect_equal(dim(modis_calculate_df), c(1, 4))
  # expect no NA values in any column
  testthat::expect_false("TRUE" %in% any(is.na(modis_calculate_df)))
  # expect "time" column
  testthat::expect_true("time" %in% names(modis_calculate_df))

  testthat::expect_warning(
    modis_calculate_sf <- beethoven::calculate_modis(
      from = mod11a1_files,
      locs = loc,
      subdataset = "(LST_)",
      name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
      preprocess = amadeus::process_modis_merge,
      radius = c(100),
      geom = "sf"
    )
  )
  # expect an sf
  testthat::expect_true(methods::is(modis_calculate_sf, "sf"))

  # testthat::expect_warning(
  #   modis_calculate_terra <- beethoven::calculate_modis(
  #     from = mod11a1_files,
  #     locs = loc,
  #     subdataset = "(LST_)",
  #     name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
  #     preprocess = amadeus::process_modis_merge,
  #     radius = c(100),
  #     geom = "terra"
  #   )
  # )
  # # expect a terra
  # testthat::expect_true(methods::is(modis_calculate_terra, "SpatVector"))


  # expect error with non-function preprocessor
  testthat::expect_error(
    beethoven::calculate_modis(
      from = mod11a1_files,
      locs = loc,
      subdataset = "(LST_)",
      name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
      preprocess = 100L,
      radius = c(100)
    )
  )

  # expect error with non-location loc
  testthat::expect_error(
    beethoven::calculate_modis(
      from = mod11a1_files,
      locs = 4,
      subdataset = "(LST_)",
      name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_"),
      preprocess = amadeus::process_modis_merge,
      radius = c(100)
    )
  )

  # expect message with extra covariate names
  testthat::expect_warning(
    beethoven::calculate_modis(
      from = mod11a1_files,
      locs = loc,
      subdataset = "(LST_)",
      name_covariates = c("MOD_LSTNT_0_", "MOD_LSTDY_0_", "extra1", "extra2"),
      preprocess = amadeus::process_modis_merge,
      radius = c(100)
    )
  )

})
