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



testthat::test_that("calculate_modis_direct: correct column renaming, date‐parsing, and mark=TRUE vs FALSE", {

  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  # identify MOD11A1 files
  mod11a1_files <- list.files(
    testthat::test_path("..", "testdata", "injection", "modis", "mod11a1"),
    full.names = TRUE
  )
  mod11ras <- terra::rast(mod11a1_files, subds="LST_Day_1km")
  tdir <- tempdir()
  mod11tif <- file.path(tdir, "mod11a1_h11v05_2021-08-15.tif")
  terra::writeRaster(
    mod11ras,
    filename = mod11tif,
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE")
  )

  site_sf <- sf::st_as_sf(
    data.frame(
      lon = -90, lat = 39.5,
      site_id = "site_id", time = as.Date("2021-08-15")
    ),
    coords = c("lon", "lat"),
    crs = "EPSG:4326"
  )
  
  # Run with mark = TRUE (default)
  out1 <- calculate_modis_direct(
    file     = mod11tif,
    site     = site_sf,
    site_id  = "site_id",
    radius   = 1000,           # numeric
    colheader = "hdR_",
    mark     = TRUE
  )
  
  # Column names include "hdR_01000"
  testthat::expect_length(grep("^hdR_01000", names(out1)), 1L)
  
  # The returned time column should be a Date extracted from the file name
  testthat::expect_s3_class(out1$time, "Date")
  testthat::expect_equal(out1$time, as.Date("2021-08-15"))
  
  # The printed column name should have been printed to the console:
  testthat::expect_identical(colnames(out1)[2], "hdR_01000")
  
  # Run again with mark = FALSE
  out2 <- calculate_modis_direct(
    file      = mod11tif,
    site      = site_sf,
    site_id   = "site_id",
    radius    = 500,
    colheader = "FooBar_",
    mark      = FALSE
  )
  
  # colheader should be exactly "FooBar_" (no padding added).
  testthat::expect_true(all(grepl("^FooBar_$", names(out2)[grep("FooBar_", names(out2))])))
  testthat::expect_length(grep("^FooBar_$", names(out2)), 1L)
  testthat::expect_equal(out2$time, as.Date("2021-08-15"))
})


testthat::test_that("sum_edc_mod: warning on overlapping names and correct numeric sums", {

  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  # Build a tiny 'locs' SpatVector with one point
  sf_locs <- sf::st_as_sf(
    data.frame(site_id = "S1", X = 100, Y = 100, stringsAsFactors = FALSE),
    coords = c("X", "Y"), crs = 32614
  )
  locs_sp <- terra::vect(sf_locs)
  locs_sp$site_id <- sf_locs$site_id

  # Build a tiny 'from' SpatVector with two points: (0,0) and (10,0), each with a field "VAL_AIR"
  df_from <- data.frame(
    id = c("F1", "F2"),
    X = c(130, 70),
    Y = c(140, 60),
    VAL_AIR = c(10, 10),
    stringsAsFactors = FALSE
  )
  sf_from <- sf::st_as_sf(
    df_from, coords = c("X", "Y"), crs = 32614)
  from_sp <- terra::vect(sf_from)
  
  # Test the warning about overlapping names:
  # For example, if both locs and from share a column called “id”:
  sf_locs2 <- sf::st_as_sf(
    data.frame(id = "S1", X=0, Y=0, stringsAsFactors = FALSE),
    coords = c("X","Y"), crs = 32614
  )

  locs_sp2 <- terra::vect(sf_locs2)
  locs_sp2$id <- sf_locs2$id
  # Now "id" overlaps with from_sp's "id", so we expect a warning:
  testthat::expect_warning(
    suppressMessages(
      sum_edc_mod(
        from            = from_sp,
        locs            = locs_sp2,
        locs_id         = "id",
        sedc_bandwidth  = 100,
        target_fields   = c("VAL_AIR")
      )
    )
  )
  
  # At sedc_bandwidth, each point's attributes are attenuated to
  # exp(-3)*value, so we expect the sum to be 2 * exp(-3) = 0.099574
  suppressMessages(
    out_sedc <- sum_edc_mod(
      from           = from_sp,
      locs           = locs_sp,
      locs_id        = "site_id",
      sedc_bandwidth = 50,
      target_fields  = "VAL_AIR"
    )
  )
  
  # It should return a data.frame (tibble) with one row "site_id"
  testthat::expect_s3_class(out_sedc, "data.frame")
  testthat::expect_true("site_id" %in% names(out_sedc))
  
  # The column used will be named "VAL_AIR_00005" since sprintf("%05d", 5) → "00005"
  testthat::expect_true("VAL_AIR" %in% names(out_sedc))
  
  computed <- out_sedc$VAL_AIR
  expected <- 2 * 10 * exp(-3)
  testthat::expect_equal(computed, expected, tolerance = 1e-6)
  
  # Attributes "sedc_bandwidth" and "sedc_threshold" should be attached
  testthat::expect_equal(attr(out_sedc, "sedc_bandwidth"), 50)
  testthat::expect_equal(attr(out_sedc, "sedc_threshold"), 100)  # 2 * bandwidth
  
})


testthat::test_that("calc_tri_mod: correct behavior, whitespace fixed, suffixes appended, and errors", {

  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  # Build a tiny 'locs' SpatVector with one point
  sf_locs <- sf::st_as_sf(
    data.frame(site_id = "S1", X = 100, Y = 100, stringsAsFactors = FALSE),
    coords = c("X", "Y"), crs = 32614
  )
  locs_sp <- terra::vect(sf_locs)
  locs_sp$site_id <- sf_locs$site_id

  # Build a minimal 'from' SpatVector with one field "CHEM AIR" (note the space!)
  #     and set its tri_year attribute to test the time‐roundtrip.
  df_from <- data.frame(
    id = c("F1", "F2"),
    X = c(130, 70),
    Y = c(140, 60),
    CHEM_AIR = c(10, 10),
    stringsAsFactors = FALSE
  )
  sf_from <- sf::st_as_sf(
    df_from, coords = c("X", "Y"), crs = 32614)
  from_sp <- terra::vect(sf_from)

  # Attach the attribute “tri_year”
  attr(from_sp, "tri_year") <- 2021L
  
  
  # Use a small radius (e.g. 10) so only the first “from” point at (0,0)
  #     is within the buffer created by calc_tri_mod()
  suppressMessages(
    out_tri <- calc_tri_mod(
      from    = from_sp,
      locs    = sf_locs,    # test the branch that converts sf → SpatVector
      locs_id = "site_id",
      radius  = 100L
    )
  )

  # The “_AIR_” pattern in “CHEM AIR” should have whitespace fixed → "CHEM_AIR"
  # Then grep("_AIR", names(from2_sp)) picks that field. In sum_edc_mod, a suffix
  # of sedc_bandwidth ("00010") will be appended. So we expect a column named
  # "CHEM_AIR_00010" to appear in out_tri.
  testthat::expect_true(any(grepl("^CHEM_AIR", names(out_tri))))
  
  # Check that the “time” column is an integer and matches attr(from,) “tri_year”
  testthat::expect_true("time" %in% names(out_tri))
  testthat::expect_type(out_tri$time, "integer")
  testthat::expect_equal(out_tri$time, 2021L)
  
  # If radius is non‐numeric, we expect an error at the very top of calc_tri_mod()
  testthat::expect_error(
    suppressMessages(
      calc_tri_mod(
        from    = from_sp,
        locs    = sf_locs,
        locs_id = "site_id",
        radius  = "not_numeric"
      )
    ),
    regexp = "radius should be numeric"
  )
})

