## test for calculating covariates

testthat::test_that("calc_koppen_geiger works well", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  site_faux <-
    data.frame(
      site_id = "37031000188101",
      lon = -78.90,
      lat = 35.97
    )
  site_faux <- terra::vect(site_faux, crs = "EPSG:4326")
  kp_path <- testthat::test_path("..", "testdata", "koppen_subset.tif")

  testthat::expect_no_error(
    kg_res <- calc_koppen_geiger(
      path = kp_path,
      sites = site_faux
    )
  )
  testthat::expect_no_error(
    kg_res <- calc_koppen_geiger(
      path = kp_path,
      sites = sf::st_as_sf(site_faux)
    )
  )
  # the result is a data frame
  testthat::expect_s3_class(kg_res, "data.frame")
  # ncol is equal to 6
  testthat::expect_equal(ncol(kg_res), 6)
  # should have only one climate zone
  testthat::expect_equal(sum(unlist(kg_res[, -1])), 1)
  testthat::expect_true(is.function(calc_koeppen_geiger))
})

testthat::test_that("calc_dummies works well", {

  site_faux <-
    data.frame(
      site_id = "37031000188101",
      lon = -78.90,
      lat = 35.97,
      date = "2022-01-01"
    )

  testthat::expect_no_error(
    dum_res <- calc_temporal_dummies(
      sites = site_faux,
      domain_year = seq(2018L, 2022L)
    )
  )

  # the result is a data frame
  testthat::expect_s3_class(dum_res, "data.frame")
  # ncol is equal to 12 + 5 + 7 + 4
  testthat::expect_equal(ncol(dum_res), 28L)
  # should have each of the indicator groups
  testthat::expect_equal(sum(unlist(dum_res[, -1:-4])), 3L)

  # error cases
  site_faux_err <- site_faux
  colnames(site_faux_err)[4] <- "time"
  testthat::expect_error(
    dum_res <- calc_temporal_dummies(
      sites = site_faux_err
    )
  )

  testthat::expect_error(
    dum_res <- calc_temporal_dummies(
      sites = as.matrix(site_faux_err)
    )
  )

})

testthat::test_that("calc_ecoregion works well", {

  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  site_faux <-
    data.frame(
      site_id = "37999109988101",
      lon = -77.576,
      lat = 39.40,
      date = as.Date("2022-01-01")
    )
  site_faux <-
    terra::vect(
                site_faux,
                geom = c("lon", "lat"),
                crs = "EPSG:4326")
  site_faux <- terra::project(site_faux, "EPSG:5070")

  testthat::expect_no_error(
    ecor_res <- calc_ecoregion(
      path = testthat::test_path("..", "testdata", "eco_l3_clip.gpkg"),
      sites = site_faux,
      id_col = "site_id"
    )
  )

  # the result is a data frame
  testthat::expect_s3_class(ecor_res, "data.frame")
  # ncol is equal to 2 + 5 + 2 + 1 + 1
  testthat::expect_equal(ncol(ecor_res), 3L)
  # should have each of the indicator groups
  dum_cn <- grep("DUM_", colnames(ecor_res))
  testthat::expect_equal(
                         sum(unlist(ecor_res[, dum_cn])), 2L)
})


testthat::test_that("calc_modis works well.", {
  withr::local_package("terra")
  withr::local_package("foreach")
  withr::local_package("doRNG")
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))

  site_faux <-
    data.frame(
      site_id = "37999904288101",
      lon = -78.87,
      lat = 35.8734,
      date = as.Date("2021-08-15")
    )
  site_faux <-
    terra::vect(
                site_faux,
                geom = c("lon", "lat"),
                crs = "EPSG:4326")
  site_faux <- terra::project(site_faux, "EPSG:5070")

  # case 1: standard mod11a1
  path_mod11 <-
    testthat::test_path(
      "../testdata/modis/",
      "MOD11A1.A2021227.h11v05.061.2021228105320.hdf"
    )
  testthat::expect_no_error(
    calc_mod11 <-
      calc_modis(
        path = path_mod11,
        product = "MOD11A1",
        sites = site_faux,
        name_covariates = "MOD_NDVIV_0_",
        nthreads = 2
      )
  )

  # case 2: swath mod06l2
  path_mod06 <-
    list.files(
      "tests/testthat/../testdata/modis",
      "MOD06",
      full.names = TRUE
    )
  testthat::expect_no_error(
    calc_mod06 <-
      calc_modis(
        path = path_mod06,
        product = "MOD06_L2",
        sites = site_faux,
        name_covariates = c("MOD_CLFRN_0_", "MOD_CLFRD_0_"),
        nthreads = 2
      )
  )

  # case 3: VIIRS
  path_vnp46 <-
    list.files(
      "tests/testthat/../testdata/modis",
      "VNP46",
      full.names = TRUE
    )
  testthat::expect_no_error(
    calc_vnp46 <-
      calc_modis(
        path = path_vnp46,
        product = "VNP46A2",
        sites = site_faux,
        name_covariates = c("MOD_NITLT_0_"),
        tilelist =
          read.csv("tests/testthat/../../inst/extdata/modis_vnp46_tiles.csv"),
        nthreads = 2
      )
  )

})
