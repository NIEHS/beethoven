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
  kp_path <- "../testdata/koppen_subset.tif"

  testthat::expect_no_error(
    kg_res <- calc_koppen_geiger(
      path_koppen = kp_path,
      sites = site_faux
    )
  )
  testthat::expect_no_error(
    kg_res <- calc_koppen_geiger(
      path_koppen = kp_path,
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
      date = as.Date("2022-01-01")
    )

  testthat::expect_no_error(
    dum_res <- calc_temporal_dummies(
      sites = sites_faux
    )
  )

  # the result is a data frame
  testthat::expect_s3_class(dum_res, "data.frame")
  # ncol is equal to 12 + 5 + 7 + 4
  testthat::expect_equal(ncol(dum_res), 28L)
  # should have each of the indicator groups
  testthat::expect_equal(sum(unlist(kg_res[, -1:-4])), 3L)

  # error cases
  sites_faux_err <- sites_faux
  colnames(sites_faux_err)[4] <- "time"
  testthat::expect_error(
    dum_res <- calc_temporal_dummies(
      sites = sites_faux_err
    )
  )

  testthat::expect_error(
    dum_res <- calc_temporal_dummies(
      sites = as.matrix(sites_faux_err)
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
      path = "../testdata/eco_l3_clip.gpkg",
      sites = sites_faux,
      id_col = "site_id"
    )
  )

  # the result is a data frame
  testthat::expect_s3_class(ecor_res, "data.frame")
  # ncol is equal to 2 + 5 + 2 + 1 + 1
  testthat::expect_equal(ncol(ecor_res), 11L)
  # should have each of the indicator groups
  dum_cn <- grep("DUM_", colnames(ecor_res))
  testthat::expect_equal(
                         sum(unlist(ecor_res[, dum_cn])), 2L)


})


