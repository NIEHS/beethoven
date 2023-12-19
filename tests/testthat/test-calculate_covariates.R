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
