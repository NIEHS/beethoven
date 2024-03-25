#' @author Insang Song
#' @title space-time cross-validation function tests

testthat::test_that("leave-ones run without errors", {
  withr::local_package("sf")
  withr::local_package("sftime")
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  set.seed(202311)
  nco <- sf::st_read(testthat::test_path("../testdata/test_nc_output.nc")) |>
    unique()
  nco_s <- nco |>
    dplyr::sample_n(10)
  ncost <-
    split(x = seq(1, 10), f = seq(1, 10)) |>
    lapply(function(x) as.POSIXct("2022-11-30") + (x * 8.64e4)) |>
    lapply(function(x) mutate(nco_s, time = x)) |>
    Reduce(rbind, x = _)
  # to sftime
  ncost <-
    sftime::st_as_sftime(
                         ncost,
                         time_column_name = "time")

  testthat::expect_error(generate_cv_index(ncost, "lolo"))
  testthat::expect_error(generate_cv_index(ncost, "lolo", sp_fold = 3L))

  # convert to stdt
  ncostdt <- convert_stobj_to_stdt(ncost)

  ncostdte <- ncostdt
  ncostdte$stdt$sp_index <- seq(1, nrow(ncostdte$stdt))
  testthat::expect_no_error(sppre <- generate_spt_index(ncostdte, "spatial"))
  testthat::expect_no_error(
    spprest <- generate_spt_index(ncostdt, "spatiotemporal")
  )

  # spatial and temporal unique values
  slength <- nrow(nco_s)
  tlength <- length(unique(ncost$time))
  rlength <- nrow(ncost)

  # indices
  index_lolo <- generate_cv_index(ncostdt, "lolo")
  index_loto <- generate_cv_index(ncostdt, "loto")
  index_lolto <- generate_cv_index(ncostdt, "lolto")

  ncodto <- data.table::copy(ncostdt$stdt)
  ncodto$lolo <- index_lolo
  ncodto$loto <- index_loto
  ncodto$lolto <- index_lolto

  # type check
  testthat::expect_true(is.numeric(index_lolo))
  testthat::expect_true(is.numeric(index_loto))
  testthat::expect_true(is.numeric(index_lolto))

  testthat::expect_equal(max(index_lolo), slength)
  testthat::expect_equal(max(index_loto), tlength)
  testthat::expect_equal(max(index_lolto), rlength)

}
)


testthat::test_that("leave-block-outs work as expected", {
  withr::local_package("sf")
  withr::local_package("sftime")
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  set.seed(202311)
  nco <-
    sf::st_read(
      testthat::test_path("..", "testdata/test_nc_output.nc")
    ) |>
    unique()
  sf::st_crs(nco) <- "OGC:WGS84"

  # data preparation for larger stdt than the previous example
  ncost <-
    split(x = seq(1, 10), f = seq(1, 10)) |>
    lapply(function(x) as.POSIXct("2022-11-30") + (x * 8.64e4)) |>
    lapply(function(x) mutate(nco, time = x)) |>
    Reduce(rbind, x = _)

  ncost <-
    sftime::st_as_sftime(
                         ncost,
                         time_column_name = "time")

  # to stdt
  ncostdt <- convert_stobj_to_stdt(ncost)

  ncv_fold <- 5L
  ncv_sp <- 3L
  ncv_t <- 5L

  testthat::expect_no_error(
    index_lblo <- generate_cv_index(ncostdt, "lblo", cv_fold = ncv_fold)
  )
  testthat::expect_no_error(
    ret_spblock <- generate_block_sp_index(ncostdt, cv_fold = ncv_fold)
  )
  testthat::expect_no_error(
    index_lbto <- generate_cv_index(ncostdt, "lbto", cv_fold = ncv_fold)
  )
  testthat::expect_no_error(
    index_lblto <-
      generate_cv_index(
                        ncostdt,
                        "lblto",
                        sp_fold = ncv_sp,
                        t_fold = ncv_t)
  )

  # max value test
  testthat::expect_equal(max(index_lblo), ncv_fold)
  testthat::expect_equal(max(index_lbto), ncv_fold)
  testthat::expect_equal(max(index_lblto), ncv_sp * ncv_t)

  # raw coordinate based lblo should have attributes
  testthat::expect_identical(class(ret_spblock), c("list", "stdt"))
  testthat::expect_true(!is.null(attr(ret_spblock, "kmeans_centers")))
  testthat::expect_true(!is.null(attr(ret_spblock, "kmeans_sizes")))

  # external block data is present
  eco4 <-
    readRDS(
      testthat::test_path("..", "testdata/ecoregion_lv4.rds")
    )
  suppressWarnings(sf::st_crs(eco4) <- "OGC:WGS84")
  eco4d <- eco4 |>
    dplyr::group_by(US_L3CODE, US_L3NAME) |>
    dplyr::summarize(nsubregions = n()) |>
    ungroup()
  neco4 <- nrow(eco4d)


  ncodt <- data.table::copy(ncostdt$stdt)
  # when sf input is entered
  index_lblo_sf <-
    generate_cv_index(
                      ncostdt, "lblo",
                      blocks = eco4d,
                      block_id = "US_L3NAME")
  # error case: blocks are sf, but no block_id
  testthat::expect_error(
    generate_cv_index(
                      ncostdt, "lblo",
                      blocks = eco4d,
                      block_id = NULL)
  )

  # when SpatVector input is entered
  index_lblo_tr <-
    generate_cv_index(
                      ncostdt, "lblo",
                      blocks = terra::vect(eco4d),
                      block_id = "US_L3NAME")

  # no block_id
  expect_error(
    generate_cv_index(
                      ncostdt, "lblo",
                      blocks = terra::vect(eco4d),
                      block_id = "USS")
  )


  # If returned index is character, convert to integer via factor
  if (any(is.factor(index_lblo_sf), is.factor(index_lblo_tr))) {
    index_lblo_sfn <- as.numeric(index_lblo_sf)
    index_lblo_trn <- as.numeric(index_lblo_tr)
  }

  # worked as expected with the external blocks reference
  testthat::expect_true(is.numeric(index_lblo_sfn))
  testthat::expect_true(is.numeric(index_lblo_trn))
  testthat::expect_equal(max(index_lblo_sfn), neco4)
  testthat::expect_equal(max(index_lblo_trn), neco4)

  # using sf or SpatVector should return the same result
  testthat::expect_true(all.equal(index_lblo_sf, index_lblo_tr))

  # intentional duplicates in block name
  eco4e <- eco4d
  eco4e$US_L3NAME[2] <- eco4e$US_L3NAME[1]
  testthat::expect_error(
    generate_cv_index(
                      ncostdt, "lblo",
                      blocks = terra::vect(eco4e),
                      block_id = "US_L3NAME")
  )

  # numeric block case
  testthat::expect_no_error(
    generate_cv_index(
      ncostdt, "lblo",
      blocks = c(0.5, 0.5)
    )
  )

  # lblo error case
  testthat::expect_error(
    generate_cv_index(ncostdt, "lblo", cv_fold = NULL, blocks = NULL)
  )

  # lbto error cases
  testthat::expect_error(
    generate_cv_index(ncostdt, "lbto", NULL)
  )
  testthat::expect_error(
    generate_cv_index(ncostdt, "lbto", 20L)
  )
}
)


testthat::test_that("random cross-validation abides", {
  withr::local_package("sf")
  withr::local_package("sftime")
  withr::local_package("terra")
  withr::local_package("data.table")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  nco <- sf::st_read("../testdata/test_nc_output.nc") |>
    unique()

  # data preparation for larger stdt than the previous example
  ncost <-
    split(x = seq(1, 10), f = seq(1, 10)) |>
    lapply(function(x) as.POSIXct("2022-11-30") + (x * 8.64e4)) |>
    lapply(function(x) mutate(nco, time = x)) |>
    Reduce(rbind, x = _)

  ncost <-
    sftime::st_as_sftime(
                         ncost,
                         time_column_name = "time")

  # to stdt
  ncostdt <- convert_stobj_to_stdt(ncost)
  ncv_fold <- 10L
  testthat::expect_no_error(
    index_random <- generate_cv_index(ncostdt, "random", cv_fold = ncv_fold)
  )

  testthat::expect_true(is.numeric(index_random))
  testthat::expect_equal(max(index_random), ncv_fold)

  # error case
  testthat::expect_error(
    generate_cv_index(ncostdt, "random", NULL)
  )
})
