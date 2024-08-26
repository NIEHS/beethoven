################################################################################
##### unit and integration tests for covariate calculation functions
##### main files: R/calc_postprocessing.R

################################################################################
##### post_calc_merge_features
testthat::test_that("post_calc_merge_features", {
  # import sample data
  dt_geos <- readRDS(
    testthat::test_path("..", "testdata", "postprocessing", "dt_geos.rds")
  )
  dt_narr <- readRDS(
    testthat::test_path("..", "testdata", "postprocessing", "dt_narr.rds")
  )
  dt_gmted <- readRDS(
    testthat::test_path("..", "testdata", "postprocessing", "dt_gmted.rds")
  )

  # expect no error with two date-enabled data
  testthat::expect_no_error(
    dt_pcmf1 <- post_calc_merge_features(
      by = c("site_id"),
      time = TRUE,
      dt_geos,
      dt_narr
    )
  )
  # expect data.frame
  testthat::expect_s3_class(dt_pcmf1, "data.frame")
  # expect 15 rows (5 dates x 3 sites) and 183 columns
  testthat::expect_equal(dim(dt_pcmf1), c(15, 183))


  # expect no error with date and no-time data
  testthat::expect_no_error(
    dt_pcmf2 <- post_calc_merge_features(
      by = c("site_id"),
      time = FALSE,
      dt_geos,
      dt_gmted
    )
  )
  # expect data.frame
  testthat::expect_s3_class(dt_pcmf2, "data.frame")
  # expect 15 rows (5 dates x 3 sites) and 183 columns
  testthat::expect_equal(dim(dt_pcmf2), c(15, 85))
  # expect 1 elevation value for each site, regardless of time
  testthat::expect_length(
    unique(dt_pcmf2[dt_pcmf2$site_id == "06065101688101", "LDU_ESTDV_01000"]),
    1
  )


  # expect NA values with merging date wiith no-time data AND time = TRUE
  testthat::expect_no_error(
    dt_pcmf3 <- post_calc_merge_features(
      by = c("site_id"),
      time = TRUE,
      dt_geos,
      dt_gmted
    )
  )
  testthat::expect_true("TRUE" %in% any(is.na(dt_pcmf3)))


  # expect "lon" and "lat" are dropped when included
  lonlat <- data.frame(
    site_id = c("06065101688101", "30063002488101", "51650000888101"),
    lon = c(-80, -81, -82),
    lat = c(37, 38, 39)
  )
  dt_geos_ll <- merge(dt_geos, lonlat, by = "site_id")
  testthat::expect_no_error(
    dt_pcmf4 <- post_calc_merge_features(
      by = c("site_id"),
      time = TRUE,
      dt_geos_ll,
      dt_narr
    )
  )
  testthat::expect_false(unique(c("lon", "lat") %in% names(dt_pcmf4)))

})


################################################################################
##### post_calc_unify_timecols
testthat::test_that("post_calc_unify_timecols", {
  # import sample data
  dt_nlcd <- readRDS(
    testthat::test_path("..", "testdata", "postprocessing", "dt_nlcd.rds")
  )
  # set improper colname for NLCD years
  names(dt_nlcd)[2] <- "year"

  
  # expect no error when colname is set to time
  testthat::expect_no_error(
    dt_pcut <- post_calc_unify_timecols(
      df = dt_nlcd,
      candidates = "year",
      replace = "time"
    )
  )
  # expect time in names
  testthat::expect_true("time" %in% names(dt_pcut))
  # expect year is not in names
  testthat::expect_false("year" %in% names(dt_pcut))
  # expect dimensions retained
  testthat::expect_equal(dim(dt_pcut), dim(dt_nlcd))

  # expect error when > 1 candidate is matched
  testthat::expect_error(
    post_calc_unify_timecols(
      df = dt_nlcd,
      candidates = c("year", "LDU_TWATR_0_01000"),
      replace = "time"
    )
  )
})


################################################################################
##### post_calc_unify_timecols
testthat::test_that("post_calc_convert_time", {
  # import sample data
  dt_geos <- readRDS(
    testthat::test_path("..", "testdata", "postprocessing", "dt_geos.rds")
  )

  # expect no error converting to character
  testthat::expect_no_error(
    dt_pcct <- post_calc_convert_time(dt_geos)
  )
  # expect "time" is a character
  testthat::expect_true(is.character(dt_pcct$time))

  
  # expect error without time column
  dt_notime <- dt_geos[, -c("time")]
  testthat::expect_error(
    post_calc_convert_time(dt_notime)
  )
})


################################################################################
##### post_calc_join_yeardate
testthat::test_that("post_calc_join_yeardate", {
  # import sample data
  dt_nlcd <- readRDS(
    testthat::test_path("..", "testdata", "postprocessing", "dt_nlcd.rds")
  )


  # expect error when data.frame and integer
  testthat::expect_error(
    post_calc_join_yeardate(
      df_year = dt_nlcd,
      df_year = 100
    )
  )
})


################################################################################
##### post_calc_year_expand
testthat::test_that("post_calc_year_expand", {
  # expect no error
  testthat::expect_no_error(
    pcye <- post_calc_year_expand(
      time_start = 2000,
      time_end = 2025,
      time_unit = "year",
      time_available = c(2019, 2021, 2023)
    )
  )

  # expect length of 26 to match available time
  testthat::expect_length(pcye, 26)
  # expect first 21 = 2019
  testthat::expect_true(all(pcye[1:21] == 2019))
  # expect next 2 = 2021
  testthat::expect_true(all(pcye[22:23] == 2021))
  # expect next 3 = 2023
  testthat::expect_true(all(pcye[24:26] == 2023))
})


################################################################################
##### post_calc_df_year_expand
testthat::test_that("post_calc_df_year_expand", {
  # import sample data
  dt_nlcd <- readRDS(
    testthat::test_path("..", "testdata", "postprocessing", "dt_nlcd.rds")
  )
  # subset to get uneven rows
  dt_nlcd_sub <- dt_nlcd[1:4, ]


  # expect error with uneven rows in df
  testthat::expect_error(
    post_calc_df_year_expand(
      df = dt_nlcd_sub,
      locs_id = "site_id",
      time_field = "time",
      time_start = 2019,
      time_end = 2021,
      time_unit = "year"
    )
  )
})


################################################################################
##### post_calc_merge_all
testthat::test_that("post_calc_merge_all", {
  # sample data
  dt_narr <- readRDS(
    testthat::test_path("..", "testdata", "postprocessing", "dt_narr.rds")
  )
  dt_gmted <- readRDS(
    testthat::test_path("..", "testdata", "postprocessing", "dt_gmted.rds")
  )
  # create locs
  locs <- dt_narr[, c("site_id", "time")]

  
  # expect no error when merging spatial (gmted) to spatiotemporal (narr)
  testthat::expect_no_error(
    dt_pcma1 <- post_calc_merge_all(
      locs = locs,
      locs_id = "site_id",
      time_id = "time",
      target_years = seq(2018, 2022),
      df_sp = dt_gmted,
      df_spt = dt_narr
    )
  )
  # expect data.frame
  testthat::expect_s3_class(dt_pcma1, "data.frame")
  # expect 15 rows (3 sites x 5 days) and 180 columns
  testthat::expect_equal(dim(dt_pcma1), c(15, 180))
  # exepct time column is character
  testthat::expect_true(is.character(dt_pcma1$time))


  # create locs as sf
  locs$lon <- rep(c(-70, -71, -72), 5)
  locs$lat <- rep(c(30, 31, 32), 5)
  locs_sf <- sf::st_as_sf(
    locs,
    coords = c("lon", "lat"),
    crs = "ESPG:4326"
  )
  # expect no error when locs are sf
  testthat::expect_no_error(
    dt_pcma2 <- post_calc_merge_all(
      locs = locs_sf,
      locs_id = "site_id",
      time_id = "time",
      target_years = seq(2018, 2022),
      df_sp = dt_gmted,
      df_spt = dt_narr
    )
  )
  # expect data.frame
  testthat::expect_s3_class(dt_pcma2, "data.frame")
  # expect 15 rows (3 sites x 5 days) and 180 columns
  testthat::expect_equal(dim(dt_pcma2), c(15, 180))
  # exepct time column is character
  testthat::expect_true(is.character(dt_pcma2$time))
  # expect lon, lat, geom are NOT in names
  testthat::expect_equal(
    c("lon", "lat", "geometry") %in% names(dt_pcma2),
    c(FALSE, FALSE, FALSE)
  )

})


################################################################################
##### post_calc_drop_cols
# testthat::test_that("post_calc_drop_cols", {
#   # sample data
#   dt_sample <- data.frame(
#     lon = -70:-75,
#     lat = 30:35,
#     geoid = sprintf("L%s", 1:6),
#     year = 2000:2005,
#     other = rep("saved", 6)
#   )
  
#   # expect no error with strict = FALSE
#   testthat::expect_no_error(
#     dt_weak <- post_calc_drop_cols(
#       df = dt_sample,
#       candidates = "geoid|year",
#       strict = FALSE
#     )
#   )
#   # expect 3 columns (dropped "year" and "geoid")
#   testthat::expect_length(names(dt_weak))


#   # expect no error with strict = TRUE
#   testthat::expect_no_error(
#     dt_strict <- post_calc_drop_cols(
#       df = dt_sample,
#       strict = TRUE
#     )
#   )
#   # expect 1 columns (only "saved")
#   testthat::expect_length(names(dt_strict))
# })


################################################################################
##### post_calc_autojoin
testthat::test_that("post_calc_autojoin expands and joins data.frames with different temporal resolutions", {
  withr::local_package("dplyr")
  withr::local_package("data.table")

  # data.frame that are resolved daily
  df_fine1 <-
    expand.grid(
      site_id = rep(LETTERS[1:4], 5),
      time =
      rep(
        seq.Date(as.Date("2021-12-30"), as.Date("2022-01-03"), by = 1),
        each = 4
      ),
      value = rnorm(20)
    )

  df_fine2 <-
    data.frame(
      site_id = rep(c("A", "B", "C", "D"), 2),
      time =
      c(
        as.Date(c("2022-01-01", "2022-01-02", "2021-12-31", "2021-01-03")),
        as.Date(c("2022-01-01", "2022-01-02", "2021-12-31", "2021-01-03")) + 1
      ),
      value2 = c(c(1+2i, 2+3i, 3+4i, 5+6i),
                c(1+2i, 2+3i, 3+4i, 5+6i) + 1)
    )
  # attempt to join two data.frames with the same temporal resolution
  # will run quietly
  testthat::expect_no_error(
    autojoin_iden <-
      post_calc_autojoin(
        df_fine = df_fine1, df_coarse = df_fine2,
        field_sp = "site_id", field_t = "time"
      )
  )

  df_coarse0 <- data.frame(site_id = c("A", "B", "C", "D"),
                           time = rep(2020, 4),
                           other_value = c(10, 20, 30, 40))
  # df_coarse0 includes year values. It will lead to an error when
  # attempting to convert the year values to Date objects.
  # we leverage that characteristics to detect whether the temporal
  # values are year or date. In the current beethoven implementation,
  # there are only two temporal resolutions: daily and yearly.
  testthat::expect_message(
    autojoin_diff <- post_calc_autojoin(df_fine1, df_coarse0)
  )

  # remove the year column
  df_coarse2 <- df_coarse0[, -2]
  # then it will quietly join two data.frames by site_id
  testthat::expect_no_error(
    autojoin_diff <- post_calc_autojoin(df_fine1, df_coarse2)
  )

})


################################################################################
##### impute_all
testthat::test_that("impute_all", {
  # sample data
  # sample data is 3 sites for 5 days
  dt_full <- qs::qread(
    testthat::test_path("..", "testdata", "postprocessing", "dt_full.qs")
  )

  # expect no errors on imputation (read object)
  testthat::expect_no_error(
    dt_imputed1 <- impute_all(
      dt = dt_full,
      period = c("2018-01-01", "2018-01-10"),
      nthreads_dt = 1L,
      nthreads_collapse = 1L,
      nthreads_imputation = 1L
    )
  )
  # expect data.frame
  testthat::expect_s3_class(dt_imputed1, "data.frame")
  # expect 33 rows and 743 columns (columns dropped due to NA in sample)
  testthat::expect_equal(dim(dt_imputed1), c(33, 743))
  # expect no NA values after imputation
  testthat::expect_false(TRUE %in% unique(is.na(dt_imputed1)))
  # expect "site_id" and "time" in names
  testthat::expect_equal(
    c("site_id", "time") %in% names(dt_imputed1), c(TRUE, TRUE)
  )
  # expect 10 dates
  testthat::expect_length(unique(dt_imputed1$time), 10)


  # expect no errors on imputation (file path)
  testthat::expect_no_error(
    dt_imputed2 <- impute_all(
      dt = testthat::test_path(
        "..", "testdata", "postprocessing", "dt_full.qs"
      ),
      period = c("2018-01-01", "2018-01-10"),
      nthreads_dt = 1L,
      nthreads_collapse = 1L,
      nthreads_imputation = 1L
    )
  )
  # expect data.frame
  testthat::expect_s3_class(dt_imputed2, "data.frame")
  # expect 33 rows and 743 columns (columns dropped due to NA in sample)
  testthat::expect_equal(dim(dt_imputed2), c(33, 743))
  # expect no NA values after imputation
  testthat::expect_false(TRUE %in% unique(is.na(dt_imputed2)))
  # expect "site_id" and "time" in names
  testthat::expect_equal(
    c("site_id", "time") %in% names(dt_imputed2), c(TRUE, TRUE)
  )
  # expect 10 dates
  testthat::expect_length(unique(dt_imputed2$time), 10)

  # expect error with non-qs file path
  testthat::expect_error(
    impute_all(
      dt = testthat::test_path(
        "..", "testdata", "postprocessing", "wRoNgFiLePaTh.rDs"
      ),
      period = c("2018-01-01", "2018-01-10"),
      nthreads_dt = 1L,
      nthreads_collapse = 1L,
      nthreads_imputation = 1L
    )
  )
})


################################################################################
##### append_predecessors
testthat::test_that("append_predecessors", {
  # working directory
  append_directory <- file.path(paste0(tempdir(), "/append/"))

  # expect no error for imputed data (impute 1)
  testthat::expect_no_error(
    dt_imputed1 <- impute_all(
      dt = testthat::test_path(
        "..", "testdata", "postprocessing", "dt_full.qs"
      ),
      period = c("2018-01-01", "2018-01-10"),
      nthreads_dt = 1L,
      nthreads_collapse = 1L,
      nthreads_imputation = 1L
    )
  )
  # expect no error for no predecessor
  testthat::expect_no_error(
    path_append1 <- append_predecessors(
      path_qs = append_directory,
      period_new = c("2018-01-01", "2018-01-01"),
      input_new = dt_imputed1,
      nthreads = 1
    )
  )
  # expect file path created
  testthat::expect_true(file.exists(paste0(append_directory, path_append1)))
  # expect no error on re-read
  testthat::expect_no_error(
    dt_append1 <- qs::qread(paste0(append_directory, path_append1))
  )
  # expect data.frame
  testthat::expect_s3_class(dt_append1, "data.frame")
  # expect 33 rows and 743 columns (columns dropped due to NA in sample)
  testthat::expect_equal(dim(dt_append1), c(33, 743))
  # expect 10 dates
  testthat::expect_length(unique(dt_append1$time), 10)


  # expect no error for imputed data (impute 2)
  testthat::expect_no_error(
    dt_imputed2 <- impute_all(
      dt = testthat::test_path(
        "..", "testdata", "postprocessing", "dt_append.qs"
      ),
      period = c("2018-01-11", "2018-01-20"),
      nthreads_dt = 1L,
      nthreads_collapse = 1L,
      nthreads_imputation = 1L
    )
  )
  # SUBSET dt_imputed2 TO MATCH COLUMNS OF dt_imputed1 
  # dt_imputed1 has 743 columns, ~3200 columns dropped due to using a small
  # sample of sites and times
  # dt_imputed_sub <- dt_imputed2[, names(dt_imputed1)]
  # testthat::expect_equal(dim(dt_imputed_sub), c(57, 743))
  # testthat::expect_no_error(
  #   dt_append2 <- append_predecessors(
  #     path_qs = append_directory,
  #     period_new = c("2018-01-11", "2018-01-11"),
  #     input_new = dt_imputed_sub,
  #     nthreads = 1
  #   )
  # )
  # # expect data.frame
  # testthat::expect_s3_class(dt_append2, "data.frame")
  # # expect 33 rows and 743 columns (columns dropped due to NA in sample)
  # testthat::expect_equal(dim(dt_append2), c(90, 743))
  # # expect 10 dates
  # testthat::expect_length(unique(dt_append2$time), 20)


  # expect error with NULL `input_new`
  testthat::expect_error(
    append_predecessors()
  )
})
