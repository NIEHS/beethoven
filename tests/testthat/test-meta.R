################################################################################
##### unit and integration tests for meta learner functions
##### main files: R/meta_learner.R

################################################################################
##### attach_pred
testthat::test_that("attach_pred", {
  # import sample data
  mlp <- readRDS(testthat::test_path("..", "testdata", "meta", "mlp.rds"))
  xgb <- readRDS(testthat::test_path("..", "testdata", "meta", "xgb.rds"))
  elnet <- readRDS(testthat::test_path("..", "testdata", "meta", "elnet.rds"))
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  target_cols <- c("site_id", "time", "Event.Type", "lon", "lat")
  # expect no error on attaching predictions to base
  testthat::expect_no_error(
    dt_meta <- attach_pred(
      data = dt_base,
      pred = list(mlp, xgb, elnet),
      target_cols = target_cols,
      yvar = "Arithmetic.Mean"
    )
  )
  # expect data.frame
  testthat::expect_s3_class(dt_meta, "data.frame")
  # expect unique columns
  testthat::expect_equal(ncol(dt_meta), length(unique(names(dt_meta))))
  # expect important columns retained
  testthat::expect_true(
    all(c(target_cols, "Arithmetic.Mean") %in% names(dt_meta))
  )
  # expect 9 columns (target_cols + Arithmetic.Mean + 3 base learners)
  testthat::expect_length(names(dt_meta), 9)

})


################################################################################
##### fit_meta_learner (temporal)
testthat::test_that("fit_meta_learner (temporal)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_meta <- readRDS(
    testthat::test_path("..", "testdata", "meta", "dt_meta.rds")
  )

  # expect no error for rset preparation
  testthat::expect_no_error(
    index_ts <- generate_cv_index_ts(
      data = dt_meta,
      time_col = "time",
      cv_fold = 5L,
      window = 5L
    )
  )
  testthat::expect_no_error(
    rset_ts <- convert_cv_index_rset(
      cvindex = index_ts,
      data = dt_meta,
      cv_mode = "temporal"
    )
  )

  # expect no error for meta fit
  testthat::expect_warning(
    meta1 <- fit_meta_learner(
      data = dt_meta,
      p_col_sel = 0.5,
      rset = rset_ts,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_meta)),
      tune_iter = 1
    )
  )
  # expect a list
  testthat::expect_true(is.list(meta1))
  # expect 3 items in the list
  testthat::expect_length(meta1, 3)
  # expect first item is a workflow
  testthat::expect_true("workflow" %in% class(meta1[[1]]))
  # expect third item is tune results
  testthat::expect_true("tune_results" %in% class(meta1[[3]]))

})


################################################################################
##### fit_meta_learner (spatial)
testthat::test_that("fit_meta_learner (spatial)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_meta <- readRDS(
    testthat::test_path("..", "testdata", "meta", "dt_meta.rds")
  )

  # expect no error for rset preparation
  # expect CRS warning during indexing
  testthat::expect_warning(
    index_sp <- generate_cv_index_sp(
      data = dt_meta,
      target_cols = c("lon", "lat"),
      cv_make_fun = spatialsample::spatial_block_cv,
      v = 2
    )
  )
  testthat::expect_no_error(
    rset_sp <- convert_cv_index_rset(
      cvindex = index_sp,
      data = dt_meta,
      cv_mode = "spatial"
    )
  )

  # expect no error for meta fit
  testthat::expect_warning(
    meta2 <- fit_meta_learner(
      data = dt_meta,
      p_col_sel = 0.5,
      rset = rset_sp,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_meta)),
      tune_iter = 1
    )
  )
  # expect a list
  testthat::expect_true(is.list(meta2))
  # expect 3 items in the list
  testthat::expect_length(meta2, 3)
  # expect first item is a workflow
  testthat::expect_true("workflow" %in% class(meta2[[1]]))
  # expect third item is tune results
  testthat::expect_true("tune_results" %in% class(meta2[[3]]))

})


################################################################################
##### fit_meta_learner (spatiotemporal)
testthat::test_that("fit_meta_learner (spatiotemporal)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_meta <- readRDS(
    testthat::test_path("..", "testdata", "meta", "dt_meta.rds")
  )

  # expect no error for rset preparation
  testthat::expect_no_error(
    index_spt <- generate_cv_index_spt(
      data = data.table::data.table(dt_meta),
      target_cols = c("lon", "lat", "time"),
      preprocessing = "none",
      ngroup_init = 2L,
      cv_pairs = NULL,
      pairing = "1"
    )
  )
  testthat::expect_no_error(
    rset_spt <- convert_cv_index_rset(
      cvindex = index_spt,
      data = dt_meta,
      cv_mode = "spatiotemporal"
    )
  )

  # expect no error for meta fit
  testthat::expect_warning(
    meta3 <- fit_meta_learner(
      data = dt_meta,
      p_col_sel = 0.5,
      rset = rset_spt,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_meta)),
      tune_iter = 1
    )
  )
  # expect a list
  testthat::expect_true(is.list(meta3))
  # expect 3 items in the list
  testthat::expect_length(meta3, 3)
  # expect first item is a workflow
  testthat::expect_true("workflow" %in% class(meta3[[1]]))
  # expect third item is tune results
  testthat::expect_true("tune_results" %in% class(meta3[[3]]))

})


################################################################################
##### fit_meta_learner (errors)
testthat::test_that("fit_meta_learner (errors)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_meta <- readRDS(
    testthat::test_path("..", "testdata", "meta", "dt_meta.rds")
  )

  # expect error with missing data
  testthat::expect_error(
    fit_meta_learner(
      data = NULL,
      p_col_sel = 0.5,
      rset = NULL,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_meta)),
      tune_iter = 2
    )
  )

  # expect error with missing rset
  testthat::expect_error(
    fit_meta_learner(
      data = dt_meta,
      p_col_sel = 0.5,
      rset = NULL,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_meta)),
      tune_iter = 2
    )
  )

})


################################################################################
##### predict_meta_learner
testthat::test_that("predict_meta_learner", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_meta <- readRDS(
    testthat::test_path("..", "testdata", "meta", "dt_meta.rds")
  )

  # expect no error for rset preparation
  testthat::expect_no_error(
    index_spt <- generate_cv_index_spt(
      data = data.table::data.table(dt_meta),
      target_cols = c("lon", "lat", "time"),
      preprocessing = "none",
      ngroup_init = 2L,
      cv_pairs = NULL,
      pairing = "1"
    )
  )
  testthat::expect_no_error(
    rset_spt <- convert_cv_index_rset(
      cvindex = index_spt,
      data = dt_meta,
      cv_mode = "spatiotemporal"
    )
  )

  # expect no error for meta fit
  testthat::expect_no_error(
    meta4 <- fit_meta_learner(
      data = dt_meta,
      p_col_sel = 0.5,
      rset = rset_spt,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_meta)),
      tune_iter = 1
    )
  )

  # import new prediction data
  dt_pred <- readRDS(
    testthat::test_path("..", "testdata", "meta", "dt_pred.rds")
  )

  # # expect no error for prediction
  # testthat::expect_no_error(
  #   pred1 <- predict_meta_learner(
  #     meta_fitted = meta4$meta_fitted,
  #     new_data = dt_pred
  #   )
  # )
  # # expect tibble data.frame
  # testthat::expect_true(methods::is(pred1, "tbl_df"))
  # # expect 1 item
  # testthat::expect_length(pred1, 1)
  # # expect unique vlaues
  # testthat::expect_true(length(unique(pred1$.pred)) > 1)

})
