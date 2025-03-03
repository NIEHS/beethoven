################################################################################
##### unit and integration tests for meta learner functions
##### main files: R/meta_learner.R

################################################################################
##### attach_pred
testthat::test_that("attach_pred", {
  # import sample data
  fit_learner_base_mlp <- readRDS(testthat::test_path("..", "testdata", "meta", "mlp.rds"))
  fit_learner_base_xgb <- readRDS(testthat::test_path("..", "testdata", "meta", "xgb.rds"))
  fit_learner_base_elnet <- readRDS(testthat::test_path("..", "testdata", "meta", "elnet.rds"))
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  target_cols <- c("site_id", "time", "Event.Type", "lon", "lat")
  # expect no error on attaching predictions to base
  testthat::expect_no_error(
    dt_meta <- attach_pred(
      data = dt_base,
      pred = list(
        fit_learner_base_mlp = fit_learner_base_mlp,
        fit_learner_base_xgb = fit_learner_base_mlp,
        fit_learner_base_elnet = fit_learner_base_mlp
      ),
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
##### fit_meta_learner (spatiotemporal)
testthat::test_that("fit_meta_learner (spatiotemporal)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_meta <- readRDS(
    testthat::test_path("..", "testdata", "meta", "dt_meta.rds")
  )

  # expect warning for spatiotemporal folds
  testthat::expect_warning(
    meta1 <- fit_meta_learner(
      data = data.table::data.table(dt_meta),
      c_subsample = 1.0,
      r_subsample = 1.0,
      yvar = "Arithmetic.Mean",
      target_cols = c("site_id", "time", "Event.Type", "lon", "lat"),
      args_generate_cv = list(v = 3),
      tune_iter = 1L,
      trim_resamples = FALSE,
      return_best = TRUE,
      metric = "rmse"
    )
  )
  # expect a list
  testthat::expect_true(is.list(meta1))
  # expect 4 items in the list
  testthat::expect_length(meta1, 4)
  # expect first item is a tibble data.frame
  testthat::expect_true("tbl_df" %in% class(meta1[[1]]))
  # expect third item is tune results
  testthat::expect_true("tune_results" %in% class(meta1[[3]]))
  # expect fourth item is workflow
  testthat::expect_true("workflow" %in% class(meta1[[4]]))

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
      c_subsample = 0.5,
      r_subsample = 1.0,
      yvar = "Arithmetic.Mean",
      target_cols = c("site_id", "time", "lon", "lat", "Event.Type"),
      args_generate_cv = list(),
      tune_iter = 50L,
      trim_resamples = FALSE,
      return_best = TRUE,
      metric = "rmse"
    )
  )

  # expect error with missing yvar
  testthat::expect_error(
    fit_meta_learner(
      data = dt_meta,
      c_subsample = 0.5,
      r_subsample = 1.0,
      yvar = NULL,
      target_cols = c("site_id", "time", "lon", "lat", "Event.Type"),
      args_generate_cv = list(),
      tune_iter = 50L,
      trim_resamples = FALSE,
      return_best = TRUE,
      metric = "rmse"
    )
  )
  
  # expect error with missing yvar
  testthat::expect_error(
    fit_meta_learner(
      data = dt_meta,
      c_subsample = 0.5,
      r_subsample = 1.0,
      yvar = "Arithmetic.Mean",
      target_cols = NULL,
      args_generate_cv = list(),
      tune_iter = 50L,
      trim_resamples = FALSE,
      return_best = TRUE,
      metric = "rmse"
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

  # expect no error for meta fit
  testthat::expect_warning(
    meta4 <- fit_meta_learner(
      data.table::data.table(dt_meta),
      c_subsample = 1.0,
      r_subsample = 1.0,
      yvar = "Arithmetic.Mean",
      target_cols = c("site_id", "time", "Event.Type", "lon", "lat"),
      args_generate_cv = list(v = 3),
      tune_iter = 1L,
      trim_resamples = FALSE,
      return_best = TRUE,
      metric = "rmse"
    )
  )

  # import new prediction data
  dt_pred <- readRDS(
    testthat::test_path("..", "testdata", "meta", "dt_pred.rds")
  )

  # # expect no error for prediction
  # testthat::expect_no_error(
  #   pred1 <- predict_meta_learner(
  #     meta_fitted = meta4[[1]],
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
