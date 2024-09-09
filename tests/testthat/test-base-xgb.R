################################################################################
##### unit and integration tests for xgboost base learner functions
##### main files: R/base_learner.R

################################################################################
##### folds + grid tuning
testthat::test_that("fit xgboost (folds + grid)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  xgb_model <- switch_model("xgb")
  # set grid
  xgb_grid <- expand.grid(
    mtry = c(300),
    trees = seq(1000, 3000, 1000),
    learn_rate = c(0.1)
  )

  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    xgb1 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = 5L,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = xgb_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb1))
  # expect length 3
  testthat::expect_length(xgb1, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb1[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb1$base_prediction$.pred))


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    xgb2 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = 5L,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = xgb_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb2))
  # expect length 3
  testthat::expect_length(xgb2, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb2[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb2$base_prediction$.pred))


  # spatiotemporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    xgb3 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = 5L,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = xgb_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb3))
  # expect length 3
  testthat::expect_length(xgb3, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb3[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb3$base_prediction$.pred))
  
})


################################################################################
##### folds + bayes tuning
testthat::test_that("fit xgboost (folds + bayes)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  xgb_model <- switch_model("xgb")

  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    xgb4 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = 5L,
      cv_mode = "temporal",
      tune_mode = "bayes",
      tune_bayes_iter = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb4))
  # expect length 3
  testthat::expect_length(xgb4, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb4[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb4$base_prediction$.pred))


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    xgb5 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = 5L,
      cv_mode = "spatial",
      tune_mode = "bayes",
      tune_bayes_iter = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb5))
  # expect length 3
  testthat::expect_length(xgb5, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb5[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb5$base_prediction$.pred))


  # spatiotemporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    xgb6 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = 5L,
      cv_mode = "spatiotemporal",
      tune_mode = "bayes",
      tune_bayes_iter = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb6))
  # expect length 3
  testthat::expect_length(xgb6, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb6[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb6$base_prediction$.pred))
  
})


################################################################################
##### args_generate_cv + grid tuning
testthat::test_that("fit xgboost (args_generate_cv + grid)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  xgb_model <- switch_model("xgb")
  # set grid
  xgb_grid <- expand.grid(
    mtry = c(300),
    trees = seq(1000, 3000, 1000),
    learn_rate = c(0.1)
  )

  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_warning(
    xgb7 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = NULL,
      args_generate_cv = args_temp,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = xgb_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb7))
  # expect length 3
  testthat::expect_length(xgb7, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb7[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb7$base_prediction$.pred))


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  testthat::expect_warning(
    xgb8 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = NULL,
      args_generate_cv = args_spatial,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = xgb_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb8))
  # expect length 3
  testthat::expect_length(xgb8, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb8[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb8$base_prediction$.pred))


  # spatiotemporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_spatiotemporal <- list(
    target_cols = c("lon", "lat", "time"),
    preprocessing = "none",
    ngroup_init = 2L,
    cv_pairs = NULL,
    pairing = "1"
  )
  testthat::expect_warning(
    xgb9 <- fit_base_learner(
      learner = "xgb",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 1, # full sample dataset for accurate ngroup cv
      model = xgb_model,
      folds = NULL,
      args_generate_cv = args_spatiotemporal,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = xgb_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb9))
  # expect length 3
  testthat::expect_length(xgb9, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb9[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb9$base_prediction$.pred))
  
})


################################################################################
##### args_generate_cv + bayes tuning
testthat::test_that("fit xgboost (args_generate_cv + bayes)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  xgb_model <- switch_model("xgb")

  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_warning(
    xgb10 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = NULL,
      args_generate_cv = args_temp,
      cv_mode = "temporal",
      tune_mode = "bayes",
      tune_bayes_iter = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb10))
  # expect length 3
  testthat::expect_length(xgb10, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb10[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb10$base_prediction$.pred))


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  testthat::expect_warning(
    xgb11 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = NULL,
      args_generate_cv = args_spatial,
      cv_mode = "spatial",
      tune_mode = "bayes",
      tune_bayes_iter = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb11))
  # expect length 3
  testthat::expect_length(xgb11, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(xgb11[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb11$base_prediction$.pred))


  # spatiotemporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_spatiotemporal <- list(
    target_cols = c("lon", "lat", "time"),
    preprocessing = "none",
    ngroup_init = 2L,
    cv_pairs = NULL,
    pairing = "1"
  )
  testthat::expect_warning(
    xgb12 <- fit_base_learner(
      learner = "xgb",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 1, # full sample dataset for accurate ngroup cv
      model = xgb_model,
      folds = NULL,
      args_generate_cv = args_spatiotemporal,
      cv_mode = "spatiotemporal",
      tune_mode = "bayes",
      tune_bayes_iter = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = TRUE, # trim samples
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(xgb12))
  # expect length 3
  testthat::expect_length(xgb12, 2) # LENGTH 2 DUE TO UPDATED TRIM
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:2, function(x) methods::is(xgb12[[x]], "tbl_df"))),
    c(TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb12$base_prediction$.pred))

})
