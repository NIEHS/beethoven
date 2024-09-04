################################################################################
##### unit and integration tests for neural network base learner functions
##### main files: R/base_learner.R

################################################################################
##### folds + grid tuning
testthat::test_that("fit mlp (folds + grid))", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )


  # set model
  mlp_model <- switch_model("mlp")
  # set grid
  mlp_grid <- expand.grid(
    hidden_units = list(16, c(8, 8), c(8, 16, 32)),
    dropout = c(0.2, 0.3333333),
    activation = c("relu"),
    learn_rate = c(0.1)
   )


  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    mlp1 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
      folds = 5L,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
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
  testthat::expect_true(is.list(mlp1))
  # expect length 3
  testthat::expect_length(mlp1, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp1[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp1$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp1$base_prediction$.pred)) > 1)


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    mlp2 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
      folds = 5L,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
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
  testthat::expect_true(is.list(mlp2))
  # expect length 3
  testthat::expect_length(mlp2, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp2[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp2$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp2$base_prediction$.pred)) > 1)


  # spatiotemporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    mlp3 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
      folds = 5L,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
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
  testthat::expect_true(is.list(mlp3))
  # expect length 3
  testthat::expect_length(mlp3, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp3[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp3$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp3$base_prediction$.pred)) > 1)

})


################################################################################
##### folds + bayes tuning
testthat::test_that("fit mlp (folds + bayes)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )


  # set model
  mlp_model <- switch_model("mlp")


  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    mlp4 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
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
  testthat::expect_true(is.list(mlp4))
  # expect length 3
  testthat::expect_length(mlp4, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp4[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp4$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp4$base_prediction$.pred)) > 1)


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    mlp5 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
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
  testthat::expect_true(is.list(mlp5))
  # expect length 3
  testthat::expect_length(mlp5, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp5[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp5$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp5$base_prediction$.pred)) > 1)


  # spatiotemporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  testthat::expect_warning(
    mlp6 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
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
  testthat::expect_true(is.list(mlp6))
  # expect length 3
  testthat::expect_length(mlp6, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp6[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp6$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp6$base_prediction$.pred)) > 1)

})


################################################################################
##### args_generate_cv + grid tuning
testthat::test_that("fit mlp (args_generate_cv + grid)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )


  # set model
  mlp_model <- switch_model("mlp")
  # set grid
  mlp_grid <- expand.grid(
    hidden_units = list(16, c(8, 8), c(8, 16, 32)),
    dropout = c(0.2, 0.3333333),
    activation = c("relu"),
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
    mlp7 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
      args_generate_cv = args_temp,
      folds = NULL,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
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
  testthat::expect_true(is.list(mlp7))
  # expect length 3
  testthat::expect_length(mlp7, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp7[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp7$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp7$base_prediction$.pred)) > 1)


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  testthat::expect_warning(
    mlp8 <- fit_base_learner(
      learner = "mlp",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 0.3,
      model = mlp_model,
      args_generate_cv = args_spatial,
      folds = NULL,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
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
  testthat::expect_true(is.list(mlp8))
  # expect length 3
  testthat::expect_length(mlp8, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp8[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp8$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp8$base_prediction$.pred)) > 1)


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
    mlp9 <- fit_base_learner(
      learner = "mlp",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 1, # full sample dataset for accurate ngroup cv
      model = mlp_model,
      args_generate_cv = args_spatiotemporal,
      folds = NULL,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
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
  testthat::expect_true(is.list(mlp9))
  # expect length 3
  testthat::expect_length(mlp9, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp9[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp9$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp9$base_prediction$.pred)) > 1)

})


################################################################################
##### args_generate_cv + bayes tuning
testthat::test_that("fit mlp (args_generate_cv + bayes)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )


  # set model
  mlp_model <- switch_model("mlp")


  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_warning(
    mlp10 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
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
  testthat::expect_true(is.list(mlp10))
  # expect length 3
  testthat::expect_length(mlp10, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp10[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp10$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp10$base_prediction$.pred)) > 1)


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  testthat::expect_warning(
    mlp11 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
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
  testthat::expect_true(is.list(mlp11))
  # expect length 3
  testthat::expect_length(mlp11, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp11[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp11$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp11$base_prediction$.pred)) > 1)


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
    mlp12 <- fit_base_learner(
      learner = "mlp",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 1, # full sample dataset for accurate ngroup cv
      model = mlp_model,
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
  testthat::expect_true(is.list(mlp12))
  # expect length 3
  testthat::expect_length(mlp12, 2) # LENGTH 2 DUE TO UPDATED TRIM
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:2, function(x) methods::is(mlp12[[x]], "tbl_df"))),
    c(TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp12$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(length(unique(mlp12$base_prediction$.pred)) > 1)
  # expect NA only in base performance splits due to trim
  # testthat::expect_equal(unique(mlp12$best_performance[[1]]), NA)

})