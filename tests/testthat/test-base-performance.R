################################################################################
##### performance tests for base learner functions
##### models, tuning parameters, and cross-validation methods are derived
##### from full pipeline run
##### main files: R/base_learner.R

################################################################################
##### elnet
testthat::test_that("fit elnet (performance)", {
  # import sample data
  # sample inlcudes 1 year of data for 50 sites
  # subset to only 200 predictors for light weight
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # set model
  elnet_model <- switch_model("elnet")
  # set grid
  elnet_grid <- expand.grid(
    mixture = c(0.4, 0.5, 0.6, 0.7, 0.8),
    penalty = c(0.1, 0.2)
  )

  # temporal
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_no_error(
    elnet_p1 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_performance,
      r_subsample = 0.3,
      model = elnet_model,
      folds = NULL,
      args_generate_cv = args_temp,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = elnet_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_performance)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet_p1$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% elnet_p1$base_prediction$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(elnet_p1$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  elnet_p1_mean <- mean(elnet_p1$base_prediction$.pred)
  testthat::expect_true(elnet_p1_mean >= 6 && elnet_p1_mean <= 9)
  # expect SD ~= 7 (> 3 and < 10)
  elnet_p1_sd <- sd(elnet_p1$base_prediction$.pred)
  testthat::expect_true(elnet_p1_sd >= 3 && elnet_p1_sd <= 10)


  # spatial
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  # warning is due to dt_base not having CRS
  testthat::expect_warning(
    elnet_p2 <- fit_base_learner(
      learner = "elnet",
      dt_full = data.table::data.table(dt_performance),
      r_subsample = 0.3,
      model = elnet_model,
      folds = NULL,
      args_generate_cv = args_spatial,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = elnet_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_performance)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet_p2$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% elnet_p2$base_prediction$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(elnet_p2$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  elnet_p2_mean <- mean(elnet_p2$base_prediction$.pred)
  testthat::expect_true(elnet_p2_mean >= 6 && elnet_p2_mean <= 9)
  # expect SD ~= 7 (> 3 and < 10)
  elnet_p2_sd <- sd(elnet_p2$base_prediction$.pred)
  testthat::expect_true(elnet_p2_sd >= 3 && elnet_p2_sd <= 10)


  # spatiotemporal
  args_spatiotemporal <- list(
    target_cols = c("lon", "lat", "time"),
    preprocessing = "none",
    ngroup_init = 2L,
    cv_pairs = NULL,
    pairing = "1"
  )
  testthat::expect_no_error(
    elnet_p3 <- fit_base_learner(
      learner = "elnet",
      dt_full = data.table::data.table(dt_performance),
      r_subsample = 0.3,
      model = elnet_model,
      folds = NULL,
      args_generate_cv = args_spatiotemporal,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = elnet_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_performance)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet_p3$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% elnet_p3$base_prediction$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(elnet_p3$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  elnet_p3_mean <- mean(elnet_p3$base_prediction$.pred)
  testthat::expect_true(elnet_p3_mean >= 6 && elnet_p3_mean <= 9)
  # expect SD ~= 7 (> 3 and < 10)
  elnet_p3_sd <- sd(elnet_p3$base_prediction$.pred)
  testthat::expect_true(elnet_p3_sd >= 3 && elnet_p3_sd <= 10)

})


################################################################################
##### lightgbm
testthat::test_that("fit lightgbm (performance)", {
  withr::local_package("bonsai")
  # import sample data
  # sample inlcudes 1 year of data for 50 sites
  # subset to only 200 predictors for light weight
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # set model
  lgb_model <- switch_model("lgb", device = "cpu")
  # set grid
  lgb_grid <- expand.grid(
    mtry = floor(c(0.025, seq(0.05, 0.2, 0.05)) * ncol(dt_performance)),
    trees = seq(100, 200, 100),
    learn_rate = c(0.3)
  )

  # temporal
  args_temp <- list(
    time_col = "time",
    cv_fold = 3L,
    window = 5L
  )
  testthat::expect_no_error(
    lgb_p1 <- fit_base_learner(
      learner = "lgb",
      dt_full = dt_performance,
      r_subsample = 0.3,
      model = lgb_model,
      folds = NULL,
      args_generate_cv = args_temp,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = lgb_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_performance)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(lgb_p1$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% lgb_p1$base_prediction$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(lgb_p1$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  lgb_p1_mean <- mean(lgb_p1$base_prediction$.pred)
  testthat::expect_true(lgb_p1_mean >= 6 && lgb_p1_mean <= 9)
  # expect SD ~= 7 (> 3 and < 10)
  lgb_p1_sd <- sd(lgb_p1$base_prediction$.pred)
  testthat::expect_true(lgb_p1_sd >= 3 && lgb_p1_sd <= 10)


  # spatial
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  # warning is due to dt_base not having CRS
  testthat::expect_warning(
    lgb_p2 <- fit_base_learner(
      learner = "lgb",
      dt_full = dt_performance,
      r_subsample = 0.3,
      model = lgb_model,
      folds = NULL,
      args_generate_cv = args_spatial,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = lgb_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_performance)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(lgb_p2$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% lgb_p2$base_prediction$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(lgb_p2$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  lgb_p2_mean <- mean(lgb_p2$base_prediction$.pred)
  testthat::expect_true(lgb_p2_mean >= 6 && lgb_p2_mean <= 9)
  # expect SD ~= 7 (> 3 and < 10)
  lgb_p2_sd <- sd(lgb_p2$base_prediction$.pred)
  testthat::expect_true(lgb_p2_sd >= 3 && lgb_p2_sd <= 10)


  # spatiotemporal
  args_spatiotemporal <- list(
    target_cols = c("lon", "lat", "time"),
    preprocessing = "none",
    ngroup_init = 2L,
    cv_pairs = NULL,
    pairing = "1"
  )
  testthat::expect_no_error(
    lgb_p3 <- fit_base_learner(
      learner = "lgb",
      dt_full = data.table::data.table(dt_performance),
      r_subsample = 0.3,
      model = lgb_model,
      folds = NULL,
      args_generate_cv = args_spatiotemporal,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = lgb_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_performance)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(lgb_p3$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% lgb_p3$base_prediction$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(lgb_p3$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  lgb_p3_mean <- mean(lgb_p3$base_prediction$.pred)
  testthat::expect_true(lgb_p3_mean >= 6 && lgb_p3_mean <= 9)
  # expect SD ~= 7 (> 3 and < 10)
  lgb_p3_sd <- sd(lgb_p3$base_prediction$.pred)
  testthat::expect_true(lgb_p3_sd >= 3 && lgb_p3_sd <= 10)
  
})


################################################################################
##### mlp
testthat::test_that("fit mlp (performance)", {
  testthat::skip_on_ci()
  # import sample data
  # sample inlcudes 1 year of data for 50 sites
  # subset to only 200 predictors for light weight
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # set model
  mlp_model <- switch_model("mlp", device = "cpu")
  # set grid
  mlp_grid <- expand.grid(
    hidden_units = list(c(16, 16)),
    dropout = c(0.5),
    activation = c("relu"),
    learn_rate = c(0.1)
  )

  # temporal
  args_temp <- list(
    time_col = "time",
    cv_fold = 3L,
    window = 5L
  )
  testthat::expect_no_error(
    mlp_p1 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_performance,
      r_subsample = 0.3,
      model = mlp_model,
      args_generate_cv = args_temp,
      folds = NULL,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_performance)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp_p1$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% mlp_p1$base_prediction$.pred)
  # expect non-repetative predictions
  # testthat::expect_true(length(unique(mlp_p1$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  mlp_p1_mean <- mean(mlp_p1$base_prediction$.pred)
  # testthat::expect_true(mlp_p1_mean >= 6 && mlp_p1_mean <= 9)
  # expect SD ~= 2 (> 0 and < 2)
  mlp_p1_sd <- sd(mlp_p1$base_prediction$.pred)
  # testthat::expect_true(mlp_p1_sd > 0 && mlp_p1_sd <= 2)


  # spatial
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  # warning is due to dt_base not having CRS
  testthat::expect_warning(
    mlp_p2 <- fit_base_learner(
      learner = "mlp",
      dt_full = data.table::data.table(dt_performance),
      r_subsample = 0.3,
      model = mlp_model,
      args_generate_cv = args_spatial,
      folds = NULL,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_performance)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp_p2$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% mlp_p2$base_prediction$.pred)
  # expect non-repetative predictions
  # testthat::expect_true(length(unique(mlp_p2$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  mlp_p2_mean <- mean(mlp_p2$base_prediction$.pred)
  # testthat::expect_true(mlp_p2_mean >= 6 && mlp_p2_mean <= 9)
  # expect SD ~= 2 (> 0 and < 2)
  mlp_p2_sd <- sd(mlp_p2$base_prediction$.pred)
  # testthat::expect_true(mlp_p2_sd > 0 && mlp_p2_sd <= 2)


  # spatiotemporal
  args_spatiotemporal <- list(
    target_cols = c("lon", "lat", "time"),
    preprocessing = "none",
    ngroup_init = 4L,
    cv_pairs = NULL,
    pairing = "1"
  )
  testthat::expect_no_error(
    mlp_p3 <- fit_base_learner(
      learner = "mlp",
      dt_full = data.table::data.table(dt_performance),
      r_subsample = 0.3,
      model = mlp_model,
      args_generate_cv = args_spatiotemporal,
      folds = NULL,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_performance)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp_p3$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% mlp_p3$base_prediction$.pred)
  # expect non-repetative predictions
  # testthat::expect_true(length(unique(mlp_p3$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  mlp_p3_mean <- mean(mlp_p3$base_prediction$.pred)
  # testthat::expect_true(mlp_p3_mean >= 6 && mlp_p3_mean <= 9)
  # expect SD ~= 2 (> 0 and < 2)
  mlp_p3_sd <- sd(mlp_p3$base_prediction$.pred)
  # testthat::expect_true(mlp_p3_sd > 0 && mlp_p3_sd <= 2)

})


################################################################################
##### xgboost
# testthat::test_that("fit xgboost (performance)", {
#   # import sample data
#   # sample inlcudes 1 year of data for 50 sites
#   # subset to only 200 predictors for light weight
#   dt_performance <- readRDS(
#     testthat::test_path("..", "testdata", "base", "dt_performance.rds")
#   )

#   # set model
#   xgb_model <- switch_model("xgb")
#   # set grid
#   xgb_grid <- expand.grid(
#     mtry = floor(c(0.025, seq(0.05, 0.2, 0.05)) * 2000L),
#     trees = seq(1000, 3000, 1000),
#     learn_rate = c(0.1, 0.05, 0.01, 0.005)
#   )

#   # temporal
#   args_temp <- list(
#     time_col = "time",
#     cv_fold = 10L,
#     window = 5L
#   )
#   testthat::expect_no_error(
#     xgb_p1 <- fit_base_learner(
#       learner = "xgb",
#       dt_full = dt_performance,
#       r_subsample = 0.3,
#       model = xgb_model,
#       folds = NULL,
#       args_generate_cv = args_temp,
#       cv_mode = "temporal",
#       tune_mode = "grid",
#       tune_grid_in = xgb_grid,
#       tune_grid_size = 10,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_performance)),
#       nthreads = 1,
#       trim_resamples = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(xgb_p1$base_prediction$.pred))
#   # expect no NA values
#   testthat::expect_false(NA %in% xgb_p1$base_prediction$.pred)
#   # expect non-repetative predictions
#   testthat::expect_true(length(unique(xgb_p1$base_prediction$.pred)) > 1)
#   # expect mean ~= 7.76 (observed mean)
#   xgb_p1_mean <- mean(xgb_p1$base_prediction$.pred)
#   testthat::expect_true(xgb_p1_mean >= 6 && xgb_p1_mean <= 9)
#   # expect SD ~= 7 (> 3 and < 10)
#   xgb_p1_sd <- sd(xgb_p1$base_prediction$.pred)
#   testthat::expect_true(xgb_p1_sd >= 3 && xgb_p1_sd <= 10)


#   # spatial
#   args_spatial = list(
#     target_cols = c("lon", "lat"),
#     cv_make_fun = spatialsample::spatial_block_cv,
#     v = 4
#   )
#   # warning is due to dt_base not having CRS
#   testthat::expect_warning(
#     xgb_p2 <- fit_base_learner(
#       learner = "xgb",
#       dt_full = dt_performance,
#       r_subsample = 0.3,
#       model = xgb_model,
#       folds = NULL,
#       args_generate_cv = args_spatial,
#       cv_mode = "spatial",
#       tune_mode = "grid",
#       tune_grid_in = xgb_grid,
#       tune_grid_size = 10,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_performance)),
#       nthreads = 1,
#       trim_resamples = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(xgb_p2$base_prediction$.pred))
#   # expect no NA values
#   testthat::expect_false(NA %in% xgb_p2$base_prediction$.pred)
#   # expect non-repetative predictions
#   testthat::expect_true(length(unique(xgb_p2$base_prediction$.pred)) > 1)
#   # expect mean ~= 7.76 (observed mean)
#   xgb_p2_mean <- mean(xgb_p2$base_prediction$.pred)
#   testthat::expect_true(xgb_p2_mean >= 6 && xgb_p2_mean <= 9)
#   # expect SD ~= 7 (> 3 and < 10)
#   xgb_p2_sd <- sd(xgb_p2$base_prediction$.pred)
#   testthat::expect_true(xgb_p2_sd >= 3 && xgb_p2_sd <= 10)


#   # spatiotemporal
#   args_spatiotemporal <- list(
#     target_cols = c("lon", "lat", "time"),
#     preprocessing = "none",
#     ngroup_init = 2L,
#     cv_pairs = NULL,
#     pairing = "1"
#   )
#   testthat::expect_no_error(
#     xgb_p3 <- fit_base_learner(
#       learner = "xgb",
#       dt_full = data.table::data.table(dt_performance),
#       r_subsample = 0.3,
#       model = xgb_model,
#       folds = NULL,
#       args_generate_cv = args_spatiotemporal,
#       cv_mode = "spatiotemporal",
#       tune_mode = "grid",
#       tune_grid_in = xgb_grid,
#       tune_grid_size = 10,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_performance)),
#       nthreads = 1,
#       trim_resamples = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(xgb_p3$base_prediction$.pred))
#   # expect no NA values
#   testthat::expect_false(NA %in% xgb_p3$base_prediction$.pred)
#   # expect non-repetative predictions
#   testthat::expect_true(length(unique(xgb_p3$base_prediction$.pred)) > 1)
#   # expect mean ~= 7.76 (observed mean)
#   xgb_p3_mean <- mean(xgb_p3$base_prediction$.pred)
#   testthat::expect_true(xgb_p3_mean >= 6 && xgb_p3_mean <= 9)
#   # expect SD ~= 7 (> 3 and < 10)
#   xgb_p3_sd <- sd(xgb_p3$base_prediction$.pred)
#   testthat::expect_true(xgb_p3_sd >= 3 && xgb_p3_sd <= 10)
  
# })
