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
    mixture = seq(0, 1, length.out = 21),
    penalty = 10 ^ seq(-3, 5, 1)
  )

  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_warning(
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
  testthat::expect_true(
    mean(elnet_p1$base_prediction$.pred) >= 6 &&
    mean(elnet_p1$base_prediction$.pred) <= 9
  )
  # expect SD ~= 7 (> 3 and < 10)
  testthat::expect_true(
    sd(elnet_p1$base_prediction$.pred) >= 3 &&
    sd(elnet_p1$base_prediction$.pred) <= 10
  )


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
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
  testthat::expect_true(
    mean(elnet_p2$base_prediction$.pred) >= 6 &&
    mean(elnet_p2$base_prediction$.pred) <= 9
  )
  # expect SD ~= 7 (> 3 and < 10)
  testthat::expect_true(
    sd(elnet_p2$base_prediction$.pred) >= 3 &&
    sd(elnet_p2$base_prediction$.pred) <= 10
  )


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
    elnet_p3 <- fit_base_learner(
      learner = "elnet",
      dt_full = data.table::data.table(dt_performance),
      r_subsample = 0.5, # full sample dataset for accurate ngroup cv
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
  # expect mean ~= 7.76 (observed mean of performance data)
  testthat::expect_true(
    mean(elnet_p3$base_prediction$.pred) >= 6 &&
    mean(elnet_p3$base_prediction$.pred) <= 9
  )
  # expect SD ~= 7 (> 3 and < 10)
  testthat::expect_true(
    sd(elnet_p3$base_prediction$.pred) >= 3 &&
    sd(elnet_p3$base_prediction$.pred) <= 10
  )

})


################################################################################
##### lightGBM
# testthat::test_that("fit lightgbm (performance)", {
#   # import sample data
#   # sample inlcudes 1 year of data for 50 sites
#   # subset to only 200 predictors for light weight
#   dt_performance <- readRDS(
#     testthat::test_path("..", "testdata", "base", "dt_performance.rds")
#   )

#   # set model
#   lgb_model <- switch_model("lgb")
#   # set grid
#   lgb_grid <- expand.grid(
#     mtry = floor(c(0.025, seq(0.05, 0.2, 0.05)) * 2000L),
#     trees = seq(1000, 3000, 1000),
#     learn_rate = c(0.1, 0.05, 0.01, 0.005)
#   )

#   # temporal
#   # warning is due to 3 metrics (rmse, rsq, mae)
#   args_temp <- list(
#     time_col = "time",
#     cv_fold = 10L,
#     window = 5L
#   )
#   testthat::expect_warning(
#     lgb_p1 <- fit_base_learner(
#         learner = "lgb",
#         dt_full = dt_base,
#         r_subsample = 0.3,
#         model = lgb_model,
#         folds = NULL,
#         args_generate_cv = args_temp,
#         cv_mode = "temporal",
#         tune_mode = "grid",
#         tune_grid_in = lgb_grid,
#         tune_grid_size = 10,
#         learn_rate = 0.1,
#         yvar = "Arithmetic.Mean",
#         xvar = seq(5, ncol(dt_base)),
#         nthreads = 1,
#         trim_resamples = FALSE,
#         return_best = TRUE
#     )
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(lgb_p1$base_prediction$.pred))
#   # expect no NA values
#   testthat::expect_false(NA %in% lgb_p1$base_prediction$.pred)
#   # expect non-repetative predictions
#   testthat::expect_true(length(unique(lgb_p1$base_prediction$.pred)) > 1)
#   # expect mean ~= 7.76 (observed mean of performance data)
#   testthat::expect_true(
#     mean(lgb_p1$base_prediction$.pred) >= 6 &&
#     mean(lgb_p1$base_prediction$.pred) <= 9
#   )
#   # expect SD ~= 7 (> 3 and < 10)
#   testthat::expect_true(
#     sd(lgb_p1$base_prediction$.pred) >= 3 &&
#     sd(lgb_p1$base_prediction$.pred) <= 10
#   )


#   # spatial
#   # warning is due to 3 metrics (rmse, rsq, mae)
#   args_spatial = list(
#     target_cols = c("lon", "lat"),
#     cv_make_fun = spatialsample::spatial_block_cv,
#     v = 4
#   )
#   testthat::expect_warning(
#     lgb_p2 <- fit_base_learner(
#         learner = "lgb",
#         dt_full = dt_base,
#         r_subsample = 0.3,
#         model = lgb_model,
#         folds = NULL,
#         args_generate_cv = args_spatial,
#         cv_mode = "spatial",
#         tune_mode = "grid",
#         tune_grid_in = lgb_grid,
#         tune_grid_size = 10,
#         learn_rate = 0.1,
#         yvar = "Arithmetic.Mean",
#         xvar = seq(5, ncol(dt_base)),
#         nthreads = 1,
#         trim_resamples = FALSE,
#         return_best = TRUE
#     )
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(lgb_p2$base_prediction$.pred))
#   # expect no NA values
#   testthat::expect_false(NA %in% lgb_p2$base_prediction$.pred)
#   # expect non-repetative predictions
#   testthat::expect_true(length(unique(lgb_p2$base_prediction$.pred)) > 1)
#   # expect mean ~= 7.76 (observed mean of performance data)
#   testthat::expect_true(
#     mean(lgb_p2$base_prediction$.pred) >= 6 &&
#     mean(lgb_p2$base_prediction$.pred) <= 9
#   )
#   # expect SD ~= 7 (> 3 and < 10)
#   testthat::expect_true(
#     sd(lgb_p2$base_prediction$.pred) >= 3 &&
#     sd(lgb_p2$base_prediction$.pred) <= 10
#   )


#   # spatiotemporal
#   # warning is due to 3 metrics (rmse, rsq, mae)
#   args_spatiotemporal <- list(
#     target_cols = c("lon", "lat", "time"),
#     preprocessing = "none",
#     ngroup_init = 2L,
#     cv_pairs = NULL,
#     pairing = "1"
#   )
#   testthat::expect_warning(
#     lgb_p3 <- fit_base_learner(
#         learner = "lgb",
#         dt_full = data.table::data.table(dt_base),
#         r_subsample = 0.5, # full sample dataset for accurate ngroup cv
#         model = lgb_model,
#         folds = NULL,
#         args_generate_cv = args_spatiotemporal,
#         cv_mode = "spatiotemporal",
#         tune_mode = "grid",
#         tune_grid_in = lgb_grid,
#         tune_grid_size = 10,
#         learn_rate = 0.1,
#         yvar = "Arithmetic.Mean",
#         xvar = seq(5, ncol(dt_base)),
#         nthreads = 1,
#         trim_resamples = FALSE,
#         return_best = TRUE
#     )
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(lgb_p3$base_prediction$.pred))
#   # expect no NA values
#   testthat::expect_false(NA %in% lgb_p3$base_prediction$.pred)
#   # expect non-repetative predictions
#   testthat::expect_true(length(unique(lgb_p3$base_prediction$.pred)) > 1)
#   # expect mean ~= 7.76 (observed mean of performance data)
#   testthat::expect_true(
#     mean(lgb_p3$base_prediction$.pred) >= 6 &&
#     mean(lgb_p3$base_prediction$.pred) <= 9
#   )
#   # expect SD ~= 7 (> 3 and < 10)
#   testthat::expect_true(
#     sd(lgb_p3$base_prediction$.pred) >= 3 &&
#     sd(lgb_p3$base_prediction$.pred) <= 10
#   )
  
# })


################################################################################
##### brulee MLP
testthat::test_that("fit mlp (performance)", {
  # import sample data
  # sample inlcudes 1 year of data for 50 sites
  # subset to only 200 predictors for light weight
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # set model
  mlp_model <- switch_model("mlp")
  # set grid
  mlp_grid <- expand.grid(
    hidden_units = list(
      c(256, 256), c(256, 512),
      c(512, 512), c(512, 1024),
      c(256, 512, 1024)
    ),
    dropout = 1 / seq(5, 2, -1),
    activation = c("relu", "leaky_relu"),
    learn_rate = c(0.1, 0.05, 0.01, 0.005)
  )

  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_warning(
    mlp_p1 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
      args_generate_cv = args_temp,
      folds = NULL,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
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
  testthat::expect_true(length(unique(mlp_p1$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean of performance data)
  testthat::expect_true(
    mean(mlp_p1$base_prediction$.pred) >= 6 &&
    mean(mlp_p1$base_prediction$.pred) <= 9
  )
  # expect SD ~= 7 (> 3 and < 10)
  testthat::expect_true(
    sd(mlp_p1$base_prediction$.pred) >= 3 &&
    sd(mlp_p1$base_prediction$.pred) <= 10
  )


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  testthat::expect_warning(
    mlp_p2 <- fit_base_learner(
      learner = "mlp",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 0.3,
      model = mlp_model,
      args_generate_cv = args_spatial,
      folds = NULL,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
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
  testthat::expect_true(length(unique(mlp_p2$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean of performance data)
  testthat::expect_true(
    mean(mlp_p2$base_prediction$.pred) >= 6 &&
    mean(mlp_p2$base_prediction$.pred) <= 9
  )
  # expect SD ~= 7 (> 3 and < 10)
  testthat::expect_true(
    sd(mlp_p2$base_prediction$.pred) >= 3 &&
    sd(mlp_p2$base_prediction$.pred) <= 10
  )


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
    mlp_p3 <- fit_base_learner(
      learner = "mlp",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 0.5, # full sample dataset for accurate ngroup cv
      model = mlp_model,
      args_generate_cv = args_spatiotemporal,
      folds = NULL,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
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
  testthat::expect_true(length(unique(mlp_p3$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean of performance data)
  testthat::expect_true(
    mean(mlp_p3$base_prediction$.pred) >= 6 &&
    mean(mlp_p3$base_prediction$.pred) <= 9
  )
  # expect SD ~= 7 (> 3 and < 10)
  testthat::expect_true(
    sd(mlp_p3$base_prediction$.pred) >= 3 &&
    sd(mlp_p3$base_prediction$.pred) <= 10
  )

})


################################################################################
##### XGBoost
testthat::test_that("fit xgboost (performance)", {
  # import sample data
  # sample inlcudes 1 year of data for 50 sites
  # subset to only 200 predictors for light weight
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # set model
  xgb_model <- switch_model("xgb")
  # set grid
  xgb_grid <- expand.grid(
    mtry = floor(c(0.025, seq(0.05, 0.2, 0.05)) * 2000L),
    trees = seq(1000, 3000, 1000),
    learn_rate = c(0.1, 0.05, 0.01, 0.005)
  )

  # temporal
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_warning(
    xgb_p1 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = NULL,
      args_generate_cv = args_temp,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = xgb_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb_p1$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% xgb_p1$base_prediction$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(xgb_p1$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean of performance data)
  testthat::expect_true(
    mean(xgb_p1$base_prediction$.pred) >= 6 &&
    mean(xgb_p1$base_prediction$.pred) <= 9
  )
  # expect SD ~= 7 (> 3 and < 10)
  testthat::expect_true(
    sd(xgb_p1$base_prediction$.pred) >= 3 &&
    sd(xgb_p1$base_prediction$.pred) <= 10
  )


  # spatial
  # warning is due to 3 metrics (rmse, rsq, mae)
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  testthat::expect_warning(
    xgb_p2 <- fit_base_learner(
      learner = "xgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = xgb_model,
      folds = NULL,
      args_generate_cv = args_spatial,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = xgb_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb_p2$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% xgb_p2$base_prediction$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(xgb_p2$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean of performance data)
  testthat::expect_true(
    mean(xgb_p2$base_prediction$.pred) >= 6 &&
    mean(xgb_p2$base_prediction$.pred) <= 9
  )
  # expect SD ~= 7 (> 3 and < 10)
  testthat::expect_true(
    sd(xgb_p2$base_prediction$.pred) >= 3 &&
    sd(xgb_p2$base_prediction$.pred) <= 10
  )


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
    xgb_p3 <- fit_base_learner(
      learner = "xgb",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 0.5, # full sample dataset for accurate ngroup cv
      model = xgb_model,
      folds = NULL,
      args_generate_cv = args_spatiotemporal,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = xgb_grid,
      tune_grid_size = 10,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(xgb_p3$base_prediction$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% xgb_p3$base_prediction$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(xgb_p3$base_prediction$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean of performance data)
  testthat::expect_true(
    mean(xgb_p3$base_prediction$.pred) >= 6 &&
    mean(mlp_p3$base_prediction$.pred) <= 9
  )
  # expect SD ~= 7 (> 3 and < 10)
  testthat::expect_true(
    sd(xgb_p3$base_prediction$.pred) >= 3 &&
    sd(xgb_p3$base_prediction$.pred) <= 10
  )
  
})
