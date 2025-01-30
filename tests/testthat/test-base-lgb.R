################################################################################
##### unit and integration tests for lightGBM base learner functions
##### main files: R/base_learner.R

################################################################################
##### folds + grid tuning
testthat::test_that("fit lightgbm (folds + grid)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  lgb_model <- switch_model("lgb", device = "cpu")
  # set grid
  lgb_grid <- expand.grid(
    mtry = c(20),
    trees = c(1000),
    learn_rate = c(0.1)
  )

  # temporal
  testthat::expect_no_error(
    lgb1 <- fit_base_learner(
      learner = "lgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = lgb_model,
      folds = 5L,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = lgb_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(lgb1))
  # expect length 3
  testthat::expect_length(lgb1, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(lgb1[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(lgb1$base_prediction$.pred))


  # spatial
  testthat::expect_no_error(
    lgb2 <- fit_base_learner(
      learner = "lgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = lgb_model,
      folds = 5L,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = lgb_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(lgb2))
  # expect length 3
  testthat::expect_length(lgb2, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(lgb2[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(lgb2$base_prediction$.pred))


  # spatiotemporal
  testthat::expect_no_error(
    lgb3 <- fit_base_learner(
      learner = "lgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = lgb_model,
      folds = 5L,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = lgb_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(lgb3))
  # expect length 3
  testthat::expect_length(lgb3, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(lgb3[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(lgb3$base_prediction$.pred))
  
})


################################################################################
##### folds + bayes tuning
# testthat::test_that("fit lightgbm (folds + bayes)", {
#   # import sample data
#   # sample inlcudes 2 months data for 3 sites
#   # subset to only 50 predictors for light weight
#   dt_base <- readRDS(
#     testthat::test_path("..", "testdata", "base", "dt_base.rds")
#   )

#   # set model
#   lgb_model <- switch_model("lgb", device = "cpu")

#   # temporal
#   testthat::expect_no_error(
#     lgb4 <- fit_base_learner(
#       learner = "lgb",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = lgb_model,
#       folds = 5L,
#       cv_mode = "temporal",
#       tune_mode = "bayes",
#       tune_bayes_iter = 10,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       nthreads = 1,
#       trim_resamples = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(lgb4))
#   # expect length 3
#   testthat::expect_length(lgb4, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(lgb4[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(lgb4$base_prediction$.pred))


#   # spatial
#   testthat::expect_no_error(
#     lgb5 <- fit_base_learner(
#       learner = "lgb",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = lgb_model,
#       folds = 5L,
#       cv_mode = "spatial",
#       tune_mode = "bayes",
#       tune_bayes_iter = 2,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       nthreads = 1,
#       trim_resamples = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(lgb5))
#   # expect length 3
#   testthat::expect_length(lgb5, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(lgb5[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(lgb5$base_prediction$.pred))


#   # spatiotemporal
#   testthat::expect_no_error(
#     lgb6 <- fit_base_learner(
#       learner = "lgb",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = lgb_model,
#       folds = 5L,
#       cv_mode = "spatiotemporal",
#       tune_mode = "bayes",
#       tune_bayes_iter = 2,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       nthreads = 1,
#       trim_resamples = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(lgb6))
#   # expect length 3
#   testthat::expect_length(lgb6, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(lgb6[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(lgb6$base_prediction$.pred))
  
# })


################################################################################
##### args_generate_cv + grid tuning
testthat::test_that("fit lightgbm (args_generate_cv + grid)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  lgb_model <- switch_model("lgb", device = "cpu")
  # set grid
  lgb_grid <- expand.grid(
    mtry = c(20),
    trees = c(1000),
    learn_rate = c(0.1)
  )

  # temporal
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_no_error(
    lgb7 <- fit_base_learner(
      learner = "lgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = lgb_model,
      folds = NULL,
      args_generate_cv = args_temp,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = lgb_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(lgb7))
  # expect length 3
  testthat::expect_length(lgb7, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(lgb7[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(lgb7$base_prediction$.pred))


  # spatial
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  # warning is due to dt_base not having CRS
  testthat::expect_warning(
    lgb8 <- fit_base_learner(
      learner = "lgb",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = lgb_model,
      folds = NULL,
      args_generate_cv = args_spatial,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = lgb_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(lgb8))
  # expect length 3
  testthat::expect_length(lgb8, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(lgb8[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(lgb8$base_prediction$.pred))


  # spatiotemporal
  args_spatiotemporal <- list(
    target_cols = c("lon", "lat", "time"),
    preprocessing = "none",
    ngroup_init = 2L,
    cv_pairs = NULL,
    pairing = "1"
  )
  testthat::expect_no_error(
    lgb9 <- fit_base_learner(
      learner = "lgb",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 1, # full sample dataset for accurate ngroup cv
      model = lgb_model,
      folds = NULL,
      args_generate_cv = args_spatiotemporal,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = lgb_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      nthreads = 1,
      trim_resamples = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(lgb9))
  # expect length 3
  testthat::expect_length(lgb9, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(lgb9[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(lgb9$base_prediction$.pred))
  
})


################################################################################
##### args_generate_cv + bayes tuning
# testthat::test_that("fit lightgbm (args_generate_cv + bayes)", {
#   # import sample data
#   # sample inlcudes 2 months data for 3 sites
#   # subset to only 50 predictors for light weight
#   dt_base <- readRDS(
#     testthat::test_path("..", "testdata", "base", "dt_base.rds")
#   )

#   # set model
#   lgb_model <- switch_model("lgb", device = "cpu")

#   # temporal
#   args_temp <- list(
#     time_col = "time",
#     cv_fold = 10L,
#     window = 5L
#   )
#   testthat::expect_no_error(
#     lgb10 <- fit_base_learner(
#       learner = "lgb",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = lgb_model,
#       folds = NULL,
#       args_generate_cv = args_temp,
#       cv_mode = "temporal",
#       tune_mode = "bayes",
#       tune_bayes_iter = 2,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       nthreads = 1,
#       trim_resamples = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(lgb10))
#   # expect length 3
#   testthat::expect_length(lgb10, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(lgb10[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(lgb10$base_prediction$.pred))


#   # spatial
#   args_spatial = list(
#     target_cols = c("lon", "lat"),
#     cv_make_fun = spatialsample::spatial_block_cv,
#     v = 4
#   )
#   # warning is due to dt_base not having CRS
#   testthat::expect_no_error(
#     lgb11 <- fit_base_learner(
#       learner = "lgb",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = lgb_model,
#       folds = NULL,
#       args_generate_cv = args_spatial,
#       cv_mode = "spatial",
#       tune_mode = "bayes",
#       tune_bayes_iter = 2,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       nthreads = 1,
#       trim_resamples = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(lgb11))
#   # expect length 3
#   testthat::expect_length(lgb11, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(lgb11[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(lgb11$base_prediction$.pred))


#   # spatiotemporal
#   args_spatiotemporal <- list(
#     target_cols = c("lon", "lat", "time"),
#     preprocessing = "none",
#     ngroup_init = 2L,
#     cv_pairs = NULL,
#     pairing = "1"
#   )
#   testthat::expect_no_error(
#     lgb12 <- fit_base_learner(
#       learner = "lgb",
#       dt_full = data.table::data.table(dt_base),
#       r_subsample = 1, # full sample dataset for accurate ngroup cv
#       model = lgb_model,
#       folds = NULL,
#       args_generate_cv = args_spatiotemporal,
#       cv_mode = "spatiotemporal",
#       tune_mode = "bayes",
#       tune_bayes_iter = 2,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       nthreads = 1,
#       trim_resamples = TRUE, # trim samples
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(lgb12))
#   # expect length 3
#   testthat::expect_length(lgb12, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(lgb12[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(lgb12$base_prediction$.pred))

# })
