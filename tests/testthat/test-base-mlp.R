################################################################################
##### unit and integration tests for neural network base learner functions
##### main files: R/base_learner.R

################################################################################
##### folds + grid tuning
testthat::test_that("fit mlp (folds + grid))", {
  testthat::skip_on_ci()
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )
  dt_base <- dt_base[, grep("STACK|FUGITIVE", names(dt_base), invert = TRUE)]

  # set model
  mlp_model <- switch_model("mlp", device = "cpu")
  # set grid
  mlp_grid <- expand.grid(
    hidden_units = list(8),
    dropout = c(0.2),
    activation = c("relu"),
    learn_rate = c(0.01)
   )

  # temporal
  testthat::expect_no_error(
    mlp1 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
      folds = 5L,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      normalize = TRUE,
      trim_resamples = FALSE,
      workflow = TRUE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(mlp1))
  # expect length 4
  testthat::expect_length(mlp1, 4)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp1[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect fourth item is a workflow
  testthat::expect_true("workflow" %in% class(mlp1[[4]]))
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp1$base_prediction$.pred))


  # spatial
  testthat::expect_no_error(
    mlp2 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
      folds = 5L,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      normalize = TRUE,
      trim_resamples = FALSE,
      workflow = FALSE,
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


  # spatiotemporal
  testthat::expect_no_error(
    mlp3 <- fit_base_learner(
      learner = "mlp",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = mlp_model,
      folds = 5L,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = mlp_grid,
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      normalize = TRUE,
      trim_resamples = FALSE,
      workflow = FALSE,
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

})


################################################################################
##### folds + bayes tuning
# testthat::test_that("fit mlp (folds + bayes)", {
#   # import sample data
#   # sample inlcudes 2 months data for 3 sites
#   # subset to only 50 predictors for light weight
#   dt_base <- readRDS(
#     testthat::test_path("..", "testdata", "base", "dt_performance.rds")
#   )
#   dt_base <- dt_base[, grep("STACK|FUGITIVE", names(dt_base), invert = TRUE)]

#   # set model
#   mlp_model <- switch_model("mlp", device = "cpu")

#   # temporal
#   testthat::expect_no_error(
#     mlp4 <- fit_base_learner(
#       learner = "mlp",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = mlp_model,
#       folds = 5L,
#       cv_mode = "temporal",
#       tune_mode = "bayes",
#       tune_bayes_iter = 1,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       normalize = TRUE,
#       trim_resamples = FALSE,
#       workflow = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(mlp4))
#   # expect length 3
#   testthat::expect_length(mlp4, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(mlp4[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(mlp4$base_prediction$.pred))


#   # spatial
#   testthat::expect_no_error(
#     mlp5 <- fit_base_learner(
#       learner = "mlp",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = mlp_model,
#       folds = 5L,
#       cv_mode = "spatial",
#       tune_mode = "bayes",
#       tune_bayes_iter = 1,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       normalize = TRUE,
#       trim_resamples = FALSE,
#       workflow = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(mlp5))
#   # expect length 3
#   testthat::expect_length(mlp5, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(mlp5[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(mlp5$base_prediction$.pred))


#   # spatiotemporal
#   testthat::expect_no_error(
#     mlp6 <- fit_base_learner(
#       learner = "mlp",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = mlp_model,
#       folds = 5L,
#       cv_mode = "spatiotemporal",
#       tune_mode = "bayes",
#       tune_bayes_iter = 1,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       normalize = TRUE,
#       trim_resamples = FALSE,
#       workflow = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(mlp6))
#   # expect length 3
#   testthat::expect_length(mlp6, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(mlp6[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(mlp6$base_prediction$.pred))

# })


################################################################################
###### args_generate_cv + grid tuning
testthat::test_that("fit mlp (args_generate_cv + grid)", {
  testthat::skip_on_ci()
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )
  dt_base <- dt_base[, grep("STACK|FUGITIVE", names(dt_base), invert = TRUE)]

  # set model
  mlp_model <- switch_model("mlp", device = "cpu")
  # set grid
  mlp_grid <- expand.grid(
    hidden_units = list(8),
    dropout = c(0.2),
    activation = c("relu"),
    learn_rate = c(0.01)
   )

  # temporal
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_no_error(
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
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      normalize = TRUE,
      trim_resamples = FALSE,
      workflow = FALSE,
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


  # spatial
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  # warning is due to dt_base not having CRS
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
      tune_grid_size = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      normalize = TRUE,
      trim_resamples = FALSE,
      workflow = FALSE,
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


  # spatiotemporal
  args_spatiotemporal <- list(v = 2)
  testthat::expect_warning(
    mlp9 <- fit_base_learner(
      learner = "mlp",
      dt_full = data.table::data.table(dt_base),
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
      xvar = seq(5, ncol(dt_base)),
      normalize = TRUE,
      trim_resamples = FALSE,
      workflow = FALSE,
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

})


################################################################################
###### args_generate_cv + bayes tuning
# testthat::test_that("fit mlp (args_generate_cv + bayes)", {
#   # import sample data
#   # sample inlcudes 2 months data for 3 sites
#   # subset to only 50 predictors for light weight
#   dt_base <- readRDS(
#     testthat::test_path("..", "testdata", "base", "dt_performance.rds")
#   )
#   dt_base <- dt_base[, grep("STACK|FUGITIVE", names(dt_base), invert = TRUE)]

#   # set model
#   mlp_model <- switch_model("mlp", device = "cpu")

#   # temporal
#   args_temp <- list(
#     time_col = "time",
#     cv_fold = 10L,
#     window = 5L
#   )
#   testthat::expect_no_error(
#     mlp10 <- fit_base_learner(
#       learner = "mlp",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = mlp_model,
#       folds = NULL,
#       args_generate_cv = args_temp,
#       cv_mode = "temporal",
#       tune_mode = "bayes",
#       tune_bayes_iter = 1,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       normalize = TRUE,
#       trim_resamples = FALSE,
#       workflow = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(mlp10))
#   # expect length 3
#   testthat::expect_length(mlp10, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(mlp10[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(mlp10$base_prediction$.pred))


#   # spatial
#   args_spatial = list(
#     target_cols = c("lon", "lat"),
#     cv_make_fun = spatialsample::spatial_block_cv,
#     v = 4
#   )
#   # warning is due to dt_base not having CRS
#   testthat::expect_warning(
#     mlp11 <- fit_base_learner(
#       learner = "mlp",
#       dt_full = dt_base,
#       r_subsample = 0.3,
#       model = mlp_model,
#       folds = NULL,
#       args_generate_cv = args_spatial,
#       cv_mode = "spatial",
#       tune_mode = "bayes",
#       tune_bayes_iter = 1,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       normalize = TRUE,
#       trim_resamples = FALSE,
#       workflow = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(mlp11))
#   # expect length 3
#   testthat::expect_length(mlp11, 3)
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:3, function(x) methods::is(mlp11[[x]], "tbl_df"))),
#     c(TRUE, TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(mlp11$base_prediction$.pred))


#   # spatiotemporal
#   args_spatiotemporal <- list(v = 2)
#   testthat::expect_warning(
#     mlp12 <- fit_base_learner(
#       learner = "mlp",
#       dt_full = data.table::data.table(dt_base),
#       r_subsample = 0.3,
#       model = mlp_model,
#       folds = NULL,
#       args_generate_cv = args_spatiotemporal,
#       cv_mode = "spatiotemporal",
#       tune_mode = "bayes",
#       tune_bayes_iter = 1,
#       learn_rate = 0.1,
#       yvar = "Arithmetic.Mean",
#       xvar = seq(5, ncol(dt_base)),
#       normalize = TRUE,
#       trim_resamples = TRUE, # trim samples
#       workflow = FALSE,
#       return_best = TRUE
#     )
#   )
#   # expect a list
#   testthat::expect_true(is.list(mlp12))
#   # expect length 3
#   testthat::expect_length(mlp12, 2) # LENGTH 2 DUE TO UPDATED TRIM
#   # expect sub-items are tibble data.frames
#   testthat::expect_equal(
#     unlist(lapply(1:2, function(x) methods::is(mlp12[[x]], "tbl_df"))),
#     c(TRUE, TRUE)
#   )
#   # expect base predictions are numeric
#   testthat::expect_true(is.numeric(mlp12$base_prediction$.pred))

# })
