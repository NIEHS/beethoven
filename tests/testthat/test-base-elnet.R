################################################################################
##### unit and integration tests for elastic net base learner functions
##### main files: R/base_learner.R

################################################################################
##### folds + grid tuning
testthat::test_that("fit elnet (folds + grid)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  elnet_model <- switch_model("elnet")
  # set grid
  elnet_grid <- expand.grid(
    mixture = c(0, 1),
    penalty = c(0.01, 0.05)
  )

  # temporal
  testthat::expect_no_error(
    elnet1 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = elnet_model,
      folds = 5L,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = elnet_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = TRUE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet1))
  # expect length 4
  testthat::expect_length(elnet1, 4)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet1[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect fourth item is a workflow
  testthat::expect_true("workflow"  %in% class(elnet1[[4]]))
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet1$base_prediction$.pred))


  # spatial
  testthat::expect_no_error(
    elnet2 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = elnet_model,
      folds = 5L,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = elnet_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet2))
  # expect length 3
  testthat::expect_length(elnet2, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet2[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet2$base_prediction$.pred))


  # spatiotemporal
  testthat::expect_no_error(
    elnet3 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = elnet_model,
      folds = 5L,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = elnet_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet3))
  # expect length 3
  testthat::expect_length(elnet3, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet3[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet3$base_prediction$.pred))

})


################################################################################
##### folds + bayes tuning
testthat::test_that("fit elnet (folds + bayes)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  elnet_model <- switch_model("elnet")

  # temporal
  testthat::expect_no_error(
    elnet4 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = elnet_model,
      folds = 5L,
      cv_mode = "temporal",
      tune_mode = "bayes",
      tune_bayes_iter = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet4))
  # expect length 3
  testthat::expect_length(elnet4, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet4[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet4$base_prediction$.pred))


  # spatial
  testthat::expect_no_error(
    elnet5 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = elnet_model,
      folds = 5L,
      cv_mode = "spatial",
      tune_mode = "bayes",
      tune_bayes_iter = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet5))
  # expect length 3
  testthat::expect_length(elnet5, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet5[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet5$base_prediction$.pred))


  # spatiotemporal
  testthat::expect_no_error(
    elnet6 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = elnet_model,
      folds = 5L,
      cv_mode = "spatiotemporal",
      tune_mode = "bayes",
      tune_bayes_iter = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet6))
  # expect length 3
  testthat::expect_length(elnet6, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet6[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet6$base_prediction$.pred))

})


################################################################################
##### args_generate_cv + grid tuning
testthat::test_that("fit elnet (args_generate_cv + grid)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  elnet_model <- switch_model("elnet")
  # set grid
  elnet_grid <- expand.grid(
    mixture = c(0, 1),
    penalty = c(0.01, 0.05)
  )

  # temporal
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_no_error(
    elnet7 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = elnet_model,
      folds = NULL,
      args_generate_cv = args_temp,
      cv_mode = "temporal",
      tune_mode = "grid",
      tune_grid_in = elnet_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet7))
  # expect length 3
  testthat::expect_length(elnet7, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet7[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet7$base_prediction$.pred))


  # spatial
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  # warning is due to dt_base not having CRS
  testthat::expect_warning(
    elnet8 <- fit_base_learner(
      learner = "elnet",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 0.3,
      model = elnet_model,
      folds = NULL,
      args_generate_cv = args_spatial,
      cv_mode = "spatial",
      tune_mode = "grid",
      tune_grid_in = elnet_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet8))
  # expect length 3
  testthat::expect_length(elnet8, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet8[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet8$base_prediction$.pred))


  # spatiotemporal
  args_spatiotemporal <- list(v = 2)
  testthat::expect_warning(
    elnet9 <- fit_base_learner(
      learner = "elnet",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 0.3,
      model = elnet_model,
      folds = NULL,
      args_generate_cv = args_spatiotemporal,
      cv_mode = "spatiotemporal",
      tune_mode = "grid",
      tune_grid_in = elnet_grid,
      tune_grid_size = 2,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet9))
  # expect length 3
  testthat::expect_length(elnet9, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet9[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet9$base_prediction$.pred))

})


################################################################################
##### args_generate_cv + bayes tuning
testthat::test_that("fit elnet (args_generate_cv + bayes)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # set model
  elnet_model <- switch_model("elnet")

  # temporal
  args_temp <- list(
    time_col = "time",
    cv_fold = 10L,
    window = 5L
  )
  testthat::expect_no_error(
    elnet10 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = elnet_model,
      folds = NULL,
      args_generate_cv = args_temp,
      cv_mode = "temporal",
      tune_mode = "bayes",
      tune_bayes_iter = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet10))
  # expect length 3
  testthat::expect_length(elnet10, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet10[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet10$base_prediction$.pred))


  # spatial
  args_spatial = list(
    target_cols = c("lon", "lat"),
    cv_make_fun = spatialsample::spatial_block_cv,
    v = 4
  )
  # warning is due to dt_base not having CRS
  testthat::expect_warning(
    elnet11 <- fit_base_learner(
      learner = "elnet",
      dt_full = dt_base,
      r_subsample = 0.3,
      model = elnet_model,
      folds = NULL,
      args_generate_cv = args_spatial,
      cv_mode = "spatial",
      tune_mode = "bayes",
      tune_bayes_iter = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = FALSE,
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet11))
  # expect length 3
  testthat::expect_length(elnet11, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet11[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet11$base_prediction$.pred))


  # spatiotemporal
  args_spatiotemporal <- list(v = 2)
  testthat::expect_warning(
    elnet12 <- fit_base_learner(
      learner = "elnet",
      dt_full = data.table::data.table(dt_base),
      r_subsample = 0.3,
      model = elnet_model,
      folds = NULL,
      args_generate_cv = args_spatiotemporal,
      cv_mode = "spatiotemporal",
      tune_mode = "bayes",
      tune_bayes_iter = 1,
      learn_rate = 0.1,
      yvar = "Arithmetic.Mean",
      xvar = seq(5, ncol(dt_base)),
      trim_resamples = TRUE, # trim samples
      workflow = FALSE,
      return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet12))
  # expect length 3
  testthat::expect_length(elnet12, 2) # LENGTH 2 DUE TO UPDATED TRIM
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:2, function(x) methods::is(elnet12[[x]], "tbl_df"))),
    c(TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet12$base_prediction$.pred))

})
