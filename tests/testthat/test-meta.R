################################################################################
##### Unit and integration tests for meta learner functions
##### main files: R/meta_learner.R

################################################################################
##### fit_prediction
testthat::test_that("fit_prediction", {
  # import sample data
  fit_learner_base_elnet <- readRDS(
    testthat::test_path("..", "testdata", "meta", "fit_learner_base_elnet.rds")
  )
  list_dt_test <- readRDS(
    testthat::test_path("..", "testdata", "meta", "list_dt_test.rds")
  )

  testthat::expect_no_error(
    list_dt_pred <- lapply(
      seq_len(length(list_dt_test)),
      function(x) {
        beethoven::fit_prediction(
          fit = fit_learner_base_elnet[[x]],
          test = list_dt_test[[x]],
          target_cols = c("site_id", "time", "lon", "lat"),
          name = paste0("elnet_", sprintf("%05d", x))
        )
      }
    )
  )
  testthat::expect_true(is.list(list_dt_pred))
  testthat::expect_length(list_dt_pred, 3)
  testthat::expect_true("data.frame" %in% class(list_dt_pred[[1]]))

  # expected failures
  # bad fit object
  testthat::expect_error(
    beethoven::fit_prediction(
      fit = fit_learner_base_elnet[[1]][[1]],
      test = list_dt_test[[1]],
      target_cols = c("site_id", "time", "lon", "lat"),
      name = paste0("elnet_", sprintf("%05d", 1))
    )
  )
  # bad test object
  testthat::expect_error(
    beethoven::fit_prediction(
      fit = fit_learner_base_elnet[[1]],
      test = list_dt_test,
      target_cols = c("site_id", "time", "lon", "lat"),
      name = paste0("elnet_", sprintf("%05d", 1))
    )
  )
  # NULL columns
  testthat::expect_error(
    beethoven::fit_prediction(
      fit = fit_learner_base_elnet[[1]],
      test = list_dt_test[[1]],
      target_cols = NULL,
      name = paste0("elnet_", sprintf("%05d", 1))
    )
  )
  # NULL name
  testthat::expect_error(
    beethoven::fit_prediction(
      fit = fit_learner_base_elnet[[1]],
      test = list_dt_test[[1]],
      target_cols = c("site_id", "time", "lon", "lat"),
      name = NULL
    )
  )
})

################################################################################
##### attach_pred
testthat::test_that("attach_pred", {
  # import sample data
  fit_learner_base_elnet <- readRDS(
    testthat::test_path("..", "testdata", "meta", "fit_learner_base_elnet.rds")
  )
  list_dt_test <- readRDS(
    testthat::test_path("..", "testdata", "meta", "list_dt_test.rds")
  )

  testthat::expect_true(exists("fit_learner_base_elnet"))
  testthat::expect_true(is.list(fit_learner_base_elnet))
  testthat::expect_true(is.list(fit_learner_base_elnet[[1]]))
  testthat::expect_true(exists("list_dt_test"))
  testthat::expect_true(is.list(list_dt_test))
  testthat::expect_s3_class(dt_pred, "data.frame")

  # testthat::expect_no_error(
  dt_pred <- beethoven::fit_prediction(
    fit = fit_learner_base_elnet[[1]],
    test = list_dt_test[[1]],
    target_cols = c("site_id", "time", "lon", "lat"),
    name = paste0("elnet_", sprintf("%05d", 1))
  )
  # )
  testthat::expect_false("list" %in% class(dt_pred))
  testthat::expect_true(exists("dt_pred"))

  testthat::expect_s3_class(dt_pred, "data.frame")
  testthat::expect_true("elnet_00001" %in% names(dt_pred))

  target_cols <- c("site_id", "time", "lon", "lat")

  # expect no error on attaching predictions to base
  testthat::expect_no_error(
    dt_attach <- beethoven::attach_pred(
      pred = dt_pred,
      test = dt_test,
      target_cols = target_cols,
      yvar = "Arithmetic.Mean"
    )
  )
  # expect data.frame
  testthat::expect_s3_class(dt_attach, "data.frame")
  # expect unique columns
  testthat::expect_equal(ncol(dt_attach), length(unique(names(dt_attach))))
  # expect important columns retained
  testthat::expect_true(
    all(c(target_cols, "Arithmetic.Mean") %in% names(dt_attach))
  )
  # expect 6 columns (target_cols + Arithmetic.Mean + 1 base learners)
  testthat::expect_length(names(dt_attach), 6)

  # expected failures
  # missing yvar
  testthat::expect_error(
    beethoven::attach_pred(
      pred = dt_pred,
      test = dt_test,
      target_cols = target_cols,
      yvar = "not_here"
    )
  )
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
  testthat::expect_no_error(
    meta1 <- beethoven::fit_meta_learner(
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
    beethoven::fit_meta_learner(
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
    beethoven::fit_meta_learner(
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
    beethoven::fit_meta_learner(
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
  testthat::expect_no_error(
    meta4 <- beethoven::fit_meta_learner(
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
