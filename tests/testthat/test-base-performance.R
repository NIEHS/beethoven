################################################################################
##### performance tests for base learner functions
##### models, tuning parameters, and cross-validation methods are derived
##### from full pipeline run
##### main files: R/base_learner.R

################################################################################
##### elnet
testthat::test_that("fit elnet (performance)", {
  testthat::skip_on_ci()
  withr::local_package("lme4")
  # sample data: 1 year; 50 sites; 35 predictors
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # create training and testing data
  rset_initial <- rsample::vfold_cv(
    dt_performance,
    v = 2
  )
  dt_train <- rsample::training(rset_initial$splits[[1]])
  dt_test <- rsample::assessment(rset_initial$splits[[1]])

  # set model
  elnet_model <- parsnip::linear_reg(
    mixture = parsnip::tune(),
    penalty = parsnip::tune()
  ) %>%
    parsnip::set_engine("brulee", device = "cpu") %>%
    parsnip::set_mode("regression")

  # generate rset index
  num_cv_index <- beethoven::generate_cv_index_spt(
    data = dt_train,
    crs = 5070L,
    cellsize = 250000L,
    locs_id = "site_id",
    coords = c("lon", "lat"),
    v = 4,
    time_id = "time"
  )

  # create rset
  rset_base <- beethoven::convert_cv_index_rset(
    cvindex = num_cv_index,
    data = dt_train,
    cv_mode = "spatiotemporal"
  )

  # manual engine
  testthat::expect_no_error(
    elnet1 <- beethoven::fit_base_learner(
      rset = rset_base,
      model = elnet_model,
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_train)),
      drop_vars = NULL,
      normalize = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet1))
  # expect length 2
  testthat::expect_length(elnet1, 2)
  # expect sub-items are workflow and metrics
  testthat::expect_true("workflow" %in% class(elnet1[[1]]))
  testthat::expect_true("tbl" %in% class(elnet1[[2]]))

  # expect no error on prediction
  testthat::expect_no_error(
    pred1 <- stats::predict(elnet1[[1]], dt_test)
  )
  # expect tibble
  testthat::expect_true("tbl" %in% class(pred1))
  # expect prediction values are numeric
  testthat::expect_true(is.numeric(pred1$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% pred1$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(pred1$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  pred1_mean <- mean(pred1$.pred)
  testthat::expect_true(pred1_mean >= 6 && pred1_mean <= 9)
  # expect SD ~= 7 (> 3 and < 10)
  pred1_sd <- sd(pred1$.pred)
  testthat::expect_true(pred1_sd >= 3 && pred1_sd <= 10)

  # performance metrics
  elnet_pred <- data.frame(
    Arithmetic.Mean = dt_test$Arithmetic.Mean,
    pred = pred1$.pred
  )
  rmse1 <- yardstick::rmse(
    data = elnet_pred,
    truth = Arithmetic.Mean,
    estimate = pred
  )
  testthat::expect_true(rmse1$.estimate < 25)
  rsq1 <- yardstick::rsq(
    data = elnet_pred,
    truth = Arithmetic.Mean,
    estimate = pred
  )
  testthat::expect_true(rsq1$.estimate > 0.0)
})

################################################################################
##### lightGBM
testthat::test_that("fit lightGBM (performance)", {
  testthat::skip_on_ci()
  # sample data: 1 year; 50 sites; 35 predictors
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # create training and testing data
  rset_initial <- rsample::vfold_cv(
    dt_performance,
    v = 2
  )
  dt_train <- rsample::training(rset_initial$splits[[1]])
  dt_test <- rsample::assessment(rset_initial$splits[[1]])

  # set model
  lightgbm_model <- parsnip::boost_tree(
    trees = 100,
    learn_rate = parsnip::tune(),
    mtry = 20
  ) %>%
    parsnip::set_engine("lightgbm", device = "cpu") %>%
    parsnip::set_mode("regression")

  # generate rset index
  num_cv_index <- beethoven::generate_cv_index_spt(
    data = dt_train,
    crs = 5070L,
    cellsize = 250000L,
    locs_id = "site_id",
    coords = c("lon", "lat"),
    v = 4,
    time_id = "time"
  )

  # create rset
  rset_base <- beethoven::convert_cv_index_rset(
    cvindex = num_cv_index,
    data = dt_train,
    cv_mode = "spatiotemporal"
  )

  # manual engine
  testthat::expect_no_error(
    lgb1 <- beethoven::fit_base_learner(
      rset = rset_base,
      model = lightgbm_model,
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_train)),
      drop_vars = NULL,
      normalize = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(lgb1))
  # expect length 2
  testthat::expect_length(lgb1, 2)
  # expect sub-items are workflow and metrics
  testthat::expect_true("workflow" %in% class(lgb1[[1]]))
  testthat::expect_true("tbl" %in% class(lgb1[[2]]))

  # expect no error on prediction
  testthat::expect_no_error(
    pred2 <- stats::predict(lgb1[[1]], dt_test)
  )
  # expect tibble
  testthat::expect_true("tbl" %in% class(pred2))
  # expect prediction values are numeric
  testthat::expect_true(is.numeric(pred2$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% pred2$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(pred2$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  pred2_mean <- mean(pred2$.pred)
  testthat::expect_true(pred2_mean >= 6 && pred2_mean <= 9)
  # expect SD ~= 7 (> 3 and < 10)
  pred2_sd <- sd(pred2$.pred)
  # testthat::expect_true(pred2_sd >= 3 && pred2_sd <= 10)

  # performance metrics
  lgb_pred <- data.frame(
    Arithmetic.Mean = dt_test$Arithmetic.Mean,
    pred = pred2$.pred
  )
  rmse2 <- yardstick::rmse(
    data = lgb_pred,
    truth = Arithmetic.Mean,
    estimate = pred
  )
  testthat::expect_true(rmse2$.estimate < 25)
  rsq2 <- yardstick::rsq(
    data = lgb_pred,
    truth = Arithmetic.Mean,
    estimate = pred
  )
  testthat::expect_true(rsq2$.estimate > 0.0)
})

################################################################################
##### {brulee} MLP
testthat::test_that("fit {brulee} MLP (performance)", {
  testthat::skip_on_ci()
  # sample data: 1 year; 50 sites; 35 predictors
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # create training and testing data
  rset_initial <- rsample::vfold_cv(
    dt_performance,
    v = 2
  )
  dt_train <- rsample::training(rset_initial$splits[[1]])
  dt_test <- rsample::assessment(rset_initial$splits[[1]])

  # set model
  mlp_model <- parsnip::mlp(
    hidden_units = 32L,
    dropout = 0.3,
    epochs = 250,
    activation = "leaky_relu",
    learn_rate = parsnip::tune()
  ) %>%
    parsnip::set_engine("brulee", device = "cpu") %>%
    parsnip::set_mode("regression")

  # generate rset index
  num_cv_index <- beethoven::generate_cv_index_spt(
    data = dt_train,
    crs = 5070L,
    cellsize = 250000L,
    locs_id = "site_id",
    coords = c("lon", "lat"),
    v = 4,
    time_id = "time"
  )

  # create rset
  rset_base <- beethoven::convert_cv_index_rset(
    cvindex = num_cv_index,
    data = dt_train,
    cv_mode = "spatiotemporal"
  )

  # manual engine
  testthat::expect_no_error(
    mlp1 <- beethoven::fit_base_learner(
      rset = rset_base,
      model = mlp_model,
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_train)),
      drop_vars = NULL,
      normalize = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(mlp1))
  # expect length 2
  testthat::expect_length(mlp1, 2)
  # expect sub-items are workflow and metrics
  testthat::expect_true("workflow" %in% class(mlp1[[1]]))
  testthat::expect_true("tbl" %in% class(mlp1[[2]]))

  # expect no error on prediction
  testthat::expect_no_error(
    pred3 <- stats::predict(mlp1[[1]], dt_test)
  )
  # expect tibble
  testthat::expect_true("tbl" %in% class(pred3))
  # expect prediction values are numeric
  testthat::expect_true(is.numeric(pred3$.pred))
  # expect no NA values
  testthat::expect_false(NA %in% pred3$.pred)
  # expect non-repetative predictions
  testthat::expect_true(length(unique(pred3$.pred)) > 1)
  # expect mean ~= 7.76 (observed mean)
  pred3_mean <- mean(pred3$.pred)
  testthat::expect_true(pred3_mean >= 6 && pred3_mean <= 9)
  # expect SD ~= 7 (> 3 and < 10)
  pred3_sd <- sd(pred3$.pred)
  testthat::expect_true(pred3_sd >= 3 && pred3_sd <= 10)

  # performance metrics
  mlp_pred <- data.frame(
    Arithmetic.Mean = dt_test$Arithmetic.Mean,
    pred = pred3$.pred
  )
  rmse3 <- yardstick::rmse(
    data = mlp_pred,
    truth = Arithmetic.Mean,
    estimate = pred
  )
  testthat::expect_true(rmse3$.estimate < 25)
  rsq3 <- yardstick::rsq(
    data = mlp_pred,
    truth = Arithmetic.Mean,
    estimate = pred
  )
  testthat::expect_true(rsq3$.estimate > 0.0)
})
