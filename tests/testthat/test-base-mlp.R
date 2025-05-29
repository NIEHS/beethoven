################################################################################
##### unit and integration tests for neural network base learner functions
##### main files: R/base_learner.R

################################################################################
##### expected success
testthat::test_that("fit_base_learner {brulee} MLP", {
  testthat::skip_if_not_installed("lme4")
  withr::local_package("bonsai")
  # import sample data (4 sites with non-zero variance predictors)
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base_new.rds")
  )

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
    data = dt_base,
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
    data = dt_base,
    cv_mode = "spatiotemporal"
  )

  # manual engine
  testthat::expect_no_error(
    mlp1 <- beethoven::fit_base_learner(
      rset = rset_base,
      model = mlp_model,
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_base)),
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

  # switch_model engine
  # testthat::expect_no_error(
  #   mlp2 <- beethoven::fit_base_learner(
  #     rset = rset_base,
  #     model = beethoven::switch_model(
  #       model_type = "mlp",
  #       device = "cpu"
  #     ),
  #     tune_grid_size = 2L,
  #     yvar = "Arithmetic.Mean",
  #     xvar = seq(4, ncol(dt_base)),
  #     drop_vars = NULL,
  #     normalize = TRUE
  #   )
  # )
})

################################################################################
##### expected failure
testthat::test_that("fit_base_learner {brulee} MLP", {
  # import sample data (4 sites with non-zero variance predictors)
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base_new.rds")
  )

  # set model
  mlp_model <- parsnip::mlp(
    hidden_units = c(parsnip::tune(), 32L),
    dropout = c(parsnip::tune(), 0.3),
    epochs = 1000,
    activation = "leaky_relu",
    learn_rate = parsnip::tune()
  ) %>%
    parsnip::set_engine("brulee", device = "cpu") %>%
    parsnip::set_mode("regression")

  # generate rset index
  num_cv_index <- beethoven::generate_cv_index_spt(
    data = dt_base,
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
    data = dt_base,
    cv_mode = "spatiotemporal"
  )

  # NULL rset
  testthat::expect_error(
    beethoven::fit_base_learner(
      rset = NULL,
      model = mlp_model,
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_base)),
      drop_vars = NULL,
      normalize = TRUE
    )
  )

  # NULL model
  testthat::expect_error(
    beethoven::fit_base_learner(
      rset = rset_base,
      model = NULL,
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_base)),
      drop_vars = NULL,
      normalize = TRUE
    )
  )

  # data.table as rset
  testthat::expect_error(
    beethoven::fit_base_learner(
      rset = dt_base,
      model = mlp_model,
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_base)),
      drop_vars = NULL,
      normalize = TRUE
    )
  )
})
