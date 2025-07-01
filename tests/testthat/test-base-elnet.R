################################################################################
##### unit and integration tests for elastic net base learner functions
##### main files: R/base_learner.R

################################################################################
##### expected success
testthat::test_that("fit_base_learner {glmnt} elastic net", {
  testthat::skip_on_ci()
  # import sample data (4 sites with non-zero variance predictors)
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base_new.rds")
  )

  # set model
  elnet_model <- parsnip::linear_reg(
    mixture = parsnip::tune(),
    penalty = parsnip::tune()
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
    elnet1 <- beethoven::fit_base_learner(
      rset = rset_base,
      model = elnet_model,
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_base)),
      drop_vars = NULL,
      normalize = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet1))
  # expect length 2
  testthat::expect_length(elnet1, 3)
  # expect sub-items are workflow and metrics
  testthat::expect_equal(
    names(elnet1),
    c("workflow", "predictions", "metrics")
  )
  testthat::expect_true("workflow" %in% class(elnet1$workflow))
  testthat::expect_true("data.frame" %in% class(elnet1$predictions))
  testthat::expect_true("tbl" %in% class(elnet1$metrics))

  # switch_model engine
  testthat::expect_no_error(
    elnet2 <- beethoven::fit_base_learner(
      rset = rset_base,
      model = beethoven::switch_model(
        model_type = "elnet",
        device = "cpu"
      ),
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_base)),
      drop_vars = NULL,
      normalize = TRUE
    )
  )
})

################################################################################
##### expected failure
testthat::test_that("fit_base_learner {glmnet} elastic net", {
  # import sample data (4 sites with non-zero variance predictors)
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base_new.rds")
  )

  # set model
  elnet_model <- parsnip::linear_reg(
    mixture = parsnip::tune(),
    penalty = parsnip::tune()
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
      model = elnet_model,
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
      model = elnet_model,
      tune_grid_size = 2L,
      yvar = "Arithmetic.Mean",
      xvar = seq(4, ncol(dt_base)),
      drop_vars = NULL,
      normalize = TRUE
    )
  )
})
