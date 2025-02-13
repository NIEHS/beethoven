################################################################################
##### unit and integration tests for base learner auxiliary functions
##### main files: R/base_learner.R

################################################################################
##### make_subdata
testthat::test_that("make_subdata", {
  dt_base <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_base.rds")
  )

  # expect no error for rows to be sampled
  testthat::expect_no_error(
    make_rows <- beethoven:::make_subdata(data = dt_base, n = 50)
  )
  # expect 50 rows
  testthat::expect_length(make_rows, 50)

  # expect error for NULL p and n
  testthat::expect_error(make_subdata(data = dt_base, n = NULL, p = NULL))
})


################################################################################
##### attach_xy
testthat::test_that("attach_xy", {
  # import sample data
  dt_feat <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_feat_sample.rds")
  )
  sf_feat <- readRDS(
    testthat::test_path("..", "testdata", "base", "sf_feat_sample.rds")
  )


  # expect no error on attachment
  testthat::expect_no_error(
    dt_attach <- attach_xy(dt_feat, sf_feat)
  )
  # expect data.frame
  testthat::expect_s3_class(dt_attach, "data.frame")
  # expect 2 columns added to dt_feat (lon and lat)
  testthat::expect_equal(ncol(dt_feat) + 2, ncol(dt_attach))
  # expect lon and lat in names
  testthat::expect_equal(c("lon", "lat") %in% names(dt_attach), c(TRUE, TRUE))
})


################################################################################
##### assign_learner_cv
testthat::test_that("assign_learner_cv", {
  learners <- c("mlp", "lgb", "elnet")
  modes <- c("spatiotemporal", "spatial", "temporal")
  # expect no error when assigning learners
  testthat::expect_no_error(
    alc <- assign_learner_cv(
      learner = learners,
      cv_mode = modes,
      cv_rep = 2L,
      num_device = 1
    )
  )
  # expect data.frame
  testthat::expect_s3_class(alc, "data.frame")
  # expect 18 rows (3 learners * 3 modes * 2 reps) and 3 columns
  testthat::expect_equal(dim(alc), c(18, 3))
  # expect all learners represented
  testthat::expect_true(all(learners %in% alc[, 1]))
  # expect all modes represented
  testthat::expect_true(all(modes %in% alc[, 2]))
  
  # CUDA expectatinos
  # expect CUDA for mlp
  testthat::expect_length(
    grep("cuda", alc[alc$learner == "mlp", 3]), 6
  )
  # expect CUDA for lgb
  testthat::expect_length(
    grep("cuda", alc[alc$learner == "lgb", 3]), 6
  )
  # expect NULL for elnet
  testthat::expect_length(
    grep("cuda", alc[alc$learner == "elnet", 3]), 0
  )

  # expect no error when blanacing learners
  testthat::expect_no_error(
    alcb <- assign_learner_cv(
      learner = learners,
      cv_mode = modes,
      cv_rep = 2L,
      num_device = 4,
      balance = TRUE
    )
  )
  alcb_devices <- grep("cuda", alcb$device, value = TRUE)
  alcb_devicenum <- unique(as.numeric(gsub("cuda:", "", unlist(alcb_devices))))
  testthat::expect_length(alcb_devicenum, 4)

})


################################################################################
##### generate_cv_index_spt
testthat::test_that("generate_cv_index_spt", {
  withr::local_package("dplyr")
  withr::local_package("data.table")

  # import sample data
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # expect warning due to non-sf dt_performance
  testthat::expect_warning(
    index_spt1 <- generate_cv_index_spt(
      data = data.table::data.table(dt_performance),
      locs_id = "site_id",
      coords = c("lon", "lat"),
      v = 5L,
      time_id = "time"
    )
  )
  # expect list
  testthat::expect_true(is.list(index_spt1))
  # expect length of 10 (v = 5L; data contains years 2020 + 2021)
  testthat::expect_length(index_spt1, 10)
  # expect no attributes
  testthat::expect_length(attr(index_spt1, "ref_list"), 0)

  # import sample data
  dt_performance2 <- dt_performance[dt_performance$time %like% "2020", ]

  # expect warning due to non-sf dt_performance2
  testthat::expect_warning(
    index_spt2 <- generate_cv_index_spt(
      data = data.table::data.table(dt_performance2),
      locs_id = "site_id",
      coords = c("lon", "lat"),
      v = 5L,
      time_id = "time"
    )
  )
  # expect list
  testthat::expect_true(is.list(index_spt2))
  # expect length of 5 (v = 5L; data contains years 2020)
  testthat::expect_length(index_spt2, 5)
  # expect no attributes
  testthat::expect_length(attr(index_spt2, "ref_list"), 0)

})


################################################################################
##### generate_cv_index_ts
testthat::test_that("generate_cv_index_ts", {
  # import sample data
  dt_attach <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_wide.rds")
  )

  # expect no error with cv_fold = 10, window = 5
  testthat::expect_no_error(
    index_ts1 <- generate_cv_index_ts(
      data = dt_attach,
      time_col = "time",
      cv_fold = 10L,
      window = 5L
    )
  )
  # expect a list
  testthat::expect_true(is.list(index_ts1))
  # expect list is length 10 (for each fold)
  testthat::expect_length(index_ts1, 10)
  # expect sublists are lists of length 2
  testthat::expect_true(is.list(index_ts1[[1]]))
  testthat::expect_length(index_ts1[[1]], 2)
})


################################################################################
##### generate_cv_index_sp
testthat::test_that("generate_cv_index_sp", {
  # import sample data
  dt_attach <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_wide.rds")
  )

  # expect CRS warning during indexing
  testthat::expect_warning(
    index_sp <- generate_cv_index_sp(
      data = dt_attach,
      target_cols = c("lon", "lat"),
      v = 4
    )
  )
  # expect integer
  testthat::expect_true(is.integer(index_sp))
  # expect index length = nrow of dt
  testthat::expect_length(index_sp, nrow(dt_attach))
  # expect range is 1 - 4 (equal to v)
  testthat::expect_equal(sort(unique(index_sp)), 1:4)
})


################################################################################
##### vis_spt_rset
testthat::test_that("vis_spt_rset", {
  # import sample data
  dt_attach <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_wide.rds")
  )
  
  # expect CRS warning during indexing
  testthat::expect_warning(
    index_sp <- generate_cv_index_sp(
      data = dt_attach,
      target_cols = c("lon", "lat"),
      v = 4
    )
  )

  # expect no error converting to rset
  testthat::expect_no_error(
    rset_sp <- convert_cv_index_rset(
      cvindex = index_sp,
      data = dt_attach,
      ref_list = NULL,
      cv_mode = "spatial"
    )
  )

  # expect no warning when rendering grpahic
  testthat::expect_no_warning(
    vis_spt_rset(rset_sp)
  )
})


################################################################################
##### switch_generate_cv_rset
testthat::test_that("switch_generate_cv_rset", {
  withr::local_package("dplyr")
  # import sample data
  dt_attach <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_wide.rds")
  )


  # spatiotemporal
  testthat::expect_warning(
    index_spt_direct <- generate_cv_index_spt(
      data = data.table::data.table(dt_attach),
      locs_id = "site_id",
      coords = c("lon", "lat"),
      v = 5L,
      time_id = "time"
    )
  )
  testthat::expect_warning(
    index_spt_switch <- switch_generate_cv_rset(
      learner = "spatiotemporal",
      data = data.table::data.table(dt_attach),
      locs_id = "site_id",
      coords = c("lon", "lat"),
      v = 5L,
      time_id = "time"
    )
  )
  testthat::expect_equal(class(index_spt_direct), class(index_spt_direct))
  testthat::expect_equal(length(index_spt_direct), length(index_spt_direct))


  # spatial
  # expect CRS warning during indexing
  testthat::expect_warning(
    index_sp_direct <- generate_cv_index_sp(
      data = dt_attach,
      target_cols = c("lon", "lat"),
      v = 4
    )
  )
  # expect CRS warning during indexing
  testthat::expect_warning(
    index_sp_switch <- switch_generate_cv_rset(
      learner = "spatial",
      data = dt_attach,
      target_cols = c("lon", "lat"),
      v = 4
    )
  )
  # expect direct and switch-generated have same length
  testthat::expect_equal(length(index_sp_direct), length(index_sp_switch))
  # expect same range
  testthat::expect_equal(
    sort(unique(index_sp_direct)), sort(unique(index_sp_direct))
  )


  # temporal
  testthat::expect_no_error(
    index_ts_direct <- generate_cv_index_ts(
      data = dt_attach,
      time_col = "time",
      cv_fold = 10L,
      window = 5L
    )
  )
  testthat::expect_no_error(
    index_ts_switch <- switch_generate_cv_rset(
      learner = "temporal",
      data = dt_attach,
      time_col = "time",
      cv_fold = 10L,
      window = 5L
    )
  )
  # expect direct and switch-generated rsamples are identical
  testthat::expect_identical(index_ts_direct, index_ts_switch)

})


################################################################################
##### convert_cv_index_rset
testthat::test_that("convert_cv_index_rset", {
  withr::local_package("dplyr")

  # import sample data
  dt_performance <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_performance.rds")
  )

  # spatiotemporal
  testthat::expect_warning(
    index_spt <- generate_cv_index_spt(
      data = data.table::data.table(dt_performance),
      locs_id = "site_id",
      coords = c("lon", "lat"),
      v = 5L,
      time_id = "time"
    )
  )
  # expect no error convert to rset
  testthat::expect_no_error(
    rset_spt <- convert_cv_index_rset(
      cvindex = index_spt,
      data = dt_performance,
      cv_mode = "spatiotemporal"
    )
  )
  # expect rset
  testthat::expect_true(methods::is(rset_spt, "manual_rset"))
  # expect first element is list
  testthat::expect_true(is.list(rset_spt[[1]]))
  # exepct list is length 10
  testthat::expect_length(rset_spt[[1]], 10)

  # import sample data
  dt_attach <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_wide.rds")
  )
  # spatial
  # expect CRS warning during indexing
  testthat::expect_warning(
    index_sp <- generate_cv_index_sp(
      data = dt_attach,
      target_cols = c("lon", "lat"),
      v = 4
    )
  )
  # expect no error convert to rset
  testthat::expect_no_error(
    rset_sp <- convert_cv_index_rset(
      cvindex = index_sp,
      data = dt_attach,
      cv_mode = "spatial"
    )
  )
  # expect rset
  testthat::expect_true(methods::is(rset_sp, "manual_rset"))
  # expect first element is list
  testthat::expect_true(is.list(rset_sp[[1]]))
  # exepct list is length 4 to match v
  testthat::expect_length(rset_sp[[1]], 4)


  # temporal
  testthat::expect_no_error(
    index_ts <- generate_cv_index_ts(
      data = dt_attach,
      time_col = "time",
      cv_fold = 10L,
      window = 5L
    )
  )
  # expect no error on convert to rset
  testthat::expect_no_error(
    rset_ts <- convert_cv_index_rset(
      cvindex = index_ts,
      data = dt_attach,
      cv_mode = "temporal"
    )
  )
  # expect rset
  testthat::expect_true(methods::is(rset_ts, "manual_rset"))
  # expect first element is list
  testthat::expect_true(is.list(rset_ts[[1]]))
  # exepct list is length 10 to match folds
  testthat::expect_length(rset_ts[[1]], 10)

})


################################################################################
##### switch_model
testthat::test_that("switch_model", {
  withr::local_package("dplyr")
  # expect no error with mlp
  testthat::expect_no_error(
    switch_mlp <- switch_model(model_type = "mlp")
  )
  # expect an mlp object
  testthat::expect_true(methods::is(switch_mlp, "mlp"))
  # expect brulee engine
  testthat::expect_true(switch_mlp$engine == "brulee")


  # expect no error with lgb
  testthat::expect_no_error(
    switch_lgb <- switch_model(model_type = "lgb")
  )
  # expect a boosted tree object
  testthat::expect_true(methods::is(switch_lgb, "boost_tree"))
  # expect lightgbm engine
  testthat::expect_true(switch_lgb$engine == "lightgbm")


  # expect no error with xgb
  testthat::expect_no_error(
    switch_xgb <- switch_model(model_type = "xgb")
  )
  # expect a boosted tree object
  testthat::expect_true(methods::is(switch_xgb, "boost_tree"))
  # expect xgboost engine
  testthat::expect_true(switch_xgb$engine == "xgboost")


  # expect no error with elnet
  testthat::expect_no_error(
    switch_elnet <- switch_model(model_type = "elnet")
  )
  # expect an linear regression object
  testthat::expect_true(methods::is(switch_elnet, "linear_reg"))
  # expect glmnet engine
  testthat::expect_true(switch_elnet$engine == "glmnet")
})
