################################################################################
##### unit and integration tests for base learner functions
##### main files: R/base_learner.R

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
  # expect CUDA for elnet
  testthat::expect_length(
    grep("cuda", alc[alc$learner == "elnet", 3]), 0
  )
})


################################################################################
##### generate_cv_index_spt
testthat::test_that("generate_cv_index_spt", {
  # import sample data
  dt_attach <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_wide.rds")
  )


  # expect no error with cv_pairs = NULL + pairing = 1
  testthat::expect_no_error(
    index_spt1 <- generate_cv_index_spt(
      data = data.table::data.table(dt_attach),
      target_cols = c("lon", "lat", "time"),
      preprocessing = "none",
      ngroup_init = 2L,
      cv_pairs = NULL,
      pairing = "1"
    )
  )
  # expect vector
  testthat::expect_true(is.vector(index_spt1))
  # expect numeric values
  testthat::expect_true(is.numeric(index_spt1))
  # expect index length = nrow of dt
  testthat::expect_length(index_spt1, nrow(dt_attach))
  # expect range is 1 - 2 (equal to ngroup_init)
  testthat::expect_equal(sort(unique(index_spt1)), 1:2)
  # expect no attributes
  testthat::expect_length(attr(index_spt1, "ref_list"), 0)


  # expect no error with cv_pairs = NULL + pairing = 2
  testthat::expect_no_error(
    index_spt2 <- generate_cv_index_spt(
      data = data.table::data.table(dt_attach),
      target_cols = c("lon", "lat", "time"),
      preprocessing = "none",
      ngroup_init = 2L,
      cv_pairs = NULL,
      pairing = "2"
    )
  )
  # expect vector
  testthat::expect_true(is.vector(index_spt2))
  # expect numeric values
  testthat::expect_true(is.numeric(index_spt2))
  # expect index length = nrow of dt
  testthat::expect_length(index_spt1, nrow(dt_attach))
  # expect range is 1 - 2 (equal to ngroup_init)
  testthat::expect_equal(sort(unique(index_spt2)), 1:2)
  # expect no attributes
  testthat::expect_length(attr(index_spt2, "ref_list"), 0)


  # expect no error with cv_pairs = 6 + pairing = 1
  # 645 observations to utilize 5 groups
  testthat::expect_no_error(
    index_spt3 <- generate_cv_index_spt(
      data = data.table::data.table(dt_attach[1:645, ]),
      target_cols = c("lon", "lat", "time"),
      preprocessing = "none",
      ngroup_init = 5L,
      cv_pairs = 6L,
      pairing = "1"
    )
  )
  # expect numeric values
  testthat::expect_true(is.numeric(index_spt3))
  # expect index length = nrow of dt
  testthat::expect_length(index_spt3, nrow(dt_attach) - 1)
  # expect range is 1 - 5 (equal to ngroup_init)
  testthat::expect_equal(sort(unique(index_spt3)), 1:5)
  # expect attributes
  testthat::expect_length(attr(index_spt3, "ref_list"), 6)


  # expect no error with cv_pairs = 6 + pairing = 2
  # 645 observations to utilize 5 groups
  testthat::expect_no_error(
    index_spt4 <- generate_cv_index_spt(
      data = data.table::data.table(dt_attach[1:645, ]),
      target_cols = c("lon", "lat", "time"),
      preprocessing = "none",
      ngroup_init = 5L,
      cv_pairs = 6L,
      pairing = "2"
    )
  )
  # expect numeric values
  testthat::expect_true(is.numeric(index_spt4))
  # expect index length = nrow of dt
  testthat::expect_length(index_spt4, nrow(dt_attach) - 1)
  # expect range is 1 - 5 (equal to ngroup_init)
  testthat::expect_equal(sort(unique(index_spt4)), 1:5)
  # expect attributes
  testthat::expect_length(attr(index_spt4, "ref_list"), 6)


  # expect error when > 3 target columns
  testthat::expect_error(
    generate_cv_index_spt(
      target_cols = c("lon", "lat", "time", "fourth")
    )
  )
  # expect error when ngroup_init > cv_pairs
  testthat::expect_error(
    generate_cv_index_spt(
      ngroup_init = 5L,
      cv_pairs = 2L
    )
  )
  # expect error when cv_pairs exceeding 2-combinations of ngroup_init
  testthat::expect_error(
    generate_cv_index_spt(
      ngroup_init = 4L,
      cv_pairs = 7L
    )
  )
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
      cv_make_fun = spatialsample::spatial_block_cv,
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
      cv_make_fun = spatialsample::spatial_block_cv,
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
  # import sample data
  dt_attach <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_wide.rds")
  )


  # spatiotemporal
  testthat::expect_no_error(
    index_spt_direct <- generate_cv_index_spt(
      data = data.table::data.table(dt_attach),
      target_cols = c("lon", "lat", "time"),
      preprocessing = "none",
      ngroup_init = 2L,
      cv_pairs = NULL,
      pairing = "1"
    )
  )
  testthat::expect_no_error(
    index_spt_switch <- switch_generate_cv_rset(
      learner = "spatiotemporal",
      data = data.table::data.table(dt_attach),
      target_cols = c("lon", "lat", "time"),
      preprocessing = "none",
      ngroup_init = 2L,
      cv_pairs = NULL,
      pairing = "1"
    )
  )
  # expect direct and switch-generated rsamples are identical
  testthat::expect_identical(index_spt_direct, index_spt_switch)


  # spatial
  # expect CRS warning during indexing
  testthat::expect_warning(
    index_sp_direct <- generate_cv_index_sp(
      data = dt_attach,
      target_cols = c("lon", "lat"),
      cv_make_fun = spatialsample::spatial_block_cv,
      v = 4
    )
  )
  # expect CRS warning during indexing
  testthat::expect_warning(
    index_sp_switch <- switch_generate_cv_rset(
      learner = "spatial",
      data = dt_attach,
      target_cols = c("lon", "lat"),
      cv_make_fun = spatialsample::spatial_block_cv,
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
  # import sample data
  dt_attach <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_wide.rds")
  )


  # spatiotemporal
  testthat::expect_no_error(
    index_spt <- generate_cv_index_spt(
      data = data.table::data.table(dt_attach[1:645, ]),
      target_cols = c("lon", "lat", "time"),
      preprocessing = "none",
      ngroup_init = 5L,
      cv_pairs = 6L,
      pairing = "1"
    )
  )
  # expect no error convert to rset
  testthat::expect_no_error(
    rset_spt <- convert_cv_index_rset(
      cvindex = index_spt,
      data = dt_attach,
      cv_mode = "spatiotemporal"
    )
  )
  # expect rset
  testthat::expect_true(methods::is(rset_spt, "manual_rset"))
  # expect first element is list
  testthat::expect_true(is.list(rset_spt[[1]]))
  # exepct list is length 6
  testthat::expect_length(rset_spt[[1]], 6)


  # spatial
  # expect CRS warning during indexing
  testthat::expect_warning(
    index_sp <- generate_cv_index_sp(
      data = dt_attach,
      target_cols = c("lon", "lat"),
      cv_make_fun = spatialsample::spatial_block_cv,
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


################################################################################
##### fit_base_learner
##### mlp
testthat::test_that("fit_base_learner (mlp + folds)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_long <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_long.rds")
  )


  # set model
  mlp_model <- switch_model("mlp")
  # set grid
  mlp_grid <- expand.grid(
    hidden_units = c(
      256, 512, 1024,
      c(128, 128), c(128, 256), c(256, 512),
      c(512, 1024), c(1024, 1024), c(256, 516, 1024)
    ),
    dropout = 1 / seq(5, 2, -1),
    activation = c("relu", "leaky_relu"),
    learn_rate = c(0.1, 0.05, 0.01, 0.005)
   )


  # temporal
  # warning is due to 3 metrcis (rmse, rsq, mae)
  testthat::expect_warning(
    mlp1 <- fit_base_learner(
        learner = "mlp",
        dt_full = dt_long,
        r_subsample = 0.3,
        model = mlp_model,
        folds = 5L,
        cv_mode = "temporal",
        tune_mode = "grid",
        tune_grid_in = mlp_grid,
        tune_grid_size = 2,
        learn_rate = 0.1,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_long)),
        nthreads = 1,
        trim_resamples = FALSE,
        return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(mlp1))
  # expect length 3
  testthat::expect_length(mlp1, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(mlp1[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(mlp1$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  testthat::expect_true(
    length(unique(mlp1$base_prediction$.pred)) > 1
  )


  # spatial
  # warning is due to 3 metrcis (rmse, rsq, mae)
  testthat::expect_warning(
    mlp2 <- fit_base_learner(
        learner = "mlp",
        dt_full = dt_long,
        r_subsample = 0.3,
        model = mlp_model,
        folds = 5L,
        cv_mode = "temporal",
        tune_mode = "grid",
        tune_grid_in = mlp_grid,
        tune_grid_size = 2,
        learn_rate = 0.1,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_long)),
        nthreads = 1,
        trim_resamples = FALSE,
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
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  testthat::expect_true(
    length(unique(mlp2$base_prediction$.pred)) > 1
  )


  # spatiotemporal
  # warning is due to 3 metrcis (rmse, rsq, mae)
  testthat::expect_warning(
    mlp3 <- fit_base_learner(
        learner = "mlp",
        dt_full = dt_long,
        r_subsample = 0.3,
        model = mlp_model,
        folds = 5L,
        cv_mode = "temporal",
        tune_mode = "grid",
        tune_grid_in = mlp_grid,
        tune_grid_size = 2,
        learn_rate = 0.1,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_long)),
        nthreads = 1,
        trim_resamples = FALSE,
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
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  testthat::expect_true(
    length(unique(mlp3$base_prediction$.pred)) > 1
  )
})

# testthat::test_that("fit_base_learner (mlp + args_generate_cv)", {
#   # import sample data
#   # sample inlcudes 2 months data for 3 sites
#   # subset to only 50 predictors for light weight
#   dt_long <- readRDS(
#     testthat::test_path("..", "testdata", "base", "dt_long.rds")
#   )


#   # set model
#   mlp_model <- switch_model("mlp")
#   # set grid
#   mlp_grid <- expand.grid(
#     hidden_units = c(
#       256, 512, 1024,
#       c(128, 128), c(128, 256), c(256, 512),
#       c(512, 1024), c(1024, 1024), c(256, 516, 1024)
#     ),
#     dropout = 1 / seq(5, 2, -1),
#     activation = c("relu", "leaky_relu"),
#     learn_rate = c(0.1, 0.05, 0.01, 0.005)
#   )

#   # temporal
#   # warning is due to 3 metrcis (rmse, rsq, mae)
#   args_temp <- list(
#     time_col = "time",
#     cv_fold = 10L,
#     window = 5L
#   )
#   testthat::expect_warning(
#     mlp4 <- fit_base_learner(
#         learner = "mlp",
#         dt_full = dt_long,
#         r_subsample = 0.3,
#         model = mlp_model,
#         args_generate_cv = args_temp,
#         folds = NULL,
#         cv_mode = "temporal",
#         tune_mode = "grid",
#         tune_grid_in = mlp_grid,
#         tune_grid_size = 2,
#         learn_rate = 0.1,
#         yvar = "Arithmetic.Mean",
#         xvar = seq(5, ncol(dt_long)),
#         nthreads = 1,
#         trim_resamples = FALSE,
#         return_best = TRUE
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
#   # expect base predictions have more than 1 value
#   # will be updated for SD/variance checks but hard with small sample
#   testthat::expect_true(
#     length(unique(mlp4$base_prediction$.pred)) > 1
#   )


#   # spatial
#   # warning is due to 3 metrcis (rmse, rsq, mae)
#   args_spatial = list(
#     target_cols = c("lon", "lat"),
#     cv_make_fun = spatialsample::spatial_block_cv,
#     v = 4
#   )
#   testthat::expect_warning(
#     mlp5 <- fit_base_learner(
#         learner = "mlp",
#         dt_full = dt_long,
#         r_subsample = 0.3,
#         model = mlp_model,
#         args_generate_cv = args_spatial,
#         folds = NULL,
#         cv_mode = "temporal",
#         tune_mode = "grid",
#         tune_grid_in = mlp_grid,
#         tune_grid_size = 2,
#         learn_rate = 0.1,
#         yvar = "Arithmetic.Mean",
#         xvar = seq(5, ncol(dt_long)),
#         nthreads = 1,
#         trim_resamples = FALSE,
#         return_best = TRUE
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
#   # expect base predictions have more than 1 value
#   # will be updated for SD/variance checks but hard with small sample
#   testthat::expect_true(
#     length(unique(mlp5$base_prediction$.pred)) > 1
#   )


#   # spatiotemporal
#   # warning is due to 3 metrcis (rmse, rsq, mae)
#   args_spatiotemporal <- list(
#     target_cols = c("lon", "lat", "time"),
#     preprocessing = "none",
#     ngroup_init = 2L,
#     cv_pairs = NULL,
#     pairing = "1"
#   )
#   testthat::expect_warning(
#     mlp6 <- fit_base_learner(
#         learner = "mlp",
#         dt_full = dt_long,
#         r_subsample = 0.3,
#         model = mlp_model,
#         args_generate_cv = args_spatiotemporal,
#         folds = NULL,
#         cv_mode = "temporal",
#         tune_mode = "grid",
#         tune_grid_in = mlp_grid,
#         tune_grid_size = 2,
#         learn_rate = 0.1,
#         yvar = "Arithmetic.Mean",
#         xvar = seq(5, ncol(dt_long)),
#         nthreads = 1,
#         trim_resamples = FALSE,
#         return_best = TRUE
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
#   # expect base predictions have more than 1 value
#   # will be updated for SD/variance checks but hard with small sample
#   testthat::expect_true(
#     length(unique(mlp6$base_prediction$.pred)) > 1
#   )
# })

##### elnet
testthat::test_that("fit_base_learner (elnet + folds + grid)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_long <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_long.rds")
  )


  # set model
  elnet_model <- switch_model("elnet")
  # set grid
  elnet_grid <- expand.grid(
    mixture = seq(0, 1, length.out = 21),
    penalty = 10 ^ seq(-3, 5, 1)
  )


  # temporal
  # warning is due to 3 metrcis (rmse, rsq, mae)
  testthat::expect_warning(
    elnet1 <- fit_base_learner(
        learner = "elnet",
        dt_full = dt_long,
        r_subsample = 0.3,
        model = elnet_model,
        folds = 5L,
        cv_mode = "temporal",
        tune_mode = "grid",
        tune_grid_in = elnet_grid,
        tune_grid_size = 2,
        learn_rate = 0.1,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_long)),
        nthreads = 1,
        trim_resamples = FALSE,
        return_best = TRUE
    )
  )
  # expect a list
  testthat::expect_true(is.list(elnet1))
  # expect length 3
  testthat::expect_length(elnet1, 3)
  # expect sub-items are tibble data.frames
  testthat::expect_equal(
    unlist(lapply(1:3, function(x) methods::is(elnet1[[x]], "tbl_df"))),
    c(TRUE, TRUE, TRUE)
  )
  # expect base predictions are numeric
  testthat::expect_true(is.numeric(elnet1$base_prediction$.pred))
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(
  #   length(unique(elnet1$base_prediction$.pred)) > 1
  # )


  # spatial
  # warning is due to 3 metrcis (rmse, rsq, mae)
  testthat::expect_warning(
    elnet2 <- fit_base_learner(
        learner = "elnet",
        dt_full = dt_long,
        r_subsample = 0.3,
        model = elnet_model,
        folds = 5L,
        cv_mode = "temporal",
        tune_mode = "grid",
        tune_grid_in = elnet_grid,
        tune_grid_size = 2,
        learn_rate = 0.1,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_long)),
        nthreads = 1,
        trim_resamples = FALSE,
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
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(
  #   length(unique(elnet2$base_prediction$.pred)) > 1
  # )


  # spatiotemporal
  # warning is due to 3 metrcis (rmse, rsq, mae)
  testthat::expect_warning(
    elnet3 <- fit_base_learner(
        learner = "elnet",
        dt_full = dt_long,
        r_subsample = 0.3,
        model = elnet_model,
        folds = 5L,
        cv_mode = "temporal",
        tune_mode = "grid",
        tune_grid_in = elnet_grid,
        tune_grid_size = 2,
        learn_rate = 0.1,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_long)),
        nthreads = 1,
        trim_resamples = FALSE,
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
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  # testthat::expect_true(
  #   length(unique(elnet3$base_prediction$.pred)) > 1
  # )
})


testthat::test_that("fit_base_learner (elnet + folds + bayes)", {
  # import sample data
  # sample inlcudes 2 months data for 3 sites
  # subset to only 50 predictors for light weight
  dt_long <- readRDS(
    testthat::test_path("..", "testdata", "base", "dt_long.rds")
  )


  # set model
  elnet_model <- switch_model("elnet")


  # temporal
  # warning is due to 3 metrcis (rmse, rsq, mae)
  testthat::expect_warning(
    elnet4 <- fit_base_learner(
        learner = "elnet",
        dt_full = dt_long,
        r_subsample = 0.3,
        model = elnet_model,
        folds = 5L,
        cv_mode = "temporal",
        tune_mode = "bayes",
        tune_bayes_iter = 2,
        learn_rate = 0.1,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_long)),
        nthreads = 1,
        trim_resamples = FALSE,
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
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  testthat::expect_true(
    length(unique(elnet4$base_prediction$.pred)) > 1
  )


  # spatial
  # warning is due to 3 metrcis (rmse, rsq, mae)
  testthat::expect_warning(
    elnet5 <- fit_base_learner(
        learner = "elnet",
        dt_full = dt_long,
        r_subsample = 0.3,
        model = elnet_model,
        folds = 5L,
        cv_mode = "temporal",
        tune_mode = "bayes",
        tune_bayes_iter = 2,
        learn_rate = 0.1,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_long)),
        nthreads = 1,
        trim_resamples = FALSE,
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
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  testthat::expect_true(
    length(unique(elnet5$base_prediction$.pred)) > 1
  )


  # spatiotemporal
  # warning is due to 3 metrcis (rmse, rsq, mae)
  testthat::expect_warning(
    elnet6 <- fit_base_learner(
        learner = "elnet",
        dt_full = dt_long,
        r_subsample = 0.3,
        model = elnet_model,
        folds = 5L,
        cv_mode = "temporal",
        tune_mode = "bayes",
        tune_bayes_iter = 2,
        learn_rate = 0.1,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_long)),
        nthreads = 1,
        trim_resamples = FALSE,
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
  # expect base predictions have more than 1 value
  # will be updated for SD/variance checks but hard with small sample
  testthat::expect_true(
    length(unique(elnet6$base_prediction$.pred)) > 1
  )
})
