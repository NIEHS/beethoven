################################################################################
##### Set base learner types, cross validation methods, and {tune}-able
##### hyperparameters.
target_baselearner <-
  list(
    targets::tar_target(
      list_base_params_static,
      command = list(
        dt_full = dt_feat_calc_xyt,
        r_subsample = 3,
        yvar = "Arithmetic.Mean",
        xvar = names(dt_feat_calc_xyt)[seq(5, ncol(dt_feat_calc_xyt))],
        drop_vars = names(dt_feat_calc_xyt)[seq(1, 3)],
        normalize = TRUE,
        num_base_models = 20L,
        metric = "rmse",
        tune_grid_size = 10L,
        crs = 5070L,
        cellsize = 250000L,
        cvsize = 5L
      ),
      description = "Static parameters | base learner"
    ),
    targets::tar_target(
      list_cv_rsplit,
      command = {
        outer_cv <- rsample::vfold_cv(
          list_base_params_static$dt_full,
          v = list_base_params_static$r_subsample,
          repeats = list_base_params_static$num_base_models
        ) |>
          pull(splits) |>
          as.list()
        return(outer_cv)
      },
      description = "MC vfold rsets | base learner"
    ),
    targets::tar_target(
      num_cv_index,
      command = seq_len(length(list_cv_rsplit)),
      description = "Index of CV list objects | base learner"
    ),
    targets::tar_target(
      list_cv_rsplit_buffer,
      command = list_cv_rsplit[[num_cv_index]],
      description = "Re-index for dynamic branching | base learner",
      iteration = "list",
      pattern = map(num_cv_index),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_5")
      ),
    ),
    targets::tar_target(
      list_dt_test,
      command = rsample::training(list_cv_rsplit_buffer),
      ###### NOTE: we are switching training and testing sets.
      description = "MC vfold testing sets | base learner",
      pattern = map(list_cv_rsplit_buffer),
      iteration = "list"
    ),
    targets::tar_target(
      list_rset_train,
      command = {
        ###### NOTE: we are switching training and testing sets.
        dt_train <- rsample::assessment(list_cv_rsplit_buffer)
        spatiotemporal_index <- beethoven::generate_cv_index_spt(
          data = dt_train,
          crs = list_base_params_static$crs,
          cellsize = list_base_params_static$cellsize,
          locs_id = "site_id",
          coords = c("lon", "lat"),
          v = list_base_params_static$cvsize,
          time_id = "time"
        )
        inner_cv <- beethoven::convert_cv_index_rset(
          cvindex = spatiotemporal_index,
          data = dt_train,
          cv_mode = "spatiotemporal"
        )
        return(inner_cv)
      },
      description = "MC vfold training sets | base learner",
      pattern = map(list_cv_rsplit_buffer),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      )
    )
  )

################################################################################
##### Fit CPU-enabled {elnet} base learners on {geo} cluster.
target_baselearner_elnet <-
  list(
    targets::tar_target(
      engine_base_elnet,
      command = {
        parsnip::linear_reg(
          mixture = parsnip::tune(),
          penalty = parsnip::tune()
        ) %>%
          parsnip::set_engine("brulee", device = "cpu") %>%
          parsnip::set_mode("regression")
      },
      description = "Engine and device | elnet | base learner"
    ),
    targets::tar_target(
      fit_learner_base_elnet,
      command = beethoven::fit_base_learner(
        rset = list_rset_train,
        model = engine_base_elnet,
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        drop_vars = list_base_params_static$drop_vars,
        normalize = list_base_params_static$normalize
      ),
      pattern = map(list_rset_train),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_10")
      ),
      description = "Fit base learner | elnet | brulee linear regression | cuda | base learner"
    )
  )

################################################################################
##### Fit CPU-enabled {lightGBM} base learners on {geo} cluster.
target_baselearner_lgb <-
  list(
    targets::tar_target(
      engine_base_lgb,
      command = {
        parsnip::boost_tree(
          mtry = 239,
          trees = 445,
          learn_rate = parsnip::tune(),
          tree_depth = 7
        ) %>%
          parsnip::set_engine("lightgbm", device = "cpu") %>%
          parsnip::set_mode("regression")
      },
      description = "Engine and device | lgb | base learner"
    ),
    targets::tar_target(
      fit_learner_base_lgb,
      command = beethoven::fit_base_learner(
        rset = list_rset_train,
        model = engine_base_lgb,
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        drop_vars = list_base_params_static$drop_vars,
        normalize = list_base_params_static$normalize
      ),
      pattern = map(list_rset_train),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_sequential")
      ),
      description = "Fit base learner | lgb | cpu | base learner"
    )
  )

################################################################################
##### Fit GPU-enabled {brulee} base learners on {geo} cluster.
target_baselearner_mlp <-
  list(
    targets::tar_target(
      engine_base_mlp,
      command = {
        parsnip::mlp(
          hidden_units = c(parsnip::tune(), 32L),
          dropout = c(parsnip::tune(), 0.3),
          epochs = 1000,
          activation = "leaky_relu",
          learn_rate = parsnip::tune()
        ) %>%
          parsnip::set_engine("brulee", device = "cuda") %>%
          parsnip::set_mode("regression")
      },
      description = "Engine and device | mlp | base learner"
    ),
    targets::tar_target(
      fit_learner_base_mlp,
      command = beethoven::fit_base_learner(
        rset = list_rset_train,
        model = engine_base_mlp,
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        drop_vars = list_base_params_static$drop_vars,
        normalize = list_base_params_static$normalize
      ),
      pattern = map(list_rset_train),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_geo")
      ),
      description = "Fit base learners | mlp | gpu | base learner"
    )
  )
