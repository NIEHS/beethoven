################################################################################
##### Set base learner types, cross validation methods, and {tune}-able
##### hyperparameters.
target_baselearner <-
  list(
    targets::tar_target(
      list_base_params_static,
      command = list(
        dt_full = dt_feat_pm_imputed,
        yvar = "Arithmetic.Mean",
        xvar = names(dt_feat_pm_imputed)[seq(5, ncol(dt_feat_pm_imputed))],
        drop_vars = names(dt_feat_pm_imputed)[seq(1, 3)],
        normalize = TRUE,
        num_base_models = 100L,
        metric = "mae",
        tune_grid_size = 10L,
        crs = 5070L,
        cellsize = 250000L,
        cvsize = 5L
      ),
      description = "Static parameters | base learner"
    ),
    targets::tar_target(
      num_cv_index,
      command = seq_len(list_base_params_static$num_base_models),
      description = "Index of CV list objects | base learner"
    ),
    targets::tar_target(
      list_rset_st_vfolds,
      command = {
        message(paste0("Generating `rset` fold ", num_cv_index))
        spatiotemporal_index <- beethoven::generate_cv_index_spt(
          data = list_base_params_static$dt_full,
          crs = list_base_params_static$crs,
          cellsize = list_base_params_static$cellsize,
          locs_id = "site_id",
          coords = c("lon", "lat"),
          v = list_base_params_static$cvsize,
          time_id = "time"
        )
        cv_rset <- beethoven::convert_cv_index_rset(
          cvindex = spatiotemporal_index,
          data = list_base_params_static$dt_full,
          cv_mode = "spatiotemporal"
        )
        return(cv_rset)
      },
      description = "MC vfold training sets | base learner",
      pattern = map(num_cv_index),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_10")
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
        rset = list_rset_st_vfolds,
        model = engine_base_elnet,
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        drop_vars = list_base_params_static$drop_vars,
        normalize = list_base_params_static$normalize,
        metric = list_base_params_static$metric
      ),
      pattern = map(list_rset_st_vfolds),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_10")
      ),
      description = "Fit base learner | brulee regression | cpu | base learner"
    ),
    targets::tar_target(
      list_learner_pred_elnet,
      command = {
        dt_pred_elnet <- fit_learner_base_elnet$predictions
        dt_pred_elnet_sub <- dt_pred_elnet[,
          c(".pred", ".row", "Arithmetic.Mean")
        ]
        names(dt_pred_elnet_sub) <- gsub(
          ".pred",
          sprintf("elnet_%05d", num_cv_index),
          names(dt_pred_elnet_sub)
        )
        dt_pred_elnet_sub
      },
      pattern = map(fit_learner_base_elnet, num_cv_index),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "elnet predictions as list | base learner"
    ),
    targets::tar_target(
      dt_learner_pred_elnet,
      command = beethoven::reduce_merge(
        list_learner_pred_elnet,
        by = c(".row", "Arithmetic.Mean")
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_5")
      ),
      description = "elnet predictions as data.table | base learner"
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
          parsnip::set_engine(
            "brulee",
            device = "cuda",
            early_stopping = TRUE
          ) %>%
          parsnip::set_mode("regression")
      },
      description = "Engine and device | mlp | base learner"
    ),
    targets::tar_target(
      fit_learner_base_mlp,
      command = beethoven::fit_base_learner(
        rset = list_rset_st_vfolds,
        model = engine_base_mlp,
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        drop_vars = list_base_params_static$drop_vars,
        normalize = list_base_params_static$normalize,
        metric = list_base_params_static$metric
      ),
      pattern = map(list_rset_st_vfolds),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_mlp")
      ),
      description = "Fit base learners | mlp | gpu | base learner"
    ),
    targets::tar_target(
      list_learner_pred_mlp,
      command = {
        dt_pred_mlp <- fit_learner_base_mlp$predictions
        dt_pred_mlp_sub <- dt_pred_mlp[,
          c(".pred", ".row", "Arithmetic.Mean")
        ]
        names(dt_pred_mlp_sub) <- gsub(
          ".pred",
          sprintf("mlp_%05d", num_cv_index),
          names(dt_pred_mlp_sub)
        )
        dt_pred_mlp_sub
      },
      pattern = map(fit_learner_base_mlp, num_cv_index),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "mlp predictions as list | base learner"
    ),
    targets::tar_target(
      dt_learner_pred_mlp,
      command = beethoven::reduce_merge(
        list_learner_pred_mlp,
        by = c(".row", "Arithmetic.Mean")
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_5")
      ),
      description = "mlp predictions as data.table | base learner"
    ),
    ################################################################################
    ##### Development work with 2-layer {brulee} mlp.
    targets::tar_target(
      fit_learner_base_mlp2,
      command = {
        df_mlp_grid <- expand.grid(
          hidden_units = list(
            256,
            512,
            c(256, 256),
            c(256, 512),
            c(128, 256, 512),
            c(128, 256, 128),
            c(256, 512, 256)
          ),
          dropout = seq(0.1, 0.5, 0.05),
          learn_rate = seq(0.0025, 0.01, 0.0015)
        )
        dt_mlp_grid_sample <- df_mlp_grid[
          sample(nrow(df_mlp_grid), list_base_params_static$tune_grid_size),
        ]

        engine_base_mlp2 <- parsnip::mlp(
          hidden_units = parsnip::tune(),
          dropout = parsnip::tune(),
          epochs = 1000,
          activation = "leaky_relu",
          learn_rate = parsnip::tune(),
          penalty = 1e-06
        ) %>%
          parsnip::set_engine(
            "brulee",
            device = "cuda",
            early_stopping = TRUE
          ) %>%
          parsnip::set_mode("regression")

        fit_learner_mlp2 <- beethoven::fit_base_learner(
          rset = list_rset_st_vfolds,
          model = engine_base_mlp2,
          tune_grid_size = dt_mlp_grid_sample,
          yvar = list_base_params_static$yvar,
          xvar = list_base_params_static$xvar,
          drop_vars = list_base_params_static$drop_vars,
          normalize = list_base_params_static$normalize,
          metric = list_base_params_static$metric
        )
      },
      pattern = map(list_rset_st_vfolds),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_mlp")
      ),
      description = "Fit base learners | mlp 2 layer | gpu | base learner"
    ),
    targets::tar_target(
      list_learner_pred_mlp2,
      command = {
        dt_pred_mlp2 <- fit_learner_base_mlp2$predictions
        dt_pred_mlp2_sub <- dt_pred_mlp2[,
          c(".pred", ".row", "Arithmetic.Mean")
        ]
        names(dt_pred_mlp2_sub) <- gsub(
          ".pred",
          sprintf("mlp2_%05d", num_cv_index),
          names(dt_pred_mlp2_sub)
        )
        dt_pred_mlp2_sub
      },
      pattern = map(fit_learner_base_mlp2, num_cv_index),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "mlp predictions as list | base learner"
    ),
    targets::tar_target(
      dt_learner_pred_mlp2,
      command = beethoven::reduce_merge(
        list_learner_pred_mlp2,
        by = c(".row", "Arithmetic.Mean")
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_5")
      ),
      description = "mlp predictions as data.table | base learner"
    )
  )

################################################################################
##### Fit CPU-enabled {lightGBM} base learners on {gpu:gn040809} cluster.
target_baselearner_lgb <-
  list(
    targets::tar_target(
      fit_learner_base_lgb,
      command = {
        int_lgb_threads <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
        engine_base_lgb <- parsnip::boost_tree(
          mtry = parsnip::tune(),
          trees = parsnip::tune(),
          learn_rate = parsnip::tune(),
          tree_depth = parsnip::tune()
        ) %>%
          parsnip::set_engine(
            "lightgbm",
            device = "cpu",
            num_threads = int_lgb_threads
          ) %>%
          parsnip::set_mode("regression")
        beethoven::fit_base_learner(
          rset = list_rset_st_vfolds,
          model = engine_base_lgb,
          tune_grid_size = list_base_params_static$tune_grid_size,
          yvar = list_base_params_static$yvar,
          xvar = list_base_params_static$xvar,
          drop_vars = list_base_params_static$drop_vars,
          normalize = list_base_params_static$normalize,
          metric = list_base_params_static$metric
        )
      },
      pattern = map(list_rset_st_vfolds),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_lgb")
      ),
      description = "Fit base learner | lgb | cpu | base learner"
    ),
    targets::tar_target(
      list_learner_pred_lgb,
      command = {
        dt_pred_lgb <- fit_learner_base_lgb$predictions
        dt_pred_lgb_sub <- dt_pred_lgb[,
          c(".pred", ".row", "Arithmetic.Mean")
        ]
        names(dt_pred_lgb_sub) <- gsub(
          ".pred",
          sprintf("lgb_%05d", num_cv_index),
          names(dt_pred_lgb_sub)
        )
        dt_pred_lgb_sub
      },
      pattern = map(fit_learner_base_lgb, num_cv_index),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "lgb predictions as list | base learner"
    ),
    targets::tar_target(
      dt_learner_pred_lgb,
      command = beethoven::reduce_merge(
        list_learner_pred_lgb,
        by = c(".row", "Arithmetic.Mean")
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_5")
      ),
      description = "lgb predictions as data.table | base learner"
    )
  )
