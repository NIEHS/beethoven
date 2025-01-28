################################################################################
##### Fit {brulee}, {lightgbm}, and {elnet} models for base learners.
target_baselearner <-
  list(
    ############################################################################
    #########################             DEV             ######################
    targets::tar_target(
      dt_feat_calc_xyt_devsubset,
      command = dt_feat_calc_xyt[
        grep("2018|2019", dt_feat_calc_xyt$time),
        c(1:5, grep("lon|lat", names(dt_feat_calc_xyt)), 2065:2165)
      ],
      description = "Imputed features + AQS sites | dev"
    )
    ,
    ############################################################################
    ############################################################################
    targets::tar_target(
      df_learner_type_cpu,
      command = beethoven::assign_learner_cv(
        learner = c("elnet"),
        # learner = c("elnet", "lgb"),
        cv_mode = c("spatial", "temporal", "spatiotemporal"),
        cv_rep = 1L,
        # cv_rep = 100L,
        num_device = 1L
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "Engines and CV modes | cpu | base learner"
    )
    ,
    targets::tar_target(
      df_learner_type_gpu,
      command = beethoven::assign_learner_cv(
        learner = c("mlp"),
        # learner = c("mlp", "xgb"),
        cv_mode = c("spatial", "temporal", "spatiotemporal"),
        cv_rep = 1L,
        # cv_rep = 100L,
        num_device = 1L
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "Engines and CV modes | gpu | base learner"
    )
    ,
    targets::tar_target(
      list_base_args_cv,
      command = list(
        spatial = list(
          target_cols = c("lon", "lat"),
          cv_make_fun = beethoven::generate_cv_index_sp,
          v = 10L,
          method = "snake"
        ),
        temporal = list(
          cv_fold = 10L,
          time_col = "time",
          window = 14L
        ),
        spatiotemporal = list(
          target_cols = c("lon", "lat", "time"),
          cv_make_fun = beethoven::generate_cv_index_spt,
          ngroup_init = 8L,
          cv_pairs = 10L,
          preprocessing = "normalize",
          pairing = "1"
        )
      ),
      description = "CV method arguments | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_candidates,
      command = list(
        mlp = expand.grid(
          hidden_units = list(
            1024, 512, 256, c(256, 256), c(256, 512),
            c(512, 256), c(512, 512), c(512, 1024),
            c(256, 256, 256), c(256, 512, 256)
          ),
          dropout = 1 / seq(5, 2, -1),
          activation = c("relu", "leaky_relu"),
          learn_rate = c(0.1, 0.05, 0.01, 0.005)
        ),
        elnet = expand.grid(
          mixture = seq(0, 1, length.out = 21),
          penalty = 10 ^ seq(-3, 5, 1)
        ),
        lgb = expand.grid(
          mtry = floor(
            c(0.025, seq(0.05, 0.2, 0.05)) * ncol(dt_feat_calc_xyt_devsubset)
          ),
          trees = seq(1000, 3000, 1000),
          learn_rate = c(0.1, 0.05, 0.01, 0.005)
        )
        # xgb = expand.grid(
        #   mtry = floor(
        #     c(0.025, seq(0.05, 0.2, 0.05)) * ncol(dt_feat_calc_xyt_devsubset)
        #   ),
        #   trees = seq(1000, 3000, 1000),
        #   learn_rate = c(0.1, 0.05, 0.01, 0.005)
        # )
      ),
      description = "Parameter tuning grid | base learner"
    )
    ,
    targets::tar_target(
      list_base_switch_model,
      command = list(
        mlp = beethoven::switch_model(
          model_type = "mlp",
          device = "cuda"
        ),
        elnet = beethoven::switch_model(
          model_type = "elnet",
          device = "cpu"
        ),
        lgb = beethoven::switch_model(
          model_type = "lgb",
          device = "gpu"
        )
        # xgb = beethoven::switch_model(
        #   model_type = "xgb",
        #   device = "cuda"
        # )
      ),
      description = "Engines and devices | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_static,
      command = list(
        r_subsample = 0.3,
        folds = NULL,
        tune_mode = "grid",
        tune_grid_size = 20L,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_feat_calc_xyt_devsubset)),
        nthreads = 2L,
        trim_resamples = TRUE,
        return_best = TRUE
      ),
      description = "Static parameters | base learner"
    )
    ,
    targets::tar_target(
      workflow_learner_base_cpu,
      command = beethoven::fit_base_learner(
        learner = df_learner_type_cpu$learner,
        dt_full = dt_feat_calc_xyt_devsubset,
        r_subsample = list_base_params_static$r_subsample,
        model = list_base_switch_model[[df_learner_type_cpu$learner]],
        folds = list_base_params_static$folds,
        cv_mode = df_learner_type_cpu$cv_mode,
        args_generate_cv = list_base_args_cv[[df_learner_type_cpu$cv_mode]],
        tune_mode = list_base_params_static$tune_mode,
        tune_grid_in =
          list_base_params_candidates[[df_learner_type_cpu$learner]],
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        nthreads = list_base_params_static$nthreads,
        trim_resamples = list_base_params_static$trim_resamples,
        return_best = list_base_params_static$return_best
      ),
      pattern = map(df_learner_type_cpu),
      iteration = "list",
      description = "Fit base learner | cpu | base learner"
    )
    ,
    targets::tar_target(
      workflow_learner_base_gpu,
      command = beethoven::fit_base_learner(
        learner = df_learner_type_gpu$learner,
        dt_full = dt_feat_calc_xyt_devsubset,
        r_subsample = list_base_params_static$r_subsample,
        model = list_base_switch_model[[df_learner_type_gpu$learner]],
        folds = list_base_params_static$folds,
        cv_mode = df_learner_type_gpu$cv_mode,
        args_generate_cv = list_base_args_cv[[df_learner_type_gpu$cv_mode]],
        tune_mode = list_base_params_static$tune_mode,
        tune_grid_in =
          list_base_params_candidates[[df_learner_type_gpu$learner]],
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        nthreads = list_base_params_static$nthreads,
        trim_resamples = list_base_params_static$trim_resamples,
        return_best = list_base_params_static$return_best
      ),
      pattern = map(df_learner_type_gpu),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_gpu")
      ),
      description = "Fit base learner | gpu | base learner"
    )
    ,
    targets::tar_target(
      workflow_learner_base_best,
      command = c(workflow_learner_base_cpu, workflow_learner_base_gpu),
      description = "Fit base learners | base learner"
    )
  )
