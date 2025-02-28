################################################################################
##### Set base learner types, cross validation methods, and {tune}-able
##### hyperparameters.
target_baselearner <-
  list(
    targets::tar_target(
      list_base_args_cv,
      command = list(spatiotemporal = list(v = 10L)),
      description = "CV method arguments | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_elnet,
      command = list(
        elnet = expand.grid(
          mixture = 0.5,
          penalty = 0.01
        )
      ),
      description = "tuning grid | elnet | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_lgb,
      command = list(
        lgb = expand.grid(
          mtry = floor(0.25 * (ncol(dt_feat_calc_xyt) - 4)),
          trees = 500,
          learn_rate = 0.05
        )
      ),
      description = "tuning grid | lgb | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_mlp,
      command = list(
        mlp = expand.grid(
          hidden_units = list(64, 128),
          dropout = c(0.1, 0.2),
          activation = "relu",
          learn_rate = c(0.001, 0.005)
        )
      ),
      description = "tuning grid | mlp | base learner"
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
          device = "cpu"
        )
      ),
      description = "Engines and devices | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_static,
      command = list(
        dt_full = dt_feat_calc_xyt,
        r_subsample = 0.3,
        c_subsample = 1.0,
        folds = NULL,
        tune_mode = "grid",
        tune_grid_size = 1L,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_feat_calc_xyt)),
        trim_resamples = TRUE,
        return_best = TRUE,
        cv_rep = 1L
      ),
      description = "Static parameters | base learner"
    )
  )

################################################################################
##### Fit CPU-enabled {elnet} and {lightgbm} base learners.
target_baselearner_cpu <-
  list(
    targets::tar_target(
      df_learner_type_elnet,
      command = beethoven::assign_learner_cv(
        learner = c("elnet"),
        ##### NOTE: {elnet} max ~14.8 Gb memory
        cv_mode = "spatiotemporal",
        cv_rep = list_base_params_static$cv_rep,
        num_device = 1L
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "Engines and CV modes | elnet | cpu | base learner"
    )
    ,
    targets::tar_target(
      fit_learner_base_elnet,
      command = beethoven::fit_base_learner(
        learner = df_learner_type_elnet$learner,
        dt_full = list_base_params_static$dt_full,
        r_subsample = list_base_params_static$r_subsample,
        c_subsample = list_base_params_static$c_subsample,
        model = list_base_switch_model[[df_learner_type_elnet$learner]],
        folds = list_base_params_static$folds,
        cv_mode = df_learner_type_elnet$cv_mode,
        args_generate_cv = list_base_args_cv[[df_learner_type_elnet$cv_mode]],
        tune_mode = list_base_params_static$tune_mode,
        tune_grid_in = list_base_params_elnet[[df_learner_type_elnet$learner]],
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        trim_resamples = list_base_params_static$trim_resamples,
        return_best = list_base_params_static$return_best
      ),
      pattern = map(df_learner_type_elnet),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Fit base learner | elnet | cpu | base learner"
    )
    ,
    targets::tar_target(
      df_learner_type_lgb,
      command = beethoven::assign_learner_cv(
        learner = c("lgb"),
        ##### NOTE: {lgb} max ~13.2 Gb memory.
        cv_mode = "spatiotemporal",
        cv_rep = list_base_params_static$cv_rep,
        num_device = 1L
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "Engines and CV modes | lgb | cpu | base learner"
    )
    ,
    targets::tar_target(
      fit_learner_base_lgb,
      command = beethoven::fit_base_learner(
        learner = df_learner_type_lgb$learner,
        dt_full = list_base_params_static$dt_full,
        r_subsample = list_base_params_static$r_subsample,
        c_subsample = list_base_params_static$c_subsample,
        model = list_base_switch_model[[df_learner_type_lgb$learner]],
        folds = list_base_params_static$folds,
        cv_mode = df_learner_type_lgb$cv_mode,
        args_generate_cv = list_base_args_cv[[df_learner_type_lgb$cv_mode]],
        tune_mode = list_base_params_static$tune_mode,
        tune_grid_in = list_base_params_lgb[[df_learner_type_lgb$learner]],
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        trim_resamples = list_base_params_static$trim_resamples,
        return_best = list_base_params_static$return_best
      ),
      pattern = map(df_learner_type_lgb),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Fit base learner | lgb | cpu | base learner"
    )
  )

################################################################################
##### Fit GPU-enabled {brulee} base learners.
target_baselearner_gpu <-
  list(
    targets::tar_target(
      df_learner_type_mlp,
      command = beethoven::assign_learner_cv(
        learner = c("mlp"),
        cv_mode = "spatiotemporal",
        cv_rep = list_base_params_static$cv_rep,
        num_device = 4L
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "Engines and CV modes | gpu | base learner"
    )
    ,
    targets::tar_target(
      fit_learner_base_mlp,
      command = beethoven::fit_base_learner(
        learner = df_learner_type_mlp$learner,
        dt_full = list_base_params_static$dt_full,
        r_subsample = list_base_params_static$r_subsample,
        c_subsample = list_base_params_static$c_subsample,
        model = list_base_switch_model[[df_learner_type_mlp$learner]],
        folds = list_base_params_static$folds,
        cv_mode = df_learner_type_mlp$cv_mode,
        args_generate_cv = list_base_args_cv[[df_learner_type_mlp$cv_mode]],
        tune_mode = list_base_params_static$tune_mode,
        tune_grid_in = list_base_params_mlp[[df_learner_type_mlp$learner]],
        tune_grid_size = nrow(list_base_params_mlp$mlp),
        # tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        trim_resamples = list_base_params_static$trim_resamples,
        return_best = list_base_params_static$return_best
      ),
      pattern = map(df_learner_type_mlp),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_gpu")
      ),
      description = "Fit base learners | mlp | gpu | base learner"
    )
    ,
    targets::tar_target(
      list_learner_base_best,
      command = c(
        fit_learner_base_elnet,
        fit_learner_base_lgb,
        fit_learner_base_mlp
      ),
      description = "All fit base learners | cpu | gpu | base learner"
    )
  )
