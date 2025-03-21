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
      list_base_params_static,
      command = list(
        dt_full = dt_feat_calc_xyt,
        r_subsample = 0.3,
        c_subsample = 1.0,
        folds = 10L,
        tune_mode = "grid",
        tune_grid_size = 1L,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_feat_calc_xyt)),
        normalize = TRUE,
        trim_resamples = TRUE,
        return_best = TRUE,
        workflow = TRUE,
        ##### NOTE: exclude workflow for base to meta learner dev.
        ##### will need to be included to predict base learner values
        ##### on prediction grid.
        cv_rep = 10L
      ),
      description = "Static parameters | base learner"
    )
  )

################################################################################
##### Fit CPU-enabled {elnet} base learners on {geo} cluster.
target_baselearner_elnet <-
  list(
    targets::tar_target(
      engine_base_elnet,
      command = beethoven::switch_model(
        model_type = "elnet",
        device = "cpu"
      ),
      description = "Engine and device | elnet | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_elnet,
      command = list(
        elnet = expand.grid(
          mixture = (0.1, 0.5, 1),
          penalty = double(1)
        )
      ),
      description = "tuning grid | elnet | base learner"
    )
    ,
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
        model = engine_base_elnet,
        folds = list_base_params_static$folds,
        cv_mode = df_learner_type_elnet$cv_mode,
        args_generate_cv = list_base_args_cv[[df_learner_type_elnet$cv_mode]],
        tune_mode = list_base_params_static$tune_mode,
        tune_grid_in = list_base_params_elnet[[df_learner_type_elnet$learner]],
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        normalize = list_base_params_static$normalize,
        trim_resamples = list_base_params_static$trim_resamples,
        return_best = list_base_params_static$return_best,
        workflow = list_base_params_static$workflow
      ),
      pattern = map(df_learner_type_elnet),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Fit base learner | elnet | cpu | base learner"
    )
  )
    
################################################################################
##### Fit CPU-enabled {lightGBM} base learners on {normal} cluster.
target_baselearner_lgb <-
  list(
    targets::tar_target(
      engine_base_lgb,
      command = beethoven::switch_model(
        model_type = "lgb",
        device = "cpu"
      ),
      description = "Engine and device | lgb | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_lgb,
      command = list(
        lgb = expand.grid(
          mtry = c(floor(0.25 * (ncol(dt_feat_calc_xyt) - 4)), floor(0.5 * (ncol(dt_feat_calc_xyt) - 4))),
          trees = c(100,500, 1000),
          learn_rate = c(0.05, 0.1),
          tree_depth = c(10, 20)
        )
      ),
      description = "tuning grid | lgb | base learner"
    )
    ,
    targets::tar_target(
      df_learner_type_lgb,
      command = beethoven::assign_learner_cv(
        learner = c("lgb"),
        ##### NOTE: {lgb} max ~13.2 Gb memory.
        cv_mode = "spatiotemporal",
        cv_rep = list_base_params_static$cv_rep,
        crs = 5070L,
        cellsize = 250000L,
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
        model = engine_base_lgb,
        cv_mode = df_learner_type_lgb$cv_mode,
        args_generate_cv = list_base_args_cv[[df_learner_type_lgb$cv_mode]],
        tune_mode = list_base_params_static$tune_mode,
        tune_grid_in = list_base_params_lgb[[df_learner_type_lgb$learner]],
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        normalize = list_base_params_static$normalize,
        trim_resamples = list_base_params_static$trim_resamples,
        return_best = list_base_params_static$return_best,
        workflow = list_base_params_static$workflow
      ),
      pattern = map(df_learner_type_lgb),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_1")
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
      command = beethoven::switch_model(
        model_type = "mlp",
        device = "cuda"
      ),
      description = "Engine and device | mlp | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_mlp,
      command = list(
        mlp = expand.grid(
          hidden_units = list(128),
          dropout = c(0.1, 0.3),
          activation = "relu",
          learn_rate = c(0.001, 0.1)
        )
      ),
      description = "tuning grid | mlp | base learner"
    )
    ,
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
        model = engine_base_mlp,
        folds = list_base_params_static$folds,
        cv_mode = df_learner_type_mlp$cv_mode,
        args_generate_cv = list_base_args_cv[[df_learner_type_mlp$cv_mode]],
        tune_mode = list_base_params_static$tune_mode,
        tune_grid_in = list_base_params_mlp[[df_learner_type_mlp$learner]],
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = list_base_params_static$xvar,
        normalize = list_base_params_static$normalize,
        trim_resamples = list_base_params_static$trim_resamples,
        return_best = list_base_params_static$return_best,
        workflow = list_base_params_static$workflow
      ),
      pattern = map(df_learner_type_mlp),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_geo")
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
      format = "file_fast",
      description = "All fit base learners | cpu | gpu | base learner"
    )
  )
