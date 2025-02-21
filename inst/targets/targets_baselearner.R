################################################################################
##### Set base learner types, cross validation methods, and {tune}-able
##### hyperparameters.
target_baselearner <-
  list(
    # targets::tar_target(
    #   dt_feat_calc_imputed2,
    #   command = beethoven::impute_all(
    #     dt_feat_calc_design,
    #     period = chr_daterange,
    #     nthreads_dt = 32,
    #     nthreads_collapse = 32,
    #     nthreads_imputation = 32
    #   ),
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_impute")
    #   ),
    #   description = "Imputed features + lags | fit"
    # )
    # ,
    targets::tar_target(
      list_base_args_cv,
      command = list(
        spatiotemporal = list(v = 3L)
      ),
      description = "CV method arguments | base learner"
    )
    ,
    targets::tar_target(
      list_base_params_candidates,
      command = list(
        mlp = expand.grid(
          hidden_units = c(1024, 512, 256, 128, 64),
          dropout = 1 / seq(5, 2, -1),
          activation = c("relu", "leaky_relu"),
          learn_rate = c(0.01, 0.005, 0.001, 0.0005)
        ),
        elnet = expand.grid(
          mixture = seq(0, 1, length.out = 21),
          penalty = 10 ^ seq(-3, 5, 1)
        ),
        lgb = expand.grid(
          mtry = floor(
            c(0.025, seq(0.05, 0.2, 0.05)) * ncol(dt_feat_calc_xyt)
          ),
          trees = seq(1000, 3000, 1000),
          learn_rate = c(0.1, 0.05, 0.01, 0.005)
        )
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
        tune_grid_size = 20L,
        yvar = "Arithmetic.Mean",
        xvar = seq(5, ncol(dt_feat_calc_xyt)),
        nthreads = 2L,
        trim_resamples = TRUE,
        return_best = TRUE,
        cv_rep = 100L
      ),
      description = "Static parameters | base learner"
    )
  )

################################################################################
##### Fit CPU-enabled {elnet} and {lightgbm} base learners.
target_baselearner_cpu <-
  list(
    targets::tar_target(
      df_learner_type_cpu,
      command = beethoven::assign_learner_cv(
        learner = c("elnet", "lgb"),
        cv_mode = "spatiotemporal",
        cv_rep = list_base_params_static$cv_rep,
        num_device = 1L
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "Engines and CV modes | cpu | base learner"
    )
    ,
    targets::tar_target(
      fit_learner_base_cpu,
      command = beethoven::fit_base_learner(
        learner = df_learner_type_cpu$learner,
        dt_full = list_base_params_static$dt_full,
        r_subsample = list_base_params_static$r_subsample,
        c_subsample = list_base_params_static$c_subsample,
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
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_10")
      ),
      description = "Fit base learner | cpu | base learner"
    )
  )

################################################################################
##### Fit GPU-enabled {brulee} base learners.
target_baselearner_gpu <-
  list(
    targets::tar_target(
      df_learner_type_gpu,
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
      fit_learner_base_gpu,
      command = beethoven::fit_base_learner(
        learner = df_learner_type_gpu$learner,
        dt_full = list_base_params_static$dt_full,
        r_subsample = list_base_params_static$r_subsample,
        c_subsample = list_base_params_static$c_subsample,
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
      list_learner_base_best,
      command = c(fit_learner_base_cpu, fit_learner_base_gpu),
      description = "All fit base learners | cpu | gpu | base learner"
    )
  )
