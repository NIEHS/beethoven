
target_baselearner <-
  list(
    targets::tar_target(
      name = dt_feat_calc_xyt,
      attach_xy(dt_feat_calc_imputed, sf_feat_proc_aqs_sites)
    )
    ,
    targets::tar_target(
      name = df_learner_type,
      command = assign_learner_cv(
        learner = c("lgb", "mlp", "elnet"),
        cv_mode = c("spatial", "temporal", "spatiotemporal"),
        cv_rep = 100L,
        num_device = 2L
      ) %>%
      split(seq_len(nrow(.))),
      iteration = "list"
    )
    ,
    targets::tar_target(
      name = list_base_args_cv,
      command = list(
        spatial = list(
          target_cols = c("lon", "lat"),
          cv_make_fun = generate_cv_index_sp,
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
          cv_make_fun = generate_cv_index_spt,
          ngroup_init = 8L,
          cv_pairs = 10L,
          preprocessing = "normalize",
          pairing = "1"
        )
      )
    )
    ,
    targets::tar_target(
      name = list_base_params_candidates,
      command = list(
        lgb =
          expand.grid(
            mtry = floor(c(0.05, 0.2, 0.05) * 2000L),
            trees = seq(1000, 3000, 500),
            learn_rate = c(0.1, 0.05, 0.01, 0.005)
          )        
        ,
        mlp = 
          expand.grid(
            hidden_units = c(1024, 512, 256, 128, 64, 32),
            dropout = 1 / seq(5, 2, -1),
            activation = c("relu"),
            learn_rate = c(0.1, 0.05, 0.01, 0.005)
          )
        ,
        elnet =
          expand.grid(
            mixture = seq(0, 1, length.out = 21),
            penalty = 10 ^ seq(-3, 5)
          )
      )
    )
    ,
    targets::tar_target(
      name = workflow_learner_base_best,
      command =
        fit_base_learner(
          learner = df_learner_type$learner,
          dt_full = dt_feat_calc_xyt,
          r_subsample = 0.3,
          model =
            switch_model(model_type = df_learner_type$learner,
                         device = df_learner_type$device),
          cv_mode = df_learner_type$cv_mode,
          args_generate_cv = list_base_args_cv[[df_learner_type$cv_mode]],
          tune_grid_in = list_base_params_candidates[[df_learner_type$learner]],
          # preferably match the number of threads to the random grid size.
          tune_grid_size = 10L,
          nthreads = 10L
        ),
      pattern = map(df_learner_type),
      iteration = "list",
      resources = set_slurm_resource(ncpus = 10L, memory = 8L, partition = "geo")
    )

  )