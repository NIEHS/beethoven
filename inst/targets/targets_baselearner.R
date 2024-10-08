
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
        # DEVELOPMENT CHANGE mm-0829
        # learner = c("xgb", "mlp", "elnet"),
        learner = c("elnet"),
        # DEVELOPMENT CHANGE mm-0829
        cv_mode = c("spatial", "temporal", "spatiotemporal"),
        # cv_mode = c("spatiotemporal"),
        # DEVELOPMENT CHANGE mm-0829
        # cv_rep = 100L,
        cv_rep = 1L,
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
        # DEVELOPMENT CHANGE mm-0829
        # lgb =
        #   expand.grid(
        #     mtry = floor(c(0.025, seq(0.05, 0.2, 0.05)) * 2000L),
        #     trees = seq(1000, 3000, 1000),
        #     learn_rate = c(0.1, 0.05, 0.01, 0.005)
        #   )
        # ,
        # xgb =
        #   expand.grid(
        #     mtry = floor(c(0.025, seq(0.05, 0.2, 0.05)) * 2000L),
        #     trees = seq(1000, 3000, 1000),
        #     learn_rate = c(0.1, 0.05, 0.01, 0.005)
        #   )
        # ,
        # mlp = 
        #   expand.grid(
        #     # hidden_units = c(1024, 512, 256, 128, 64),
        #     hidden_units = list(
        #       c(256, 256), c(256, 512),
        #       c(512, 512), c(512, 1024),
        #       c(256, 512, 1024)
        #     ),
        #     dropout = 1 / seq(5, 2, -1),
        #     activation = c("relu", "leaky_relu"),
        #     learn_rate = c(0.1, 0.05, 0.01, 0.005)
        #   )
        # ,
        elnet =
          expand.grid(
            # 0.05 step, 0 through 1
            mixture = seq(0, 1, length.out = 21),
            penalty = 10 ^ seq(-3, 5, 1)
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
          # DEVELOPMENT CHANGE mm-0903
          # tune_grid_size = 10L,
          # nthreads = 10L
          tune_grid_size = 2L,
          nthreads = 2L,
          # trim resamples for lightweight base learners
          trim_resamples = TRUE
        ),
      pattern = map(df_learner_type),
      iteration = "list",
      resources = set_slurm_resource(ncpus = 10L, memory = 8L, partition = "geo")
    )
    
  )