
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
      ),
      iteration = "vector"
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
          args_generate_cv = list_base_args_cv[[df_learner_type$cv_mode]]
        ),
      pattern = map(df_learner_type),
      iteration = "list",
      resources = set_slurm_resource(ncpus = 12L, memory = 20L, partition = "geo")
    )
  )