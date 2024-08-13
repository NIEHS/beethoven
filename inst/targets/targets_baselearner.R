
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
    # wf: workflow
    # lgb-spt-cv
    # length of 3600 (4 * 900)
    # targets::tar_target(
    #   workflow_learner_base_lgb,
    #   fit_base_lightgbm(
    #     dt_feat_calc_imputed,
    #     folds = list_learner_base_cv_spt,
    #     tune_mode = "bayes",
    #     tune_bayes_iter = 10L,
    #     learn_rate = num_learner_base_learn_device$rate,
    #     device = num_learner_base_learn_device$device
    #   ),
    #   pattern = cross(num_learner_base_learn_device, list_learner_base_cv_rset),
    #   resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    # )
    # ,
    # # length of 120
    # targets::tar_target(
    #   workflow_learner_base_lgb_spblock,
    #   fit_base_lightgbm(
    #     dt_feat_calc_imputed,
    #     folds = list_learner_base_cv_spblock,
    #     tune_mode = "grid",
    #     learn_rate = num_learner_base_learn_device$rate,
    #     device = num_learner_base_learn_device$device
    #   ),
    #   pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spblock),
    #   resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    # )
    # ,
    # # length of 120
    # targets::tar_target(
    #   workflow_learner_base_lgb_spcluster,
    #   fit_base_lightgbm(
    #     dt_feat_calc_imputed,
    #     folds = list_learner_base_cv_spcluster,
    #     tune_mode = "grid",
    #     learn_rate = num_learner_base_learn_device$rate,
    #     device = num_learner_base_learn_device$device
    #   ),
    #   pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spcluster),
    #   resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    # )
    # ,
    # mlp-cv: iterate by combination of rate+device and cv strategy
    # length of 3600
    # targets::tar_target(
    #   workflow_learner_base_mlp_spt,
    #   fit_base_brulee(
    #     dt_feat_calc_imputed,
    #     folds = list_learner_base_cv_spt,
    #     tune_mode = "grid",
    #     learn_rate = num_learner_base_learn_device$rate,
    #     device = num_learner_base_learn_device$device
    #   ),
    #   pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spt),
    #   resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    # )
    # ,
    # # length of 120
    # targets::tar_target(
    #   workflow_learner_base_mlp_spblock,
    #   fit_base_brulee(
    #     dt_feat_calc_imputed,
    #     folds = list_learner_base_cv_spblock,
    #     tune_mode = "grid",
    #     learn_rate = num_learner_base_learn_device$rate,
    #     device = num_learner_base_learn_device$device
    #   ),
    #   pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spblock),
    #   resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    # )
    # ,
    # # length of 120
    # targets::tar_target(
    #   workflow_learner_base_mlp_spcluster,
    #   fit_base_brulee(
    #     dt_feat_calc_imputed,
    #     folds = list_learner_base_cv_spcluster,
    #     tune_mode = "grid",
    #     learn_rate = num_learner_base_learn_device$rate,
    #     device = num_learner_base_learn_device$device
    #   ),
    #   pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spcluster),
    #   resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    # )
    # ,
    # elnet-cv is branched out only by subsamples.
    # length of 900
    # targets::tar_target(
    #   workflow_learner_base_elnet_spt,
    #   fit_base_elnet(
    #     dt_feat_calc_imputed,
    #     folds = list_learner_base_cv_rsets,
    #     nthreads = 32L
    #   ),
    #   pattern = map(list_learner_base_cv_rsets),
    #   iteration = "list",
    #   resources = set_slurm_resource(ncpus = 32L, memory = 8L, partition = "geo")
    # )
    # ,
    # # length of 30
    # targets::tar_target(
    #   workflow_learner_base_elnet_spblock,
    #   fit_base_elnet(
    #     dt_feat_calc_imputed,
    #     folds = list_learner_base_cv_spblock,
    #     nthreads = 32L
    #   ),
    #   pattern = map(list_learner_base_cv_spblock),
    #   iteration = "list",
    #   resources = set_slurm_resource(ncpus = 32L, memory = 8L, partition = "geo")
    # )
    # ,
    # # length of 30
    # targets::tar_target(
    #   workflow_learner_base_elnet_spcluster,
    #   fit_base_elnet(
    #     dt_feat_calc_imputed,
    #     folds = list_learner_base_cv_spcluster,
    #     nthreads = 32L
    #   ),
    #   pattern = map(list_learner_base_cv_spcluster),
    #   iteration = "list",
    #   resources = set_slurm_resource(ncpus = 32L, memory = 8L, partition = "geo")
    # )
    # ,
    # combine tuning results to find the best model
    # xgb and mlp were branched out by learn_rate;
    # learn_rate comes first, thus subsamples are organized
    # in a manner of 1 2 3 4 5 ... 30 1 2 3 4 5 ... 30 ...
    # combine the results with indices of 1 31 61 91 / 2 32 62 92 / ... / 30 60 90 120. Could be changed per #subsamples.
    # elnet is length of 30 (or #subsamples)
    # targets::tar_target(
    #   list_learner_base_lgb_best_spt,
    #   restore_fit_best(
    #     workflow_learner_base_lgb_spt,
    #     rset_full = list_learner_base_cv_spt,
    #     df_full = dt_feat_calc_imputed,
    #     nested = TRUE
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_learner_base_lgb_best_spblock,
    #   restore_fit_best(
    #     workflow_learner_base_lgb_spblock,
    #     rset_full = list_learner_base_cv_spblock,
    #     df_full = dt_feat_calc_imputed,
    #     nested = TRUE
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_learner_base_lgb_best_spcluster,
    #   restore_fit_best(
    #     workflow_learner_base_lgb_spcluster,
    #     rset_full = list_learner_base_cv_spcluster,
    #     df_full = dt_feat_calc_imputed,
    #     nested = TRUE
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_learner_base_mlp_best_spt,
    #   restore_fit_best(
    #     workflow_learner_base_mlp_spt,
    #     rset_full = list_learner_base_cv_spt,
    #     df_full = dt_feat_calc_imputed,
    #     nested = TRUE
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_learner_base_mlp_best_spblock,
    #   restore_fit_best(
    #     workflow_learner_base_mlp_spblock,
    #     rset_full = list_learner_base_cv_spblock,
    #     df_full = dt_feat_calc_imputed,
    #     nested = TRUE
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_learner_base_mlp_best_spcluster,
    #   restore_fit_best(
    #     workflow_learner_base_mlp_spcluster,
    #     rset_full = list_learner_base_cv_spcluster,
    #     df_full = dt_feat_calc_imputed,
    #     nested = TRUE
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_learner_base_elnet_best_spt,
    #   restore_fit_best(
    #     workflow_learner_base_elnet_spt,
    #     rset_full = list_learner_base_cv_spt,
    #     df_full = dt_feat_calc_imputed,
    #     nested = TRUE
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_learner_base_elnet_best_spblock,
    #   restore_fit_best(
    #     workflow_learner_base_elnet_spblock,
    #     rset_full = list_learner_base_cv_spblock,
    #     df_full = dt_feat_calc_imputed,
    #     nested = TRUE
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_learner_base_elnet_best_spcluster,
    #   restore_fit_best(
    #     workflow_learner_base_elnet_spcluster,
    #     rset_full = list_learner_base_cv_spcluster,
    #     df_full = dt_feat_calc_imputed,
    #     nested = TRUE
    #   )
    # )
  )