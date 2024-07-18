
target_baselearner <-
  list(
    targets::tar_target(
      name = dt_feat_calc_xyt,
      attach_xy(dt_feat_calc_imputed, sf_feat_proc_aqs_sites)
    )
    ,
    # P
    targets::tar_target(
      name = list_feat_calc_xyt,
      command =
      lapply(
        rep(1, 30),
        function(x) {
          make_subdata(dt_feat_calc_xyt, p = 0.3)
        }
      ),
      iteration = "list"
    )
    ,
    # length of 30 rsets
    targets::tar_target(
      name = list_learner_base_cv_spt,
      command =
      prepare_cvindex(
        data = list_feat_calc_xyt,
        target_cols = c("lon", "lat", "time"),
        cv_make_fun = generate_cv_index,
        cv_fold = 8L,
        cv_pairs = 10L,
        preprocessing = "normalize",
        pairing = "1"
      ),
      pattern = map(list_feat_calc_xyt),
      iteration = "list"
    )
    ,
    # length of 30
    targets::tar_target(
      name = list_learner_base_cv_spblock,
      command =
      prepare_cvindex(
        data = list_feat_calc_xyt,
        target_cols = c("lon", "lat"),
        cv_make_fun = spatialsample::spatial_block_cv,
        v = 10L,
        method = "snake"
      ),
      pattern = map(list_feat_calc_xyt),
      iteration = "list"
    )
    ,
    # length of 30
    targets::tar_target(
      name = list_learner_base_cv_spcluster,
      command =
      prepare_cvindex(
        data = list_feat_calc_xyt,
        target_cols = c("lon", "lat"),
        cv_make_fun = spatialsample::spatial_clustering_cv,
        v = 10L,
        cluster_function = "kmeans"
      ),
      pattern = map(list_feat_calc_xyt),
      iteration = "list",
      resources = set_slurm_resource(ncpus = 1L, memory = 32L, partition = "geo")
    )
    ,
    # learn_rate branching
    targets::tar_target(
      name = num_learner_base_learn_device,
      command =
        split(
          data.frame(
            device = sprintf("cuda:%d", c(0, 1, 2, 3)),
            rate = c(0.1, 0.05, 0.01, 0.001)
          ), seq(1, 4)
        ),
      description = "device and learning rate",
      iteration = "list"
    )
    ,
    # wf: workflow
    # xgb-spt-cv
    # length of 120 (4 * 30)
    targets::tar_target(
      workflow_learner_base_lgb_spt,
      fit_base_lightgbm(
        dt_feat_calc_imputed,
        folds = list_learner_base_cv_spt,
        tune_mode = "grid",
        learn_rate = num_learner_base_learn_device$rate,
        device = num_learner_base_learn_device$device
      ),
      pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spt),
      resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    )
    ,
    # length of 120
    targets::tar_target(
      workflow_learner_base_lgb_spblock,
      fit_base_lightgbm(
        dt_feat_calc_imputed,
        folds = list_learner_base_cv_spblock,
        tune_mode = "grid",
        learn_rate = num_learner_base_learn_device$rate,
        device = num_learner_base_learn_device$device
      ),
      pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spblock),
      resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    )
    ,
    # length of 120
    targets::tar_target(
      workflow_learner_base_lgb_spcluster,
      fit_base_lightgbm(
        dt_feat_calc_imputed,
        folds = list_learner_base_cv_spcluster,
        tune_mode = "grid",
        learn_rate = num_learner_base_learn_device$rate,
        device = num_learner_base_learn_device$device
      ),
      pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spcluster),
      resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    )
    ,
    # mlp-cv: iterate by combination of rate+device and cv strategy
    # length of 120
    targets::tar_target(
      workflow_learner_base_mlp_spt,
      fit_base_brulee(
        dt_feat_calc_imputed,
        folds = list_learner_base_cv_spt,
        tune_mode = "grid",
        learn_rate = num_learner_base_learn_device$rate,
        device = num_learner_base_learn_device$device
      ),
      pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spt),
      resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo,gpu")
    )
    ,
    # length of 120
    targets::tar_target(
      workflow_learner_base_mlp_spblock,
      fit_base_brulee(
        dt_feat_calc_imputed,
        folds = list_learner_base_cv_spblock,
        tune_mode = "grid",
        learn_rate = num_learner_base_learn_device$rate,
        device = num_learner_base_learn_device$device
      ),
      pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spblock),
      resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo,gpu")
    )
    ,
    # length of 120
    targets::tar_target(
      workflow_learner_base_mlp_spcluster,
      fit_base_brulee(
        dt_feat_calc_imputed,
        folds = list_learner_base_cv_spcluster,
        tune_mode = "grid",
        learn_rate = num_learner_base_learn_device$rate,
        device = num_learner_base_learn_device$device
      ),
      pattern = cross(num_learner_base_learn_device, list_learner_base_cv_spcluster),
      resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo,gpu")
    )
    ,
    # elnet-cv is branched out only by subsamples.
    # length of 30
    targets::tar_target(
      workflow_learner_base_elnet_spt,
      fit_base_elnet(
        dt_feat_calc_imputed,
        folds = list_learner_base_cv_spt,
        nthreads = 32L
      ),
      pattern = map(list_learner_base_cv_spt),
      iteration = "list",
      resources = set_slurm_resource(ncpus = 32L, memory = 8L, partition = "geo")
    )
    ,
    # length of 30
    targets::tar_target(
      workflow_learner_base_elnet_spblock,
      fit_base_elnet(
        dt_feat_calc_imputed,
        folds = list_learner_base_cv_spblock,
        nthreads = 32L
      ),
      pattern = map(list_learner_base_cv_spblock),
      iteration = "list",
      resources = set_slurm_resource(ncpus = 32L, memory = 8L, partition = "geo")
    )
    ,
    # length of 30
    targets::tar_target(
      workflow_learner_base_elnet_spcluster,
      fit_base_elnet(
        dt_feat_calc_imputed,
        folds = list_learner_base_cv_spcluster,
        nthreads = 32L
      ),
      pattern = map(list_learner_base_cv_spcluster),
      iteration = "list",
      resources = set_slurm_resource(ncpus = 32L, memory = 8L, partition = "geo")
    )
    ,
    # combine tuning results to find the best model
    # xgb and mlp were branched out by learn_rate;
    # learn_rate comes first, thus subsamples are organized
    # in a manner of 1 2 3 4 5 ... 30 1 2 3 4 5 ... 30 ...
    # combine the results with indices of 1 31 61 91 / 2 32 62 92 / ... / 30 60 90 120. Could be changed per #subsamples.
    # elnet is length of 30 (or #subsamples)
    targets::tar_target(
      list_learner_base_lgb_best_spt,
      restore_fit_best(
        workflow_learner_base_lgb_spt,
        rset_full = list_learner_base_cv_spt,
        df_full = dt_feat_calc_imputed,
        nested = TRUE
      )
    )
    ,
    targets::tar_target(
      list_learner_base_lgb_best_spblock,
      restore_fit_best(
        workflow_learner_base_lgb_spblock,
        rset_full = list_learner_base_cv_spblock,
        df_full = dt_feat_calc_imputed,
        nested = TRUE
      )
    )
    ,
    targets::tar_target(
      list_learner_base_lgb_best_spcluster,
      restore_fit_best(
        workflow_learner_base_lgb_spcluster,
        rset_full = list_learner_base_cv_spcluster,
        df_full = dt_feat_calc_imputed,
        nested = TRUE
      )
    )
    ,
    targets::tar_target(
      list_learner_base_mlp_best_spt,
      restore_fit_best(
        workflow_learner_base_mlp_spt,
        rset_full = list_learner_base_cv_spt,
        df_full = dt_feat_calc_imputed,
        nested = TRUE
      )
    )
    ,
    targets::tar_target(
      list_learner_base_mlp_best_spblock,
      restore_fit_best(
        workflow_learner_base_mlp_spblock,
        rset_full = list_learner_base_cv_spblock,
        df_full = dt_feat_calc_imputed,
        nested = TRUE
      )
    )
    ,
    targets::tar_target(
      list_learner_base_mlp_best_spcluster,
      restore_fit_best(
        workflow_learner_base_mlp_spcluster,
        rset_full = list_learner_base_cv_spcluster,
        df_full = dt_feat_calc_imputed,
        nested = TRUE
      )
    )
    ,
    targets::tar_target(
      list_learner_base_elnet_best_spt,
      restore_fit_best(
        workflow_learner_base_elnet_spt,
        rset_full = list_learner_base_cv_spt,
        df_full = dt_feat_calc_imputed,
        nested = TRUE
      )
    )
    ,
    targets::tar_target(
      list_learner_base_elnet_best_spblock,
      restore_fit_best(
        workflow_learner_base_elnet_spblock,
        rset_full = list_learner_base_cv_spblock,
        df_full = dt_feat_calc_imputed,
        nested = TRUE
      )
    )
    ,
    targets::tar_target(
      list_learner_base_elnet_best_spcluster,
      restore_fit_best(
        workflow_learner_base_elnet_spcluster,
        rset_full = list_learner_base_cv_spcluster,
        df_full = dt_feat_calc_imputed,
        nested = TRUE
      )
    )
  )