
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
        rep(0.3, 30),
        function(x) {
          nxyt <- nrow(dt_feat_calc_xyt)
          dt_feat_calc_xyt[sample(seq_len(nxyt), ceiling(nxyt * x)), ]
        }
      ),
      iteration = "list"
    )
    ,
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
      iteration = "list"
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
    # feat? base?
    targets::tar_target(
      workflow_learner_base_xgb,
      fit_base_xgb(
        dt_feat_calc_imputed,
        folds = cv_config,
        tune_mode = "grid",
        learn_rate = num_learner_base_learn_device$rate,
        device = num_learner_base_learn_device$device
      ),
      pattern = map(num_learner_base_learn_device),
      resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo")
    )
    ,
    targets::tar_target(
      workflow_learner_base_mlp,
      fit_base_brulee(
        dt_feat_calc_imputed,
        folds = cv_config,
        tune_mode = "grid",
        learn_rate = num_learner_base_learn_device$rate,
        device = num_learner_base_learn_device$device
      ),
      pattern = map(num_learner_base_learn_device),
      resources = set_slurm_resource(ncpus = 6L, memory = 20L, partition = "geo,gpu")
    )
    ,
    targets::tar_target(
      workflow_learner_base_elnet,
      fit_base_elnet(
        dt_feat_calc_imputed,
        folds = cv_config,
        nthreads = 32L
      ),
      resources = set_slurm_resource(ncpus = 32L, memory = 8L, partition = "geo,highmem")
    )
  )