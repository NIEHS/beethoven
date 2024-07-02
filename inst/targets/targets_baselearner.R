
target_baselearner <-
  list(
    targets::tar_target(
      name = dt_feat_calc_xyt,
      attach_xy(dt_feat_calc_imputed, sf_feat_proc_aqs_sites)
    )
    ,
    targets::tar_target(
      name = cv_config,
      command = generate_cv_index(
        data = dt_feat_calc_xyt,
        cv_fold = 7L,
        cv_pairs = 10L,
        preprocessing = "normalize",
        pairing = "1"
      )
    )
    ,
    # wf: workflow
    # feat? base?
    targets::tar_target(
      workflow_learner_base_xgb,
      fit_base_xgb(
        dt_feat_calc_imputed,
        folds = cv_config
      ),
      resources = set_slurm_resource(ncpus = 8L, memory = 16L, partition = "geo,gpu")
    )
    ,
    targets::tar_target(
      workflow_learner_base_mlp,
      fit_base_brulee(
        dt_feat_calc_imputed,
        folds = cv_config
      ),
      resources = set_slurm_resource(ncpus = 8L, memory = 16L, partition = "geo,gpu")
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