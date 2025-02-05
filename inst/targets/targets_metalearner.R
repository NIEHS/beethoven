################################################################################
##### Fit CPU-enabled {glmnet} meta learner from output of base learners.
target_metalearner <-
  list(
    targets::tar_target(
      name = chr_learner_meta_cols,
      command = c("site_id", "time", "Event.Type", "lon", "lat"),
      description = "ID + spatiotemporal columns | meta"
    )
    ,
    targets::tar_target(
      name = dt_feat_base_xyt,
      command = beethoven::attach_pred(
        data = data.frame(dt_feat_calc_xyt_subset),
        pred = list_learner_base_best,
        target_cols = chr_learner_meta_cols,
        yvar = list_base_params_static$yvar
      ),
      description = "Base learner predictions + AQS sites | meta"
    )
    ,
    targets::tar_target(
      name = fit_learner_meta_cpu,
      command = fit_meta_learner_dev(
        data = dt_feat_base_xyt,
        yvar = list_base_params_static$yvar,
        target_cols = chr_learner_meta_cols,
        r_subssample = 1.0,
        c_subsample = 0.5,
        args_generate_cv = list_base_args_cv$spatiotemporal,
        tune_iter = 3L,
        nthreads = 2L,
        trim_resamples = TRUE,
        return_best = TRUE,
        metric = "rmse"
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "Fit meta learner | cpu | meta"
    )
  )
