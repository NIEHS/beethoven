################################################################################
##### Fit CPU-enabled {glmnet} meta learner from output of base learners.
target_metalearner <-
  list(
    # targets::tar_target(
    #   name = chr_learner_meta_cols,
    #   command = c("site_id", "time", "Event.Type", "lon", "lat"),
    #   description = "ID + spatiotemporal columns | meta"
    # )
    # ,
    # targets::tar_target(
    #   name = dt_feat_base_xyt,
    #   command = beethoven::attach_pred(
    #     data = data.frame(dt_feat_calc_xyt),
    #     pred = list_learner_base_best,
    #     target_cols = chr_learner_meta_cols,
    #     yvar = list_base_params_static$yvar
    #   ),
    #   description = "Base learner predictions + AQS sites | meta"
    # )
    # ,
    # targets::tar_target(
    #   name = chr_iter_meta_rep,
    #   command = seq_len(list_base_params_static$cv_rep),
    #   description = "Meta learner repetitions | meta"
    # )
    targets::tar_target(
      name = base_linear_pred,
      command = {
        predict(fit_learner_base_elnet$workflow, mc_base_subsampe[[3]])
      },
      pattern = map(fit_learner_base_elnet, mc_base_subsample),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_geo")
      )
    ),
    targets::tar_target(
      name = base_lgb_pred,
      command = {
        predict(fit_learner_base_lgb$workflow, mc_base_subsample[[3]])
      },
      pattern = map(fit_learner_base_lgb, mc_base_subsample),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_geo")
      )
    ),
    targets::tar_target(
      name = base_mlp_pred,
      command = {
        predict(fit_learner_base_mlp$workflow, mc_base_subsample[[3]])
      },
      pattern = map(fit_learner_base_mlp, mc_base_subsample),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_geo")
      )
    )
    # # ,
    # # targets::tar_target(
    # #   name = fit_learner_meta_elnet,
    # #   command = beethoven::fit_meta_learner(
    # #     data = dt_feat_base_xyt,
    # #     yvar = list_base_params_static$yvar,
    # #     target_cols = chr_learner_meta_cols,
    # #     r_subsample = 1.0,
    # #     c_subsample = 0.5,
    # #     args_generate_cv = list_base_args_cv$spatiotemporal,
    # #     tune_iter = 25L,
    # #     trim_resamples = list_base_params_static$trim_resamples,
    # #     return_best = list_base_params_static$return_best,
    # #     metric = "rmse"
    # #   ),
    # #   pattern = map(chr_iter_meta_rep),
    # #   iteration = "list",
    # #   resources = targets::tar_resources(
    # #     crew = targets::tar_resources_crew(controller = "controller_50")
    # #   ),
    # #   description = "Fit meta learner | elnet | cpu | meta"
    # # )
    # # ,
    # # targets::tar_target(
    # #   name = dt_feat_meta_xyt,
    # #   command = beethoven::attach_pred(
    # #     data = data.table::data.table(dt_feat_base_xyt),
    # #     pred = fit_learner_meta_elnet,
    # #     target_cols = chr_learner_meta_cols,
    # #     yvar = list_base_params_static$yvar
    # #   ),
    # #   description = "Meta learner predictions + AQS sites | meta"
    # # )
  )
