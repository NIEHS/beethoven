target_metalearner <-
  list(
    # meta learner
    targets::tar_target(
      meta_fit,
      meta_learner(
        list(base_fit_rf, base_fit_xgboost, base_fit_cnn),
        kfolds = 10L,
        y = data_full[[mr("name_dep")]]
      )
    )
    ,
    # tar_target for initial model update if pre-fitted
    targets::tar_target(
      meta_exported,
      export_res(meta_fit)
    )
  )