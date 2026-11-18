target_predict <-
  list(
		targets::tar_target(
			prediction_rf,
			predict_base(
				fit_object = base_fit_rf,
				data = covariates_predict_final
			)
		)
		,
		targets::tar_target(
			prediction_xgboost,
			predict_base(
				fit_object = base_fit_xgboost,
				data = covariates_predict_final
			)
		)
		,
		targets::tar_target(
			prediction_cnn,
			predict_base(
				fit_object = base_fit_cnn,
				data = covariates_predict_final
			)
		)
		,
		targets::tar_target(
			prediction_meta,
			predict_meta(
				metalearner = meta_fit,
				targetdf = covariates_predict_final,
				threads = mr("nthreads_predict")
			)
		)
	)