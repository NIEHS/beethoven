
target_baselearner <-
  list(
		targets::tar_target(
			cv_config,
			configure_cv(
				covars = covariates_final
			)
		)
		,
		targets::tar_target(
			base_prep_rf,
			base_learner_prep(
				learner = "randomforest",
				data = data_full,
				dependent_name = mr("name_dep"),
				independent_name = file.path(mr("dir_output"), mr("file_name_indep"))
			)
		)
		,
		targets::tar_target(
			base_prep_xgboost,
			base_learner_prep(
				learner = "xgboost",
				data = data_full,
				dependent_name = mr("name_dep"),
				independent_name = file.path(mr("dir_output"), mr("file_name_indep"))
			)
		)
		,
		targets::tar_target(
			base_prep_cnn,
			base_learner_prep(
				learner = "cnn",
				data = data_full,
				dependent_name = mr("name_dep"),
				independent_name = file.path(mr("dir_output"), mr("file_name_indep"))
			)
		)
		,
		targets::tar_target(
			base_fit_rf,
			base_learner_cv_fit(
				"randomforest",
				ymat = base_prep_rf$ymat,
				xmat = base_prep_rf$xmat,
				cv_index = cv_config$lblto,
				fun = base_learner_fit_ranger
			)
		)
		,
		targets::tar_target(
			base_fit_xgboost,
			base_learner_cv_fit(
				"xgboost",
				ymat = base_prep_xgboost$ymat,
				xmat = base_prep_xgboost$xmat,
				cv_index = cv_config$lblto,
				fun = base_learner_fit_xgboost
			)
		)
		,
		targets::tar_target(
			base_fit_cnn,
			base_learner_cv_fit(
				"cnn",
				ymat = base_prep_cnn$ymat,
				xmat = base_prep_cnn$xmat,
				cv_index = cv_config$lblto,
				fun = base_learner_fit_cnn
			)
		)
  )