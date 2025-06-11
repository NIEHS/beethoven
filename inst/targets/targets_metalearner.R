################################################################################
##### Fit CPU-enabled {glmnet} meta learner from output of base learners.
target_metalearner <-
  list(
    targets::tar_target(
      chr_learner_meta_cols,
      command = c("site_id", "time", "Event.Type", "lon", "lat"),
      description = "ID + spatiotemporal columns | meta"
    ),
    targets::tar_target(
      list_base_predict_elnet,
      command = beethoven::attach_pred(
        pred = beethoven::fit_prediction(
          fit = fit_learner_base_elnet,
          test = list_dt_test,
          target_cols = chr_learner_meta_cols,
          name = paste0("elnet_", sprintf("%05d", num_cv_index))
        ),
        test = list_dt_test,
        target_cols = chr_learner_meta_cols,
        yvar = list_base_params_static$yvar
      ),
      pattern = map(fit_learner_base_elnet, list_dt_test, num_cv_index),
      iteration = "list",
      description = "Predict on test data | elnet | meta",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    ),
    targets::tar_target(
      list_base_predict_mlp,
      command = beethoven::attach_pred(
        pred = beethoven::fit_prediction(
          fit = fit_learner_base_mlp,
          test = list_dt_test,
          target_cols = chr_learner_meta_cols,
          name = paste0("mlp_", sprintf("%05d", num_cv_index))
        ),
        test = list_dt_test,
        target_cols = chr_learner_meta_cols,
        yvar = list_base_params_static$yvar
      ),
      pattern = map(fit_learner_base_mlp, list_dt_test, num_cv_index),
      iteration = "list",
      description = "Predict on test data | mlp | meta",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    ),
    targets::tar_target(
      list_base_predict_lgb,
      command = beethoven::attach_pred(
        pred = beethoven::fit_prediction(
          fit = fit_learner_base_lgb,
          test = list_dt_test,
          target_cols = chr_learner_meta_cols,
          name = paste0("lgb_", sprintf("%05d", num_cv_index))
        ),
        test = list_dt_test,
        target_cols = chr_learner_meta_cols,
        yvar = list_base_params_static$yvar
      ),
      pattern = map(fit_learner_base_lgb, list_dt_test, num_cv_index),
      iteration = "list",
      description = "Predict on test data | lgb | meta",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    ),
    targets::tar_target(
      dt_base_performance,
      command = dplyr::bind_rows(
        lapply(
          c(
            list_base_predict_elnet,
            list_base_predict_mlp,
            list_base_predict_lgb
          ),
          function(x) {
            int_x <- grep("(elnet|mlp|lgb)_", names(x))
            int_y <- grep("Arithmetic.Mean", names(x))
            df_rsq <- yardstick::rsq(
              data = x,
              truth = int_y,
              estimate = int_x
            )
            df_rmse <- yardstick::rmse(
              data = x,
              truth = int_y,
              estimate = int_x
            )
            df_mae <- yardstick::msd(
              data = x,
              truth = int_y,
              estimate = int_x
            )
            data.frame(
              model = names(x)[int_x],
              rbind(df_rsq, df_rmse, df_mae)
            )
          }
        )
      ),
      description = "Base prediction performance | base"
    ),
    targets::tar_target(
      list_base_predict,
      command = merge(
        list_base_predict_elnet,
        merge(
          list_base_predict_mlp,
          list_base_predict_lgb,
          by = c(chr_learner_meta_cols, list_base_params_static$yvar)
        ),
        by = c(chr_learner_meta_cols, list_base_params_static$yvar)
      ),
      pattern = map(
        list_base_predict_elnet,
        list_base_predict_mlp,
        list_base_predict_lgb
      ),
      iteration = "list",
      description = "Prediction values with Arithmetic.Mean | meta",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    ),
    targets::tar_target(
      list_meta_cv_rsplit,
      command = {
        rset_meta <- rsample::vfold_cv(list_base_predict, v = 2)
        rset_meta$splits[[1]]
      },
      description = "MC vfold rsets | meta",
      pattern = map(list_base_predict),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    ),
    targets::tar_target(
      list_meta_dt_test,
      command = rsample::assessment(list_meta_cv_rsplit),
      description = "MC vfold testing sets | meta",
      pattern = map(list_meta_cv_rsplit),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    ),
    targets::tar_target(
      list_meta_rset_train,
      command = {
        dt_meta_train <- rsample::training(list_meta_cv_rsplit)
        spatiotemporal_index_meta <- beethoven::generate_cv_index_spt(
          data = dt_meta_train,
          crs = list_base_params_static$crs,
          cellsize = list_base_params_static$cellsize,
          locs_id = "site_id",
          coords = c("lon", "lat"),
          v = list_base_params_static$cvsize,
          time_id = "time"
        )
        beethoven::convert_cv_index_rset(
          cvindex = spatiotemporal_index_meta,
          data = dt_meta_train,
          cv_mode = "spatiotemporal"
        )
      },
      description = "MC vfold training sets | meta",
      pattern = map(list_meta_cv_rsplit),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    ),
    targets::tar_target(
      fit_learner_meta,
      command = beethoven::fit_base_learner(
        rset = list_meta_rset_train,
        model = engine_base_elnet,
        tune_grid_size = list_base_params_static$tune_grid_size,
        yvar = list_base_params_static$yvar,
        xvar = grep(
          "elnet|mlp|lgb|lon|lat",
          names(list_meta_dt_test),
          value = TRUE
        ),
        drop_vars = list_base_params_static$drop_vars,
        normalize = list_base_params_static$normalize
      ),
      pattern = map(list_meta_rset_train, list_meta_dt_test),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Fit meta learner | elnet | cpu | meta"
    ),
    targets::tar_target(
      list_meta_predict,
      command = beethoven::attach_pred(
        pred = beethoven::fit_prediction(
          fit = fit_learner_meta,
          test = data.frame(list_meta_dt_test),
          target_cols = chr_learner_meta_cols,
          name = paste0("meta_", sprintf("%05d", num_cv_index))
        ),
        test = data.frame(list_meta_dt_test),
        target_cols = chr_learner_meta_cols,
        yvar = list_base_params_static$yvar
      ),
      pattern = map(fit_learner_meta, list_meta_dt_test, num_cv_index),
      iteration = "list",
      description = "Predict on test data | elnet | meta",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    ),
    targets::tar_target(
      dt_meta_mean,
      command = {
        dt_meta_model <- dplyr::bind_rows(
          lapply(
            list_meta_predict,
            function(x) {
              x$model <- grep("meta_", names(x), value = TRUE)
              names(x) <- gsub("meta_000[0-9]{2}", "prediction", names(x))
              x$st_id <- paste0(x$site_id, "_", x$time)
              x
            }
          )
        )
        dt_meta_model |>
          dplyr::group_by(st_id) |>
          dplyr::summarise(
            site_id = dplyr::first(site_id),
            time = dplyr::first(time),
            lon = dplyr::first(lon),
            lat = dplyr::first(lat),
            prediction = mean(prediction),
            Arithmetic.Mean = mean(Arithmetic.Mean),
            .groups = "drop"
          ) |>
          dplyr::select(-st_id) |>
          data.table::data.table()
      },
      description = "Average prediction from meta learners | meta"
    ),
    targets::tar_target(
      dt_meta_performance,
      command = dplyr::bind_rows(
        lapply(
          c(list_meta_predict, list(dt_meta_mean)),
          function(x) {
            int_x <- grep("meta_|prediction", names(x))
            int_y <- grep("Arithmetic.Mean", names(x))
            df_rsq <- yardstick::rsq(
              data = x,
              truth = int_y,
              estimate = int_x
            )
            df_rmse <- yardstick::rmse(
              data = x,
              truth = int_y,
              estimate = int_x
            )
            df_msd <- yardstick::msd(
              data = x,
              truth = int_y,
              estimate = int_x
            )
            data.frame(
              model = names(x)[int_x],
              rbind(df_rsq, df_rmse, df_msd)
            )
          }
        )
      ),
      description = "Meta prediction performance | meta"
    )
  )
