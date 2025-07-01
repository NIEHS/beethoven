################################################################################
##### Fit CPU-enabled {lightGBM} meta learner from output of base learners.
target_metalearner <-
  list(
    targets::tar_target(
      chr_learner_meta_cols,
      command = c("site_id", "time", "Event.Type", "lon", "lat"),
      description = "ID + spatiotemporal columns | meta"
    ),
    targets::tar_target(
      dt_state_dummy,
      command = {
        dt_meta_cols <- list_base_params_static$dt_full[, chr_learner_meta_cols]
        dt_meta_cols$DUM_STFIPS <- substr(dt_meta_cols$site_id, 1, 2)
        mat_stfips <- model.matrix(~ DUM_STFIPS - 1, data = dt_meta_cols)
        stopifnot(all(rowSums(mat_stfips) == 1))
        data.frame(cbind(dt_meta_cols, mat_stfips))
      },
      description = "Generate State FIPS dummy variable | meta"
    ),
    targets::tar_target(
      dt_base_pred,
      command = {
        dt_pred <- beethoven::reduce_merge(
          list(dt_learner_pred_elnet, dt_learner_pred_mlp, dt_learner_pred_lgb),
          by = c(".row", "Arithmetic.Mean")
        ) |>
          dplyr::arrange(.row)
        dt_base_pred <- data.frame(
          cbind(
            list_base_params_static$dt_full[,
              c(
                chr_learner_meta_cols,
                list_base_params_static$yvar,
                grep(
                  "DUM_E2",
                  names(list_base_params_static$dt_full),
                  value = TRUE
                )
              )
            ],
            dt_pred
          )
        )
        stopifnot(
          all(dt_base_pred$Arithmetic.Mean == dt_base_pred$Arithmetic.Mean.1)
        )
        dt_base_pred2 <- merge(
          dt_base_pred,
          dt_state_dummy,
          by = chr_learner_meta_cols
        )
        dt_base_pred2[,
          grep("Arithmetic.Mean.1|.row", names(dt_base_pred2), invert = TRUE)
        ]
      },
      description = "Base learner predictions with space/time ID | meta"
    )
    # targets::tar_target(
    #   dt_base_performance,
    #   command = {
    #     chr_models <- grep("elnet|mlp|lgb", names(dt_base_pred), value = TRUE)
    #     purrr::map_dfr(
    #       chr_models,
    #       function(pred_col) {
    #         df <- dt_base_pred %>%
    #           dplyr::select(
    #             truth = Arithmetic.Mean,
    #             estimate = dplyr::all_of(pred_col)
    #           )
    #         data.frame(
    #           model = pred_col,
    #           rsq = yardstick::rsq(
    #             df,
    #             truth = truth,
    #             estimate = estimate
    #           )$.estimate,
    #           rmse = yardstick::rmse(
    #             df,
    #             truth = truth,
    #             estimate = estimate
    #           )$.estimate,
    #           mae = yardstick::mae(
    #             df,
    #             truth = truth,
    #             estimate = estimate
    #           )$.estimate
    #         )
    #       }
    #     )
    #   },
    #   description = "Base prediction performance | base"
    # ),
    # targets::tar_target(
    #   rset_meta,
    #   command = rsample::vfold_cv(dt_base_pred, v = 5)$splits[[1]],
    #   description = "Split predictions into 80% train 20% test | meta"
    # ),
    # targets::tar_target(
    #   dt_meta_test,
    #   command = rsample::assessment(rset_meta),
    #   description = "Hold out testing data | meta",
    # ),
    # targets::tar_target(
    #   list_meta_rset_train,
    #   command = {
    #     dt_meta_train <- rsample::training(rset_meta)
    #     spatiotemporal_index <- beethoven::generate_cv_index_spt(
    #       data = dt_meta_train,
    #       crs = list_base_params_static$crs,
    #       cellsize = list_base_params_static$cellsize,
    #       locs_id = "site_id",
    #       coords = c("lon", "lat"),
    #       v = list_base_params_static$cvsize,
    #       time_id = "time"
    #     )
    #     cv_rset <- beethoven::convert_cv_index_rset(
    #       cvindex = spatiotemporal_index,
    #       data = dt_meta_train,
    #       cv_mode = "spatiotemporal"
    #     )
    #     return(cv_rset)
    #   },
    #   description = "MC vfold training sets | meta",
    #   pattern = map(num_cv_index),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_50")
    #   )
    # ),
    # targets::tar_target(
    #   fit_learner_meta,
    #   command = beethoven::fit_base_learner(
    #     rset = list_meta_rset_train,
    #     model = engine_base_elnet,
    #     tune_grid_size = list_base_params_static$tune_grid_size,
    #     yvar = list_base_params_static$yvar,
    #     xvar = grep(
    #       "elnet|mlp|lgb|lon|lat",
    #       names(dt_base_pred),
    #       value = TRUE
    #     ),
    #     drop_vars = list_base_params_static$drop_vars,
    #     normalize = list_base_params_static$normalize
    #   ),
    #   pattern = map(list_meta_rset_train),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_10")
    #   ),
    #   description = "Fit meta learner | elnet | cpu | meta"
    # ),
    # targets::tar_target(
    #   list_meta_predict,
    #   command = beethoven::attach_pred(
    #     pred = beethoven::fit_prediction(
    #       fit = fit_learner_meta,
    #       test = data.frame(list_meta_dt_test),
    #       target_cols = chr_learner_meta_cols,
    #       name = paste0("meta_", sprintf("%05d", num_cv_index))
    #     ),
    #     test = data.frame(list_meta_dt_test),
    #     target_cols = chr_learner_meta_cols,
    #     yvar = list_base_params_static$yvar
    #   ),
    #   pattern = map(fit_learner_meta, list_meta_dt_test, num_cv_index),
    #   iteration = "list",
    #   description = "Predict on test data | elnet | meta",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_50")
    #   )
    # ),
    # targets::tar_target(
    #   dt_meta_mean,
    #   command = {
    #     dt_meta_model <- dplyr::bind_rows(
    #       lapply(
    #         list_meta_predict,
    #         function(x) {
    #           x$model <- grep("meta_", names(x), value = TRUE)
    #           names(x) <- gsub("meta_000[0-9]{2}", "prediction", names(x))
    #           x$st_id <- paste0(x$site_id, "_", x$time)
    #           x
    #         }
    #       )
    #     )
    #     dt_meta_model |>
    #       dplyr::group_by(st_id) |>
    #       dplyr::summarise(
    #         site_id = dplyr::first(site_id),
    #         time = dplyr::first(time),
    #         lon = dplyr::first(lon),
    #         lat = dplyr::first(lat),
    #         prediction = mean(prediction),
    #         Arithmetic.Mean = mean(Arithmetic.Mean),
    #         .groups = "drop"
    #       ) |>
    #       dplyr::select(-st_id) |>
    #       data.table::data.table()
    #   },
    #   description = "Average prediction from meta learners | meta"
    # ),
    # targets::tar_target(
    #   dt_meta_performance,
    #   command = dplyr::bind_rows(
    #     lapply(
    #       c(list_meta_predict, list(dt_meta_mean)),
    #       function(x) {
    #         int_x <- grep("meta_|prediction", names(x))
    #         int_y <- grep("Arithmetic.Mean", names(x))
    #         df_rsq <- yardstick::rsq(
    #           data = x,
    #           truth = int_y,
    #           estimate = int_x
    #         )
    #         df_rmse <- yardstick::rmse(
    #           data = x,
    #           truth = int_y,
    #           estimate = int_x
    #         )
    #         df_msd <- yardstick::msd(
    #           data = x,
    #           truth = int_y,
    #           estimate = int_x
    #         )
    #         data.frame(
    #           model = names(x)[int_x],
    #           rbind(df_rsq, df_rmse, df_msd)
    #         )
    #       }
    #     )
    #   ),
    #   description = "Meta prediction performance | meta"
    # )
  )
