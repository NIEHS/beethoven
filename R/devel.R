fit_base_spark <-
  function(
    rset = NULL,
    full_data = NULL,
    model = NULL,
    tune_grid_size = 10L,
    yvar = "Arithmetic.Mean",
    xvar = NULL,
    drop_vars = NULL,
    normalize = TRUE,
    metric = "rmse",
    sc = NULL,
    ...
  ) {
    stopifnot("spark connection must be initialized" = !is.null(sc))
    stopifnot("parsnip model must be defined." = !is.null(model))

    # detect model name
    model_name <- model$engine

    # supported metrics
    chr_metrics <- c("rmse", "rsq", "msd", "mae")
    stopifnot(metric %in% chr_metrics)

    # order metrics (first metric is used for tune_race_anova)
    chr_metrics_ordered <- c(metric, setdiff(chr_metrics, metric))

    # namespaced symbols
    sym_metrics <- lapply(chr_metrics_ordered, function(m) {
      # Create a call to :: (e.g., quote(yardstick::rmse))
      call("::", quote(yardstick), as.symbol(m))
    })

    # full call
    call_metric <- as.call(c(quote(yardstick::metric_set), sym_metrics))

    # first split training for covariate names in recipe
    training_data <- rsample::training(rset$splits[[1]])

    # define recipe
    base_recipe <-
      recipes::recipe(training_data[1, ]) |>
      recipes::update_role(!!xvar, new_role = "predictor") |>
      recipes::update_role(!!yvar, new_role = "outcome") |>
      recipes::step_zv(recipes::all_predictors())

    if (!is.null(drop_vars)) {
      base_recipe <- base_recipe |>
        recipes::update_role(dplyr::all_of(drop_vars), new_role = "id") |>
        recipes::update_role_requirements("id", bake = FALSE)
    }

    if (normalize) {
      base_recipe <-
        base_recipe |>
        recipes::step_normalize(
          recipes::all_numeric_predictors(),
          -recipes::all_integer_predictors()
        )
    }

    # tune hyperparameters in spark
    spark_results <- sparklyr::tune_grid_spark(
      sc = sc,
      object = model,
      preprocessor = base_recipe,
      resamples = rset,
      control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
    )

    return(spark_results)

    # collect predictions
    dt_preds <- tune::collect_predictions(spark_results)

    # Select best model
    best_params <- tune::select_best(spark_results, metric = metric)

    # Finalize workflow with best parameters
    final_wf <- tune::finalize_workflow(spark_results, best_params) |>
      # Fit the finalized workflow to the full data
      parsnip::fit(data = full_data)

    base_results <- list(
      "workflow" = final_wf,
      "predictions" = dt_preds,
      "metrics" = tune::collect_metrics(spark_results)
    )

    return(base_results)
  }
