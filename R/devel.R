################################################################################
fit_base_spark <-
  function(
    master,
    home,
    workspace,
    database = "duckdb",
    path = NULL, # path to full AQS data stored as parquet path
    iter = 1L,
    xvar,
    yvar,
    dvar,
    stage = "base",
    ...
  ) {
    # --------------------------------------------------------------------------
    # Spark configuration.
    environment(workspace = workspace, database = database)
    config_base <- sparklyr::spark_config()
    chr_xgboost_jars <- Sys.getenv("XGBOOST_JARS", unset = "")
    if (nzchar(chr_xgboost_jars)) {
      config_base[["sparklyr.shell.jars"]] <- chr_xgboost_jars
    } else {
      config_base[["sparklyr.shell.packages"]] <- chr_xgboost_package
    }
    config_base[["spark.cores.max"]] <- as.character(int_maxcores)
    config_base[["spark.executor.cores"]] <- as.character(int_executorcores)
    config_base[["spark.executor.memory"]] <- int_executormem
    config_base[["spark.sql.shuffle.partitions"]] <- as.character(
      int_partitions
    )
    config_base[["spark.sql.warehouse.dir"]] <- paste0("file:", chr_warehouse)
    config_base[["spark.sql.parquet.compression.codec"]] <- "zstd"

    # --------------------------------------------------------------------------
    # Connect to local Spark cluster.
    message("Connecting to Spark...")
    sc <- sparklyr::spark_connect(
      master = master,
      spark_home = home,
      config = config_base,
      app_name = "beethoven-devel"
    )
    on.exit(
      {
        sparklyr::spark_disconnect(sc)
        message("Disconnected from Spark.")
      },
      add = TRUE
    )
    message("Spark connected; proceeding with model fitting.")

    # --------------------------------------------------------------------------
    # Load .parquet file into Spark memory.
    chr_feat_merge_sptmpl <- gsub("output/|\\.parquet", "", path)
    db_feat_merge_sptmpl <- sparklyr::spark_read_parquet(
      sc,
      name = chr_feat_merge_sptmpl,
      path = path,
      memory = FALSE
    )

    # --------------------------------------------------------------------------
    # Read data schema.
    dt_feat_merge_schema <- DBI::dbGetQuery(
      sc,
      paste0("DESCRIBE TABLE ", chr_feat_merge_sptmpl)
    )
    dt_feat_merge_schema <- dt_feat_merge_schema[
      nzchar(trimws(dt_feat_merge_schema$col_name)) &
        !startsWith(trimws(dt_feat_merge_schema$col_name), "#"),
      ,
      drop = FALSE
    ]

    # --------------------------------------------------------------------------
    # Extract and define numeric features (predictors).
    bin_feat_merge_num <- grepl(
      "^(tinyint|smallint|int|bigint|float|double|decimal)",
      tolower(dt_feat_merge_schema$data_type)
    )
    chr_feat_merge_xvar <- setdiff(
      dt_feat_merge_schema$col_name[bin_feat_merge_num],
      c(yvar, dvar)
    )

    # --------------------------------------------------------------------------
    # Check derived features identical to manually set `$xvar`.
    stopifnot(identical(chr_feat_merge_xvar, xvar))

    # --------------------------------------------------------------------------
    # Replace predictor variable names with generic "feature_" labels.
    chr_feat_merge_xalias <- sprintf(
      "feature_%04d",
      seq_along(chr_feat_merge_xvar)
    )

    # --------------------------------------------------------------------------
    # Define remapping of variable names to "feature_" labels.
    chr_feat_merge_proj <- paste(
      vapply(chr_feat_merge_xvar, sql_identifier, character(1)),
      "AS",
      vapply(chr_feat_merge_xalias, sql_identifier, character(1))
    )

    # --------------------------------------------------------------------------
    # Define SQL query for response and predictor variables with alias renaming
    # from the `dt_feat_merge_sptmpl` table, read from .parquet in Line 76.
    chr_feat_model_sql <- paste(
      "SELECT",
      paste(
        c(
          vapply(
            c("site_id", "time", "lon", "lat"),
            sql_identifier,
            character(1)
          ),
          paste(sql_identifier(yvar), "AS `label`"),
          chr_feat_merge_proj
        ),
        collapse = ", "
      ),
      "FROM",
      chr_feat_merge_sptmpl,
      if (int_maxrows > 0L) paste("LIMIT", int_maxrows) else ""
    )

    # --------------------------------------------------------------------------
    # Query `dt_feat_merge_sptmpl` table for response and predictor variables.
    db_feat_merge_alias <- sparklyr::sdf_sql(sc, chr_feat_model_sql)

    # --------------------------------------------------------------------------
    # Assemble the Spark feature vector.
    tbl_feat_merge_ftvec <- db_feat_merge_alias |>
      sparklyr::ft_vector_assembler(
        input_cols = chr_feat_merge_xalias,
        output_col = "features"
      ) |>
      dplyr::select(site_id, time, lon, lat, label, features)

    # --------------------------------------------------------------------------
    # VectorAssembler chooses SparseVector when a row contains many zeros. In
    # XGBoost, absent sparse entries can instead mean missing values. Round-trip
    # through an array so Spark returns a DenseVector and zeros are retained.
    sprk_feat_merge_ftvec <- sparklyr::spark_dataframe(tbl_feat_merge_ftvec)
    sprk_feat_merge_features <- sparklyr::invoke(
      sprk_feat_merge_ftvec,
      "col",
      "features"
    )
    sprk_feat_merge_array <- sparklyr::invoke_static(
      sc,
      "org.apache.spark.ml.functions",
      "vector_to_array",
      sprk_feat_merge_features,
      "float64"
    )
    sprk_feat_merge_dense <- sparklyr::invoke_static(
      sc,
      "org.apache.spark.ml.functions",
      "array_to_vector",
      sprk_feat_merge_array
    )
    tbl_feat_merge_assemble <- sparklyr::invoke(
      sprk_feat_merge_ftvec,
      "withColumn",
      "features",
      sprk_feat_merge_dense
    ) |>
      sparklyr::sdf_register()
    tbl_feat_merge_assemble <- sparklyr::sdf_persist(
      tbl_feat_merge_assemble,
      storage.level = "MEMORY_AND_DISK",
      name = "beethoven_xgb_features"
    )
    sparklyr::invoke(
      sparklyr::spark_dataframe(tbl_feat_merge_assemble),
      "count"
    )

    # --------------------------------------------------------------------------
    # Set Spark estimator and invoke XGBoost.
    # !!! Manual workers and threads for development. !!!
    int_nworkers <- 1L
    int_nthreads <- 1L
    sprk_estimator <- sparklyr::invoke_new(
      sc,
      "ml.dmlc.xgboost4j.scala.spark.XGBoostRegressor"
    )
    sprk_estimator <- sprk_estimator |>
      sparklyr::invoke("setLabelCol", "label") |>
      sparklyr::invoke("setFeaturesCol", "features") |>
      sparklyr::invoke("setPredictionCol", "prediction") |>
      sparklyr::invoke("setObjective", "reg:squarederror") |>
      sparklyr::invoke("setEvalMetric", "rmse") |>
      sparklyr::invoke("setTreeMethod", "hist") |>
      sparklyr::invoke("setDevice", "cpu") |>
      sparklyr::invoke("setNumWorkers", int_nworkers) |>
      sparklyr::invoke("setNthread", int_nthreads) |>
      sparklyr::invoke("setNumRound", int_nround) |>
      sparklyr::invoke("setMaxDepth", 8L) |>
      sparklyr::invoke("setEta", 0.1) |>
      sparklyr::invoke("setSubsample", 0.8) |>
      sparklyr::invoke("setColsampleBytree", 0.8) |>
      sparklyr::invoke("setForceRepartition", TRUE)

    # --------------------------------------------------------------------------
    # Initiate lists for storing all predictions and performance metrics.
    list_base_xgb_pred <- list()
    list_base_xgb_eval <- list()

    # --------------------------------------------------------------------------
    # Generate list of seeds for random split (temporary).
    message(
      "Fitting ",
      iter,
      " XGBoost model(s) with ",
      int_nworkers,
      " workers ",
      int_nthreads,
      " threads and ",
      int_nround,
      " boosting rounds..."
    )
    int_seeds <- sample(seq(1:1e5), iter)

    # --------------------------------------------------------------------------
    for (i in seq_along(int_seeds)) {
      message(sprintf(
        "Fitting model %03d/%03d; seed = %05d...",
        i,
        length(int_seeds),
        int_seeds[i]
      ))

      # ------------------------------------------------------------------------
      # Set seed estimator seed within the loop.
      sprk_estimator <- sprk_estimator |>
        sparklyr::invoke("setSeed", int_seeds[i])

      # ------------------------------------------------------------------------
      # Split data into train and test sets.
      list_feat_merge_splits <- sparklyr::sdf_random_split(
        tbl_feat_merge_assemble,
        training = 0.8,
        test = 0.2,
        seed = int_seeds[i]
      )
      tbl_feat_merge_train <- sparklyr::sdf_persist(
        list_feat_merge_splits$training,
        storage.level = "MEMORY_AND_DISK",
        name = "beethoven_xgb_training"
      )
      tbl_feat_merge_test <- sparklyr::sdf_persist(
        list_feat_merge_splits$test,
        storage.level = "MEMORY_AND_DISK",
        name = "beethoven_xgb_test"
      )

      # ------------------------------------------------------------------------
      # Fit XGBoost model on Spark cluster.
      time_base_xgb_model <- system.time({
        sprk_base_xgb_model <- sparklyr::invoke(
          sprk_estimator,
          "fit",
          sparklyr::spark_dataframe(tbl_feat_merge_train)
        )
      })

      # ------------------------------------------------------------------------
      # Predict model on withheld test data.
      sprk_base_xgb_test <- sparklyr::invoke(
        sprk_base_xgb_model,
        "transform",
        sparklyr::spark_dataframe(tbl_feat_merge_test)
      )
      sprk_base_xgb_pred <- sparklyr::sdf_register(
        sprk_base_xgb_test,
        name = "beethoven_xgb_predictions"
      )

      # ------------------------------------------------------------------------
      # Evaluate model performance on test data.
      dt_base_xgb_eval <- evaluate(
        sc,
        pred = sprk_base_xgb_pred,
        seed = int_seeds[i]
      )
      unpersist(list(tbl_feat_merge_train, tbl_feat_merge_test))

      # ------------------------------------------------------------------------
      # Fit model to full data for pass to meta learner.
      sprk_base_xgb_full <- sparklyr::invoke(
        sprk_base_xgb_model,
        "transform",
        sparklyr::spark_dataframe(tbl_feat_merge_assemble)
      )

      # ------------------------------------------------------------------------
      # Retain predictions in the database.
      sprk_base_xgb_fullpred <- sparklyr::sdf_register(
        sprk_base_xgb_full,
        name = sprintf(
          "beethoven_xgb_full_predictions_%05d",
          int_seeds[i]
        )
      )

      # ------------------------------------------------------------------------
      # Filter to model predictions and original identification values.
      list_base_xgb_pred[[i]] <- sprk_base_xgb_fullpred |>
        dplyr::select(site_id, time, lon, lat, label, prediction) |>
        dplyr::rename(
          !!yvar := label,
          !!sprintf("%s_%05d", stage, int_seeds[i]) := prediction
        ) |>
        dplyr::collect()

      # ------------------------------------------------------------------------
      # Retain model predictios and evaluation statistics.
      list_base_xgb_eval[[i]] <- data.frame(
        seed = int_seeds[i],
        time = unname(time_base_xgb_model[["elapsed"]]),
        dt_base_xgb_eval
      )
    }
    message("All models fit; proceeding with collection.")

    # --------------------------------------------------------------------------
    # Merge predictions from each model into a single data table for return.
    chr_joinkeys <- c("site_id", "time", "lon", "lat", yvar)
    tbl_base_xgb_preds <- Reduce(
      function(x, y) {
        dplyr::left_join(
          x,
          dplyr::select(
            y,
            dplyr::all_of(chr_joinkeys),
            dplyr::starts_with(paste0(stage, "_"))
          ),
          by = chr_joinkeys
        )
      },
      list_base_xgb_pred
    )

    # --------------------------------------------------------------------------
    # Collect evaluation metrics.
    dt_base_xgb_eval <- dplyr::bind_rows(list_base_xgb_eval)

    # --------------------------------------------------------------------------
    return(list(
      pred = data.table::data.table(tbl_base_xgb_preds),
      eval = data.frame(dt_base_xgb_eval)
    ))
  }

################################################################################
evaluate <- function(
  sc,
  pred,
  seed,
  metric = c("rmse", "mse", "r2", "mae")
) {
  # --------------------------------------------------------------------------
  # Define and check evaluation metric(s).
  methods <- c(
    rmse = "rootMeanSquaredError",
    mse = "meanSquaredError",
    r2 = "r2",
    mae = "meanAbsoluteError",
    var = "explainedVariance"
  )
  stopifnot(all(metric %in% names(methods)))

  # --------------------------------------------------------------------------
  # Evaluate predictions for each of the included metrics.
  evaluator <- sparklyr::invoke_new(
    sc,
    "org.apache.spark.ml.evaluation.RegressionEvaluator"
  ) |>
    sparklyr::invoke("setLabelCol", "label") |>
    sparklyr::invoke("setPredictionCol", "prediction")
  metrics <- sparklyr::invoke(
    evaluator,
    "getMetrics",
    sparklyr::spark_dataframe(pred)
  )

  # --------------------------------------------------------------------------
  # Collect evaluation metrics and drop row names.
  eval <- data.frame(
    metric = metric,
    value = vapply(
      unname(methods[metric]),
      function(method) sparklyr::invoke(metrics, method),
      numeric(1)
    )
  )
  rownames(eval) <- NULL

  # --------------------------------------------------------------------------
  eval
}

################################################################################
unpersist <- function(tables) {
  for (t in tables) {
    sparklyr::invoke(
      sparklyr::spark_dataframe(t),
      "unpersist",
      TRUE
    )
  }
  invisible(NULL)
}
