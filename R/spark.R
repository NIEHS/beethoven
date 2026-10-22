################################################################################
env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(default)
  }
  if (value %in% c("1", "true", "yes", "y")) {
    return(TRUE)
  }
  if (value %in% c("0", "false", "no", "n")) {
    return(FALSE)
  }
  stop(name, " must be true or false.")
}

################################################################################
sql_identifier <- function(x) {
  paste0("`", gsub("`", "``", x, fixed = TRUE), "`")
}

################################################################################
sql_string <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

################################################################################
env_integer <- function(name, default, minimum = 1L) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    return(as.integer(default))
  }

  value <- suppressWarnings(as.integer(value))
  if (is.na(value) || value < minimum) {
    stop(name, " must be an integer greater than or equal to ", minimum)
  }
  value
}

################################################################################
environment <- function(workspace = Sys.getenv("HOME"), database = "sedonadb") {
  # Detected from Sys.getenv()
  list_environ <- list(
    chr_workdir = workspace,
    int_maxcores = env_integer("SPARK_CORES_MAX", 64L),
    int_executorcores = env_integer("SPARK_EXECUTOR_CORES", 2L),
    int_executormem = Sys.getenv("SPARK_EXECUTOR_MEMORY", unset = "2g"),
    int_partitions = env_integer("SPARK_SHUFFLE_PARTITIONS", 32L),
    int_nround = env_integer("XGB_NUM_ROUND", 50L),
    int_seed = env_integer("XGB_SEED", 20260904L, minimum = 0L),
    int_maxrows = env_integer("XGB_MAX_ROWS", 0L, minimum = 0L),
    int_maxfeatures = env_integer("XGB_MAX_FEATURES", 0L, minimum = 0L),
    chr_database = Sys.getenv("XGB_DATABASE", unset = database),
    chr_xgboost_package = Sys.getenv(
      "XGBOOST_MAVEN_PACKAGE",
      unset = "ml.dmlc:xgboost4j-spark_2.13:3.4.0"
    )
  )

  # Derived from other environmental variables
  list_environ$chr_resultspath <- Sys.getenv(
    "XGB_RESULTS_PATH",
    unset = file.path(list_environ$chr_workdir, "xgboost-results.rds")
  )
  list_environ$int_nworkers <- env_integer(
    "XGB_NUM_WORKERS",
    max(1L, list_environ$int_maxcores %/% list_environ$int_executorcores)
  )
  list_environ$int_nthreads <- env_integer(
    "XGB_NTHREAD",
    list_environ$int_executorcores
  )
  list_environ$chr_warehouse <- Sys.getenv(
    "XGB_WAREHOUSE_DIR",
    unset = file.path(list_environ$chr_workdir, "spark-warehouse")
  )
  chr_databasedir <- file.path(
    list_environ$chr_warehouse,
    paste0(list_environ$chr_database, ".db")
  )

  # Threads and cores check.
  if (list_environ$int_executorcores > list_environ$int_maxcores) {
    stop("SPARK_EXECUTOR_CORES cannot exceed SPARK_CORES_MAX.")
  }

  if (list_environ$int_nthreads > list_environ$int_executorcore) {
    stop("XGB_NTHREAD cannot exceed SPARK_EXECUTOR_CORES.")
  }

  if (
    list_environ$int_nworkers * list_environ$int_nthreads >
      list_environ$int_maxcores
  ) {
    stop("XGB_NUM_WORKERS * XGB_NTHREAD cannot exceed SPARK_CORES_MAX.")
  }

  # Populate parent environment with variables
  list2env(list_environ, envir = parent.frame())

  # Invisible return
  invisible(list_environ)
}

################################################################################
fit_xgb_spark <-
  function(
    master,
    home,
    workspace,
    database = "duckdb",
    path = NULL, # path to full AQS data stored as parquet file
    rset,
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
      sparklyr::invoke("setSeed", int_seed) |>
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
      length(rset),
      " XGBoost model(s) with ",
      int_nworkers,
      " workers ",
      int_nthreads,
      " threads and ",
      int_nround,
      " boosting rounds..."
    )

    # --------------------------------------------------------------------------
    for (r in seq_along(rset)) {
      message(sprintf("Fitting model %03d/%03d...", r, length(rset)))
      folds <- rset[[r]]

      # ------------------------------------------------------------------------
      # Initiate lists for storing fold predictions and performance metrics.
      list_base_xgb_pred[[r]] <- list()
      list_base_xgb_eval[[r]] <- list()

      for (f in seq_along(folds)) {
        # ----------------------------------------------------------------------
        # Manual split into train and test sets based on spatial cv indices.
        chr_training <- folds[[f]]$analysis
        chr_test <- folds[[f]]$assessment
        list_feat_merge_splits <- list(
          training = tbl_feat_merge_assemble |>
            dplyr::filter(site_id %in% !!chr_training),
          test = tbl_feat_merge_assemble |>
            dplyr::filter(site_id %in% !!chr_test)
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

        # ----------------------------------------------------------------------
        # Fit XGBoost model on Spark cluster.
        time_base_xgb_model <- system.time({
          sprk_base_xgb_model <- sparklyr::invoke(
            sprk_estimator,
            "fit",
            sparklyr::spark_dataframe(tbl_feat_merge_train)
          )
        })

        # ----------------------------------------------------------------------
        # Predict model on withheld test data.
        sprk_base_xgb_test <- sparklyr::invoke(
          sprk_base_xgb_model,
          "transform",
          sparklyr::spark_dataframe(tbl_feat_merge_test)
        )
        sprk_base_xgb_pred <- sparklyr::sdf_register(
          sprk_base_xgb_test,
          name = sprintf("beethoven_xgb_preds_%03d_%02d", r, f)
        )

        # ----------------------------------------------------------------------
        # Evaluate model performance on test data.
        dt_base_xgb_eval <- evaluate(
          sc,
          pred = sprk_base_xgb_pred,
          r = sprintf("%03d", r),
          f = sprintf("%02d", f),
        )
        unpersist(list(tbl_feat_merge_train, tbl_feat_merge_test))

        # ----------------------------------------------------------------------
        # Filter to model predictions and original identification values.
        list_base_xgb_pred[[r]][[f]] <- sprk_base_xgb_pred |>
          dplyr::select(site_id, time, lon, lat, label, prediction) |>
          dplyr::rename(
            !!yvar := label,
            !!sprintf("%s_%03d", stage, r) := prediction
          ) |>
          dplyr::collect()

        # ----------------------------------------------------------------------
        # Retain model predictios and evaluation statistics.
        list_base_xgb_eval[[r]][[f]] <- data.frame(
          dt_base_xgb_eval,
          time = unname(time_base_xgb_model[["elapsed"]])
        )
      }
    }
    message("All models fit; proceeding with collection.")

    # --------------------------------------------------------------------------
    # Stack out of fold predictions from each independent rset object.
    list_base_xgb_oof <- lapply(list_base_xgb_pred, dplyr::bind_rows)

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
      list_base_xgb_oof
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
  r,
  f,
  pred,
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
    rset = r,
    fold = f,
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
