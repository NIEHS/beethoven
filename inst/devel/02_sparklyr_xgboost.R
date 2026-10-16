library(sparklyr)
library(dplyr)
library(DBI)

# XGBoost4J-Spark 3.x supports Spark 4.0.1+ and is published for Scala 2.13.
# The sparkxgb R package is not used because its current JVM extension supports
# Scala 2.12 only, whereas this cluster runs Spark 4.2 with Scala 2.13.
xgboost_maven_package <- Sys.getenv(
  "XGBOOST_MAVEN_PACKAGE",
  unset = "ml.dmlc:xgboost4j-spark_2.13:3.4.0"
)

chr_home <- Sys.getenv("HOME")

main <- function() {
  workspace_dir <- paste0(chr_home, "/beethoven/sedonatest")
  results_path <- Sys.getenv(
    "XGB_RESULTS_PATH",
    unset = file.path(workspace_dir, "xgboost-results.rds")
  )

  # Keep the same resource settings as the current 64-core Spark test.
  spark_cores_max <- env_integer("SPARK_CORES_MAX", 64L)
  executor_cores <- env_integer("SPARK_EXECUTOR_CORES", 2L)
  executor_memory <- Sys.getenv("SPARK_EXECUTOR_MEMORY", unset = "2g")
  shuffle_partitions <- env_integer("SPARK_SHUFFLE_PARTITIONS", 32L)

  # One two-thread XGBoost worker per two-core executor uses all 64 cores.
  num_workers <- env_integer(
    "XGB_NUM_WORKERS",
    max(1L, spark_cores_max %/% executor_cores)
  )
  nthread <- env_integer("XGB_NTHREAD", executor_cores)
  num_round <- env_integer("XGB_NUM_ROUND", 50L)
  seed <- env_integer("XGB_SEED", 20260904L, minimum = 0L)

  if (executor_cores > spark_cores_max) {
    stop("SPARK_EXECUTOR_CORES cannot exceed SPARK_CORES_MAX.")
  }
  if (nthread > executor_cores) {
    stop("XGB_NTHREAD cannot exceed SPARK_EXECUTOR_CORES.")
  }
  if (num_workers * nthread > spark_cores_max) {
    stop("XGB_NUM_WORKERS * XGB_NTHREAD cannot exceed SPARK_CORES_MAX.")
  }

  # These two optional limits make a quick integration test possible without
  # changing the full-data defaults. Zero means use all rows/features.
  max_rows <- env_integer("XGB_MAX_ROWS", 0L, minimum = 0L)
  max_features <- env_integer("XGB_MAX_FEATURES", 0L, minimum = 0L)

  limited_run <- max_rows > 0L || max_features > 0L
  database_name <- Sys.getenv("XGB_DATABASE", unset = "sedonadb")
  table_name <- Sys.getenv(
    "XGB_TABLE",
    unset = if (limited_run) {
      "dt_feat_pm_imputed_smoke"
    } else {
      "dt_feat_pm_imputed"
    }
  )
  rebuild_table <- env_flag("XGB_REBUILD_TABLE", default = limited_run)

  valid_sql_name <- "^[A-Za-z_][A-Za-z0-9_]*$"
  if (!grepl(valid_sql_name, database_name)) {
    stop("XGB_DATABASE is not a valid unqualified SQL identifier.")
  }
  if (!grepl(valid_sql_name, table_name)) {
    stop("XGB_TABLE is not a valid unqualified SQL identifier.")
  }

  database_sql <- sql_identifier(database_name)
  table_sql <- paste(database_sql, sql_identifier(table_name), sep = ".")
  warehouse_dir <- Sys.getenv(
    "XGB_WAREHOUSE_DIR",
    unset = file.path(workspace_dir, "spark-warehouse")
  )
  database_dir <- file.path(warehouse_dir, paste0(database_name, ".db"))
  dir.create(warehouse_dir, recursive = TRUE, showWarnings = FALSE)

  table_data_root <- Sys.getenv(
    "SEDONADB_TABLE_DIR",
    unset = file.path(workspace_dir, "sedonadb-tables", database_name)
  )
  table_data_path <- file.path(table_data_root, paste0(table_name, ".parquet"))

  if (rebuild_table || !file.exists(table_data_path)) {
    creator_script <- file.path(workspace_dir, "02_create_sedonadb_table.R")

    # SedonaDB links to a newer C++ runtime than the system R executable. Run
    # table creation in a clean child R process with that runtime preloaded.
    libstdcxx <- Sys.getenv(
      "SEDONADB_LIBSTDCXX",
      unset = paste0(
        chr_home
        "/.conda/envs/sdamadeus/lib/libstdc++.so.6"
      )
    )
    if (!file.exists(creator_script)) {
      stop("SedonaDB table creator not found: ", creator_script)
    }
    if (!file.exists(libstdcxx)) {
      stop("SedonaDB C++ runtime not found: ", libstdcxx)
    }

    current_preload <- Sys.getenv("LD_PRELOAD", unset = "")
    preload <- paste(
      c(libstdcxx, current_preload[nzchar(current_preload)]),
      collapse = ":"
    )
    creator_env <- c(
      paste0("LD_PRELOAD=", preload),
      paste0("SEDONADB_PARQUET_PATH=", table_data_path),
      paste0("SEDONADB_OVERWRITE=", tolower(as.character(rebuild_table))),
      paste0("XGB_MAX_ROWS=", max_rows),
      paste0("XGB_SEED=", seed)
    )

    cat("Building the persistent dataset with SedonaDB...\n")
    status <- system2(
      file.path(R.home("bin"), "Rscript"),
      args = creator_script,
      env = creator_env
    )
    if (!identical(status, 0L)) {
      stop("SedonaDB table creation failed with exit status ", status)
    }
  } else {
    cat("Using existing SedonaDB Parquet data at", table_data_path, "\n")
  }

  config <- spark_config()
  config[["spark.cores.max"]] <- as.character(spark_cores_max)
  config[["spark.executor.cores"]] <- as.character(executor_cores)
  config[["spark.executor.memory"]] <- executor_memory
  config[["spark.sql.shuffle.partitions"]] <- as.character(shuffle_partitions)
  config[["spark.sql.warehouse.dir"]] <- paste0("file:", warehouse_dir)
  config[["spark.sql.parquet.compression.codec"]] <- "zstd"

  # Reserve both executor cores for each two-thread XGBoost barrier task. This
  # prevents two native XGBoost workers from oversubscribing one executor.
  config[["spark.task.cpus"]] <- as.character(nthread)

  # By default Spark resolves the official XGBoost artifact and distributes it
  # to executors. For an offline run, XGBOOST_JARS may instead contain a
  # comma-separated list of the XGBoost JAR and all of its runtime dependencies.
  xgboost_jars <- Sys.getenv("XGBOOST_JARS", unset = "")
  if (nzchar(xgboost_jars)) {
    config[["sparklyr.shell.jars"]] <- xgboost_jars
  } else {
    config[["sparklyr.shell.packages"]] <- xgboost_maven_package
  }

  sc <- spark_connect(
    master = readLines("input/.hidden/chr_spark_master.txt"),
    spark_home = readLines("input/.hidden/chr_spark_home.txt"),
    app_name = "beethoven-xgboost-test",
    config = config
  )
  on.exit(spark_disconnect(sc), add = TRUE)

  cat("Spark version:", as.character(spark_version(sc)), "\n")
  DBI::dbExecute(
    sc,
    paste(
      "CREATE DATABASE IF NOT EXISTS",
      database_sql,
      "LOCATION",
      sql_string(paste0("file:", database_dir))
    )
  )

  if (rebuild_table) {
    cat("Dropping the existing table because XGB_REBUILD_TABLE is true...\n")
    DBI::dbExecute(sc, paste("DROP TABLE IF EXISTS", table_sql))
  }

  tables <- DBI::dbGetQuery(
    sc,
    paste(
      "SHOW TABLES IN",
      database_sql,
      "LIKE",
      sql_string(table_name)
    )
  )
  table_exists <- nrow(tables) > 0L

  if (!table_exists) {
    cat(
      "Registering the SedonaDB data as external Spark table",
      table_sql,
      "...\n"
    )
    DBI::dbExecute(
      sc,
      paste(
        "CREATE TABLE",
        table_sql,
        "USING PARQUET LOCATION",
        sql_string(paste0("file:", table_data_path))
      )
    )
  } else {
    cat("Using existing persistent table", table_sql, "\n")
  }

  table_schema <- DBI::dbGetQuery(
    sc,
    paste("DESCRIBE TABLE", table_sql)
  )
  table_schema <- table_schema[
    nzchar(trimws(table_schema$col_name)) &
      !startsWith(trimws(table_schema$col_name), "#"),
    ,
    drop = FALSE
  ]

  response <- "Arithmetic.Mean"
  id_columns <- c("site_id", "time", "Event.Type")
  numeric_type <- grepl(
    "^(tinyint|smallint|int|bigint|float|double|decimal)",
    tolower(table_schema$data_type)
  )
  feature_columns <- setdiff(
    table_schema$col_name[numeric_type],
    c(response, id_columns)
  )

  if (!response %in% table_schema$col_name) {
    stop("Response column '", response, "' is not present in ", table_sql)
  }
  if (!length(feature_columns)) {
    stop("No numeric feature columns were found in ", table_sql)
  }
  if (max_features > 0L && max_features < length(feature_columns)) {
    feature_columns <- feature_columns[seq_len(max_features)]
  }

  # VectorAssembler treats periods in an input column name as separators for
  # nested struct fields. Give every predictor a simple Spark-only alias so
  # names such as NARR_0_AIR.SFC_00000 remain ordinary scalar columns.
  feature_aliases <- sprintf(
    "feature_%04d",
    seq_along(feature_columns)
  )
  feature_projection <- paste(
    vapply(feature_columns, sql_identifier, character(1)),
    "AS",
    vapply(feature_aliases, sql_identifier, character(1))
  )

  model_sql <- paste(
    "SELECT",
    paste(
      c(
        paste(sql_identifier(response), "AS `label`"),
        feature_projection
      ),
      collapse = ", "
    ),
    "FROM",
    table_sql,
    if (max_rows > 0L) paste("LIMIT", max_rows) else ""
  )
  cat("Selecting", length(feature_columns), "features with Spark SQL...\n")
  dt_spark <- sdf_sql(sc, model_sql)

  cat("Assembling the Spark feature vector...\n")
  assembled <- dt_spark %>%
    ft_vector_assembler(
      input_cols = feature_aliases,
      output_col = "features"
    ) %>%
    select(label, features)

  # VectorAssembler chooses SparseVector when a row contains many zeros. In
  # XGBoost, absent sparse entries can instead mean missing values. Round-trip
  # through an array so Spark returns a DenseVector and every zero is retained.
  assembled_jobj <- spark_dataframe(assembled)
  sparse_features <- invoke(assembled_jobj, "col", "features")
  feature_array <- invoke_static(
    sc,
    "org.apache.spark.ml.functions",
    "vector_to_array",
    sparse_features,
    "float64"
  )
  dense_features <- invoke_static(
    sc,
    "org.apache.spark.ml.functions",
    "array_to_vector",
    feature_array
  )
  assembled <- invoke(
    assembled_jobj,
    "withColumn",
    "features",
    dense_features
  ) %>%
    sdf_register()

  splits <- sdf_random_split(
    assembled,
    training = 0.8,
    test = 0.2,
    seed = seed
  )
  training <- sdf_persist(
    splits$training,
    storage.level = "MEMORY_AND_DISK",
    name = "beethoven_xgb_training"
  )
  test <- sdf_persist(
    splits$test,
    storage.level = "MEMORY_AND_DISK",
    name = "beethoven_xgb_test"
  )

  split_counts <- bind_rows(
    training %>% summarise(split = "training", rows = n()) %>% collect(),
    test %>% summarise(split = "test", rows = n()) %>% collect()
  )
  print(split_counts)

  cat(
    "Fitting XGBoost with",
    num_workers,
    "workers x",
    nthread,
    "threads and",
    num_round,
    "boosting rounds...\n"
  )

  estimator <- invoke_new(
    sc,
    "ml.dmlc.xgboost4j.scala.spark.XGBoostRegressor"
  )

  estimator <- estimator %>%
    invoke("setLabelCol", "label") %>%
    invoke("setFeaturesCol", "features") %>%
    invoke("setPredictionCol", "prediction") %>%
    invoke("setObjective", "reg:squarederror") %>%
    invoke("setEvalMetric", "rmse") %>%
    invoke("setTreeMethod", "hist") %>%
    invoke("setDevice", "cpu") %>%
    invoke("setNumWorkers", num_workers) %>%
    invoke("setNthread", nthread) %>%
    invoke("setNumRound", num_round) %>%
    invoke("setMaxDepth", 8L) %>%
    invoke("setEta", 0.1) %>%
    invoke("setSubsample", 0.8) %>%
    invoke("setColsampleBytree", 0.8) %>%
    invoke("setSeed", seed) %>%
    invoke("setForceRepartition", TRUE)

  fit_time <- system.time({
    xgb_model <- invoke(estimator, "fit", spark_dataframe(training))
  })
  cat("XGBoost fit elapsed seconds:", unname(fit_time[["elapsed"]]), "\n")

  prediction_jobj <- invoke(
    xgb_model,
    "transform",
    spark_dataframe(test)
  )
  predictions <- sdf_register(
    prediction_jobj,
    name = "beethoven_xgb_predictions"
  )

  evaluator <- ml_regression_evaluator(
    sc,
    label_col = "label",
    prediction_col = "prediction",
    metric_name = "rmse"
  )
  rmse <- ml_evaluate(evaluator, predictions)

  cat("Test RMSE:", rmse, "\n")
  prediction_output <- predictions %>%
    select(label, prediction) %>%
    collect()
  saveRDS(
    list(rmse = rmse, predictions = prediction_output),
    results_path
  )
  cat("Saved XGBoost results to", results_path, "\n")
  print(head(prediction_output, 10))

  invisible(list(model = xgb_model, rmse = rmse, predictions = predictions))
}

main()
