################################################################################
#' Read a logical flag from an environment variable
#'
#' Recognizes `"1"`, `"true"`, `"yes"`, and `"y"` as true, and `"0"`,
#' `"false"`, `"no"`, and `"n"` as false, ignoring case. Other nonempty
#' values raise an error.
#'
#' @param name character(1). Name of the environment variable.
#' @param default logical(1). Value returned when the variable is unset or
#' empty.
#' @return A single logical value.
#' @keywords internal
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
#' Quote Spark SQL identifiers
#'
#' Wraps identifiers in backticks and escapes embedded backticks by doubling
#' them.
#'
#' @param x Character vector of table or column names.
#' @return A character vector of quoted identifiers.
#' @keywords internal
sql_identifier <- function(x) {
  paste0("`", gsub("`", "``", x, fixed = TRUE), "`")
}

################################################################################
#' Quote SQL character literals
#'
#' Wraps strings in single quotes and doubles embedded single quotes. This
#' helper does not escape backslashes; their interpretation depends on the
#' SQL parser configuration.
#'
#' @param x Character vector of values to quote.
#' @return A character vector of quoted string literals.
#' @keywords internal
sql_string <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

################################################################################
#' Read an integer from an environment variable
#'
#' Converts a nonempty environment variable with [base::as.integer()] and
#' checks the lower bound. Values that cannot be converted or are below
#' `minimum` raise an error.
#'
#' @param name character(1). Name of the environment variable.
#' @param default numeric(1). Value returned, after integer conversion, when
#'   the variable is unset or empty. The default is not checked against
#'   `minimum`.
#' @param minimum integer(1). Smallest permitted environment-variable value.
#' @return A single integer.
#' @keywords internal
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
#' Read Spark and XGBoost settings from environment variables
#'
#' Collects configuration for [fit_xgb_spark()] and checks that worker and
#' thread counts fit within the available cores. Settings are returned in a
#' list for explicit use by the caller.
#'
#' @param workspace character(1). Base directory for default warehouse and
#'   results paths. Defaults to the `HOME` environment variable.
#' @param database character(1). Default database name, overridden by
#'   `XGB_DATABASE`. Currently retained as configuration metadata only.
#' @details
#' The following environment variables control configuration:
#' * `SPARK_CORES_MAX`: maximum cores; defaults to 64.
#' * `SPARK_EXECUTOR_CORES`: cores per executor; defaults to 2.
#' * `SPARK_EXECUTOR_MEMORY`: memory per executor; defaults to `"2g"`.
#' * `SPARK_SHUFFLE_PARTITIONS`: shuffle partitions; defaults to 32.
#' * `XGB_NUM_ROUND`: boosting rounds; defaults to 50.
#' * `XGB_SEED`: nonnegative random seed; defaults to 20260904.
#' * `XGB_MAX_ROWS`: nonnegative input row limit; 0 means no limit.
#' * `XGB_NUM_WORKERS`: workers; defaults to maximum cores divided by cores
#'   per executor, rounded down, with a minimum of 1.
#' * `XGB_NTHREAD`: threads per worker; defaults to cores per executor.
#' * `XGB_WAREHOUSE_DIR`: warehouse directory; defaults to
#'   `file.path(workspace, "spark-warehouse")`.
#' * `XGBOOST_MAVEN_PACKAGE`: Maven coordinate; defaults to
#'   `"ml.dmlc:xgboost4j-spark_2.13:3.4.0"`.
#' * `XGB_DATABASE`: database name; defaults to `database`.
#' * `XGB_MAX_FEATURES`: nonnegative feature limit; defaults to 0. This
#'   setting is retained but is not currently applied by [fit_xgb_spark()].
#' * `XGB_RESULTS_PATH`: results path; defaults to
#'   `file.path(workspace, "xgboost-results.rds")`. This setting is retained
#'   but [fit_xgb_spark()] returns metrics without writing an RDS file.
#'
#' Integer settings have a minimum of 1 unless described as nonnegative.
#' An error is raised if executor cores exceed maximum cores, threads per
#' worker exceed executor cores, or workers times threads exceed maximum cores.
#'
#' @return Invisibly, a named list with elements `chr_workdir`, `int_maxcores`,
#'   `int_executorcores`, `int_executormem` (a character memory specification),
#'   `int_partitions`, `int_nround`, `int_seed`, `int_maxrows`,
#'   `int_maxfeatures`, `chr_database`, `chr_xgboost_package`,
#'   `chr_resultspath`, `int_nworkers`, `int_nthreads`, and `chr_warehouse`.
#' @seealso [env_integer()]
#' @keywords internal
spark_environment <- function(
  workspace = Sys.getenv("HOME"),
  database = "sedonadb"
) {
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

  # Threads and cores check.
  if (list_environ$int_executorcores > list_environ$int_maxcores) {
    stop("SPARK_EXECUTOR_CORES cannot exceed SPARK_CORES_MAX.")
  }

  if (list_environ$int_nthreads > list_environ$int_executorcores) {
    stop("XGB_NTHREAD cannot exceed SPARK_EXECUTOR_CORES.")
  }

  if (
    list_environ$int_nworkers * list_environ$int_nthreads >
      list_environ$int_maxcores
  ) {
    stop("XGB_NUM_WORKERS * XGB_NTHREAD cannot exceed SPARK_CORES_MAX.")
  }

  # Return settings for explicit access in the calling function.
  invisible(list_environ)
}

################################################################################
#' Fit and evaluate XGBoost regression models on Spark
#'
#' Reads Parquet data into Spark, assembles numeric predictors, and fits one
#' XGBoost regression model per resampling fold. Held-out predictions are
#' written as Parquet datasets and evaluation metrics are collected in R.
#'
#' @param master character(1). Spark master URL or local master specification,
#'   passed to [sparklyr::spark_connect()].
#' @param home character(1). Spark installation directory, passed as
#'   `spark_home` to [sparklyr::spark_connect()].
#' @param workspace character(1). Base directory for default Spark paths.
#' @param database character(1). Default database name passed to
#'   [spark_environment()]. Currently retained as configuration metadata only.
#' @param input Character path or paths to input Parquet data, passed to
#'   [sparklyr::spark_read_parquet()]. A usable path must be supplied despite
#'   the `NULL` default. Data must contain `site_id`, `time`, `lon`, `lat`, the
#'   response, and numeric predictor columns.
#' @param output character(1). Directory for prediction datasets. It must be
#'   accessible to Spark and the local R process for returned file discovery.
#'   Existing datasets with the same resample and fold names cause an error.
#' @param rset Nested list of resamples and folds. Each `rset[[r]][[f]]` must
#'   contain `analysis` and `assessment` vectors of `site_id` values used for
#'   training and testing, respectively. These are site identifiers, not row
#'   positions or `rsample` split objects.
#' @param xvar Character vector of predictor names. Must be identical, including
#'   order, to the numeric columns in the Spark schema after excluding `yvar`
#'   and `dvar`.
#' @param yvar character(1). Response column name, aliased to `label` in Spark.
#' @param dvar Character vector of columns excluded from predictor discovery,
#'   typically identifiers, coordinates, and time columns.
#' @param stage character(1). Prefix for prediction columns, which are named
#'   using `sprintf("%s_%03d", stage, r)` for resample `r`.
#' @param ... Reserved for future use. Currently ignored.
#' @details
#' Requires the sparklyr and DBI packages, a working Spark installation, and
#' compatible XGBoost4J-Spark JARs. A nonempty `XGBOOST_JARS` environment
#' variable supplies JAR paths through `sparklyr.shell.jars`; otherwise the
#' Maven coordinate from [spark_environment()] is used. See that helper for
#' resource limits, boosting rounds, seed, warehouse path, and row-limit
#' settings. A positive row limit uses SQL `LIMIT` without an ordering clause.
#'
#' Models use CPU histogram training with squared-error loss, maximum depth 8,
#' learning rate 0.1, and row and column sampling fractions of 0.8. Feature
#' vectors are made dense so zero values remain explicit. No final model is
#' retained; the Spark connection is disconnected on exit, including on error.
#'
#' Predictions are written beneath `output` as
#' `rset_NNN_fold_NN.parquet` datasets, with identification columns `site_id`,
#' `time`, `lon`, and `lat`, the original response name, and the prediction
#' column. Each dataset can contain multiple Parquet part files.
#'
#' @return A list with:
#' * `pred`: character vector of newly discovered `.parquet` file paths beneath
#'   `output`.
#' * `eval`: data frame with `rset` and `fold` identifiers, `metric` and `value`
#'   columns for RMSE, MSE, R-squared, and MAE, and `time`, the elapsed fitting
#'   time in seconds repeated for each metric within a fold.
#' @seealso [spark_environment()], [sparklyr::spark_connect()]
#' @importFrom rlang .data
#' @export
fit_xgb_spark <-
  function(
    master,
    home,
    workspace,
    database = "duckdb",
    input = NULL,
    output,
    rset,
    xvar,
    yvar,
    dvar,
    stage = "base",
    ...
  ) {
    # --------------------------------------------------------------------------
    # Spark configuration.
    config <- spark_environment(workspace = workspace, database = database)
    config_base <- sparklyr::spark_config()
    chr_xgboost_jars <- Sys.getenv("XGBOOST_JARS", unset = "")
    if (nzchar(chr_xgboost_jars)) {
      config_base[["sparklyr.shell.jars"]] <- chr_xgboost_jars
    } else {
      config_base[["sparklyr.shell.packages"]] <- config$chr_xgboost_package
    }
    config_base[["spark.cores.max"]] <- as.character(config$int_maxcores)
    config_base[["spark.executor.cores"]] <- as.character(
      config$int_executorcores
    )
    config_base[["spark.executor.memory"]] <- config$int_executormem
    config_base[["spark.sql.shuffle.partitions"]] <- as.character(
      config$int_partitions
    )
    config_base[["spark.sql.warehouse.dir"]] <- paste0(
      "file:",
      config$chr_warehouse
    )
    config_base[["spark.sql.parquet.compression.codec"]] <- "zstd"
    config_base[["spark.task.cpus"]] <- as.character(config$int_nthreads)

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
    chr_feat_merge_sptmpl <- "db_feat_merge_sptmpl"
    sparklyr::spark_read_parquet(
      sc,
      name = chr_feat_merge_sptmpl,
      path = input,
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
    # from the table read from the input Parquet data.
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
      if (config$int_maxrows > 0L) paste("LIMIT", config$int_maxrows) else ""
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
      dplyr::select(tidyselect::all_of(
        c("site_id", "time", "lon", "lat", "label", "features")
      ))

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
      sparklyr::invoke("setNumWorkers", config$int_nworkers) |>
      sparklyr::invoke("setNthread", config$int_nthreads) |>
      sparklyr::invoke("setNumRound", config$int_nround) |>
      sparklyr::invoke("setSeed", config$int_seed) |>
      sparklyr::invoke("setMaxDepth", 8L) |>
      sparklyr::invoke("setEta", 0.1) |>
      sparklyr::invoke("setSubsample", 0.8) |>
      sparklyr::invoke("setColsampleBytree", 0.8) |>
      sparklyr::invoke("setForceRepartition", TRUE)

    # --------------------------------------------------------------------------
    # Initiate lists for storing all predictions and performance metrics.
    list_base_xgb_eval <- list()

    # --------------------------------------------------------------------------
    # Report the model-fitting configuration.
    message(
      "Fitting ",
      length(rset),
      " XGBoost model(s) with ",
      config$int_nworkers,
      " workers, ",
      config$int_nthreads,
      " threads and ",
      config$int_nround,
      " boosting rounds..."
    )

    # --------------------------------------------------------------------------
    # Create output directory if does not exist.
    dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

    # --------------------------------------------------------------------------
    # Extract file names in output before parquet file writing.
    chr_output1 <- list.files(
      output,
      recursive = TRUE,
      full.names = TRUE,
      pattern = "\\.parquet$"
    )

    # --------------------------------------------------------------------------
    for (r in seq_along(rset)) {
      message(sprintf("Fitting model %03d/%03d...", r, length(rset)))
      folds <- rset[[r]]

      # ------------------------------------------------------------------------
      # Initiate lists for storing fold predictions and performance metrics.
      list_base_xgb_eval[[r]] <- list()

      for (f in seq_along(folds)) {
        message(sprintf("Fitting fold %02d/%02d...", f, length(folds)))
        # ----------------------------------------------------------------------
        # Manual split into train and test sets based on spatial cv indices.
        chr_training <- folds[[f]]$analysis
        chr_test <- folds[[f]]$assessment
        list_feat_merge_splits <- list(
          training = tbl_feat_merge_assemble |>
            dplyr::filter(.data$site_id %in% !!chr_training),
          test = tbl_feat_merge_assemble |>
            dplyr::filter(.data$site_id %in% !!chr_test)
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
          f = sprintf("%02d", f)
        )

        # ----------------------------------------------------------------------
        # Retain model evaluation statistics.
        list_base_xgb_eval[[r]][[f]] <- data.frame(
          dt_base_xgb_eval,
          time = unname(time_base_xgb_model[["elapsed"]])
        )

        # ----------------------------------------------------------------------
        # Filter to model predictions and original identification values.
        tbl_base_xgb_pred <- sprk_base_xgb_pred |>
          dplyr::select(tidyselect::all_of(
            c("site_id", "time", "lon", "lat", "label", "prediction")
          )) |>
          dplyr::rename(
            !!yvar := tidyselect::all_of("label"),
            !!sprintf("%s_%03d", stage, r) := tidyselect::all_of("prediction")
          )

        # ----------------------------------------------------------------------
        # Generate prediction value output paths.
        chr_base_xgb_path <- file.path(
          output,
          sprintf("rset_%03d_fold_%02d.parquet", r, f)
        )

        # ----------------------------------------------------------------------
        # Save model predictions as .parquet files.
        sparklyr::spark_write_parquet(
          tbl_base_xgb_pred,
          path = chr_base_xgb_path,
          mode = "error"
        )

        # ----------------------------------------------------------------------
        # Drop training and testing data from Spark.
        unpersist(list(tbl_feat_merge_train, tbl_feat_merge_test))
      }
    }
    message("All models fit; proceeding with collection.")

    # --------------------------------------------------------------------------
    # Extract file names in output after parquet file writing, and limit to only
    # the new .parquet files.
    chr_output2 <- list.files(
      output,
      recursive = TRUE,
      full.names = TRUE,
      pattern = "\\.parquet$"
    )

    # --------------------------------------------------------------------------
    # Return only the newly written .parquet file path(s) and evaluation.
    return(list(
      pred = setdiff(chr_output2, chr_output1),
      eval = data.frame(dplyr::bind_rows(list_base_xgb_eval))
    ))
  }

################################################################################
#' Evaluate Spark regression predictions
#'
#' Uses Spark's regression evaluator to collect metrics from `label` and
#' `prediction` columns without collecting individual predictions in R.
#'
#' @param sc A Spark connection created by [sparklyr::spark_connect()].
#' @param r Resample identifier, typically a zero-padded character string.
#' @param f Fold identifier, typically a zero-padded character string.
#' @param pred A Spark table containing numeric `label` and `prediction`
#'   columns.
#' @param metric Character vector of metrics to compute. Supported values are
#'   `"rmse"` (root mean squared error), `"mse"` (mean squared error),
#'   `"r2"` (coefficient of determination), `"mae"` (mean absolute error),
#'   and `"var"` (explained variance). Defaults to the first four.
#' @return A data frame with one row per requested metric and columns `rset`,
#'   `fold`, `metric`, and `value`, in the requested metric order.
#' @keywords internal
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
#' Release cached Spark tables
#'
#' Calls Spark's `unpersist` method with blocking enabled for each table,
#' waiting for cached blocks to be removed before continuing.
#'
#' @param tables List of Spark tables accepted by [sparklyr::spark_dataframe()].
#' @return Invisibly, `NULL`. Called for its cache-release side effect.
#' @keywords internal
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
