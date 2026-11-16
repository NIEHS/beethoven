# nocov start
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
#' Collects configuration for [fit_xgb_spark()] and [predict_xgb_spark()] and
#' checks that worker and thread counts fit within the available cores.
#' Settings are returned in a list for explicit use by the caller.
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
#' * `XGB_MAX_ROWS`: nonnegative training-row limit; 0 means no limit.
#'   [predict_xgb_spark()] overrides this setting to process all input rows.
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
#' XGBoost regression model per fold of each resampling repeat. Each native
#' booster is saved as a `.ubj` file. Out-of-fold predictions are combined into
#' one Parquet dataset, and evaluation metrics and a model index are returned
#' in R.
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
#' @param pred character(1). Directory for the combined out-of-fold prediction
#'   dataset, written with overwrite mode. It must be accessible to Spark and
#'   the local R process for returned file discovery.
#' @param model character(1). Directory for native booster files named
#'   `rset_NNN_fold_NN.ubj`. It is created if needed and must be accessible to
#'   the Spark driver and later prediction jobs. Keep it separate from `pred`
#'   so overwriting prediction data does not remove saved models.
#' @param rset Nested list of resampling repeats and folds. Each
#'   `rset[[r]][[f]]` must contain `analysis` and `assessment` vectors of
#'   `site_id` values used for training and testing, respectively. These are
#'   site identifiers, not row positions or `rsample` split objects.
#' @param xvar Character vector of predictor names. Must be identical, including
#'   order, to the numeric columns in the Spark schema after excluding `yvar`
#'   and `dvar`.
#' @param yvar character(1). Response column name, aliased to `label` in Spark.
#' @param dvar Character vector of columns excluded from predictor discovery,
#'   typically site identifiers and time columns.
#' @param stage character(1). Prefix for prediction columns, which are named
#'   using `sprintf("%s_%03d", stage, r)` for repeat `r`. Use `"base"` or
#'   `"meta"` to distinguish the two learner stages.
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
#' vectors are made dense so zero values remain explicit. Each fold's native
#' booster is saved immediately after fitting, before held-out prediction.
#' The Spark connection is disconnected on exit, including on error.
#'
#' Within each repeat, held-out predictions are row-bound across folds. Repeats
#' are then left-joined by `site_id` and `time`, retaining identifiers and the
#' response from the first repeat. The dataset beneath `pred` contains
#' `site_id`, `time`, `lon`, `lat`, the response under its original name, and
#' one prediction column per repeat. It can contain multiple Parquet part files.
#'
#' The returned model metadata retains the predictor order and maps each
#' repeat and fold to its saved booster path and prediction-column name. Pass
#' this complete `model` list to [predict_xgb_spark()] for subsequent
#' prediction.
#'
#' @return A list with:
#' * `pred`: character vector of `.parquet` paths beneath `pred` that were not
#'   present before writing. Paths with unchanged names are excluded.
#' * `eval`: data frame with `rset` and `fold` identifiers, `metric` and `value`
#'   columns for RMSE, MSE, R-squared, and MAE, and `time`, the elapsed fitting
#'   time in seconds repeated for each metric within a fold.
#' * `model`: list containing `index`, a data frame with integer `rset` and
#'   `fold`, character `prediction`, and absolute native-booster `path` columns;
#'   `xvar`, the ordered predictor names; `yvar`, the response name; and
#'   `stage`, the prediction prefix. The caller can save this list for later
#'    reuse.
#' @seealso [spark_environment()], [assemble_xgb_features()],
#' [predict_xgb_spark()]
#' @importFrom rlang .data
#' @keywords Baselearner
#' @export
fit_xgb_spark <- function(
  master,
  home,
  workspace,
  database = "duckdb",
  input = NULL,
  pred,
  model,
  rset,
  xvar,
  yvar,
  dvar,
  stage = "base",
  ...
) {
  # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
  # Load .parquet file into Spark memory.
  chr_feat_merge_sptmpl <- "db_feat_merge_sptmpl"
  sparklyr::spark_read_parquet(
    sc,
    name = chr_feat_merge_sptmpl,
    path = input,
    options = list(mergeSchema = "true"),
    memory = FALSE
  )
  # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
  # Extract and define numeric features (predictors).
  bin_feat_merge_num <- grepl(
    "^(tinyint|smallint|int|bigint|float|double|decimal)",
    tolower(dt_feat_merge_schema$data_type)
  )
  chr_feat_merge_xvar <- setdiff(
    dt_feat_merge_schema$col_name[bin_feat_merge_num],
    c(yvar, dvar)
  )
  # ----------------------------------------------------------------------------
  # Check derived features identical to manually set `$xvar`.
  stopifnot(identical(chr_feat_merge_xvar, xvar))
  # ----------------------------------------------------------------------------
  # Prepare xvar and yvar features.
  tbl_feat_merge_assemble <- assemble_xgb_features(
    sc = sc,
    chr_feat_merge_sptmpl = chr_feat_merge_sptmpl,
    chr_feat_merge_xvar = chr_feat_merge_xvar,
    yvar = yvar,
    config = config
  )
  # ----------------------------------------------------------------------------
  tbl_feat_merge_persist <- sparklyr::sdf_persist(
    tbl_feat_merge_assemble,
    storage.level = "MEMORY_AND_DISK",
    name = "beethoven_xgb_features"
  )
  sparklyr::invoke(
    sparklyr::spark_dataframe(tbl_feat_merge_persist),
    "count"
  )
  # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
  # Initiate lists for storing all predictions and performance metrics.
  list_base_xgb_eval <- list()
  list_base_xgb_pred <- list()
  # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
  # Create prediction directory if does not exist.
  dir.create(pred, recursive = TRUE, showWarnings = FALSE)
  # ----------------------------------------------------------------------------
  # Extract file names in output before parquet file writing.
  chr_pred1 <- list.files(
    pred,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.parquet$"
  )
  # ----------------------------------------------------------------------------
  # Create model directory if does not exist.
  dir.create(model, recursive = TRUE, showWarnings = FALSE)
  model <- normalizePath(model, mustWork = TRUE)
  list_base_xgb_model <- list()
  # ----------------------------------------------------------------------------
  # Site information for model/fold-specific messages.
  get_sites <- function(x) {
    x |>
      dplyr::filter(!is.na(site_id)) |>
      dplyr::distinct(site_id) |>
      dplyr::collect() |>
      dplyr::pull(site_id)
  }
  chr_allsites <- get_sites(tbl_feat_merge_persist)
  int_allsites <- length(chr_allsites)
  stopifnot(int_allsites > 0L)
  report_sites <- function(label, ids) {
    n <- length(ids)
    message(sprintf(
      "  %-24s %4d / %4d sites (%5.1f%%)",
      label,
      n,
      int_allsites,
      100 * n / int_allsites
    ))
  }
  # ----------------------------------------------------------------------------
  for (r in seq_along(rset)) {
    folds <- rset[[r]]
    # --------------------------------------------------------------------------
    # Initiate lists for storing fold predictions and performance metrics.
    list_base_xgb_eval[[r]] <- list()
    list_base_xgb_fold <- list()
    for (f in seq_along(folds)) {
      # ------------------------------------------------------------------------
      # Manual split into train and test sets based on spatial cv indices.
      chr_training <- folds[[f]]$analysis
      chr_test <- folds[[f]]$assessment
      list_feat_merge_splits <- list(
        training = tbl_feat_merge_persist |>
          dplyr::filter(.data$site_id %in% !!chr_training),
        test = tbl_feat_merge_persist |>
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
      # ------------------------------------------------------------------------
      # Count sites actually present in the Spark splits.
      # Message sites used in each Spark split.
      chr_trainsites <- get_sites(tbl_feat_merge_train)
      chr_testsites <- get_sites(tbl_feat_merge_test)
      chr_included <- union(chr_trainsites, chr_testsites)
      chr_excluded <- setdiff(chr_allsites, chr_included)
      chr_overlap <- intersect(chr_trainsites, chr_testsites)
      stopifnot(length(chr_overlap) == 0L)
      message(sprintf("\nModel (%s) %03d; Fold %02d:", stage, r, f))
      report_sites("Training", chr_trainsites)
      report_sites("Testing", chr_testsites)
      report_sites("Included", chr_included)
      report_sites("Excluded", chr_excluded)
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
      # Save XGBoost model in `model` output folder.
      chr_model_path <- file.path(
        model,
        sprintf("rset_%03d_fold_%02d.ubj", r, f)
      )
      sprk_base_xgb_booster <- sparklyr::invoke(
        sprk_base_xgb_model,
        "nativeBooster"
      )
      sparklyr::invoke(
        sprk_base_xgb_booster,
        "saveModel",
        chr_model_path
      )
      # ------------------------------------------------------------------------
      # Append model information to list.
      list_base_xgb_model[[length(list_base_xgb_model) + 1L]] <- data.frame(
        rset = r,
        fold = f,
        prediction = sprintf("%s_%03d", stage, r),
        path = chr_model_path
      )
      # ------------------------------------------------------------------------
      # Predict model on withheld test data.
      sprk_base_xgb_test <- sparklyr::invoke(
        sprk_base_xgb_model,
        "transform",
        sparklyr::spark_dataframe(tbl_feat_merge_test)
      )
      sprk_base_xgb_pred <- sparklyr::sdf_register(
        sprk_base_xgb_test,
        name = sprintf("beethoven_xgb_preds_%03d_%02d", r, f)
      ) |>
        dplyr::select(tidyselect::all_of(
          c("site_id", "time", "lon", "lat", "label", "prediction")
        )) |>
        sparklyr::sdf_persist(
          storage.level = "MEMORY_AND_DISK",
          name = sprintf("beethoven_xgb_predcache_%03d_%02d", r, f)
        )
      # ------------------------------------------------------------------------
      # Materialize predictions before evaluating or releasing the fold data.
      sparklyr::invoke(
        sparklyr::spark_dataframe(sprk_base_xgb_pred),
        "count"
      )
      # ------------------------------------------------------------------------
      # Evaluate model performance on test data.
      dt_base_xgb_eval <- evaluate(
        sc,
        pred = sprk_base_xgb_pred,
        r = sprintf("%03d", r),
        f = sprintf("%02d", f)
      )
      # ------------------------------------------------------------------------
      # Retain model evaluation statistics.
      list_base_xgb_eval[[r]][[f]] <- data.frame(
        dt_base_xgb_eval,
        time = unname(time_base_xgb_model[["elapsed"]])
      )
      # ------------------------------------------------------------------------
      # Filter to model predictions and original identification values.
      tbl_base_xgb_pred <- sprk_base_xgb_pred |>
        dplyr::select(tidyselect::all_of(
          c("site_id", "time", "lon", "lat", "label", "prediction")
        )) |>
        dplyr::rename(
          !!yvar := tidyselect::all_of("label"),
          !!sprintf("%s_%03d", stage, r) := tidyselect::all_of("prediction")
        )
      # ------------------------------------------------------------------------
      # Retain this fold's cached predictions for combination after fitting.
      list_base_xgb_fold[[f]] <- tbl_base_xgb_pred
      # ------------------------------------------------------------------------
      # Drop training and testing data from Spark.
      unpersist(list(tbl_feat_merge_train, tbl_feat_merge_test))
    }
    # --------------------------------------------------------------------------
    # Row-bind the folds within this resampling set in Spark.
    list_base_xgb_pred[[r]] <- sparklyr::sdf_bind_rows(list_base_xgb_fold)
  }
  message("All models fit; combining predictions.")
  # ----------------------------------------------------------------------------
  # Join resampling sets by observation and retain metadata from the first set.
  tbl_base_xgb_pred <- Reduce(
    f = function(pred, r) {
      dplyr::left_join(
        pred,
        list_base_xgb_pred[[r]] |>
          dplyr::select(tidyselect::all_of(
            c("site_id", "time", sprintf("%s_%03d", stage, r))
          )),
        by = c("site_id", "time")
      )
    },
    x = seq_along(list_base_xgb_pred)[-1L],
    init = list_base_xgb_pred[[1L]]
  )
  # ----------------------------------------------------------------------------
  # Write the complete prediction table as one Parquet dataset.
  sparklyr::spark_write_parquet(
    tbl_base_xgb_pred,
    path = pred,
    mode = "overwrite"
  )
  # ----------------------------------------------------------------------------
  # Extract file names in output after parquet file writing, and limit to only
  # the new .parquet files.
  chr_pred2 <- list.files(
    pred,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.parquet$"
  )
  # ----------------------------------------------------------------------------
  # Return only the newly written .parquet file path(s) and evaluation.
  return(list(
    pred = setdiff(chr_pred2, chr_pred1),
    eval = data.frame(dplyr::bind_rows(list_base_xgb_eval)),
    model = list(
      index = dplyr::bind_rows(list_base_xgb_model),
      xvar = chr_feat_merge_xvar,
      yvar = yvar,
      stage = stage
    )
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

################################################################################
#' Assemble dense Spark feature vectors for XGBoost
#'
#' Projects an existing Spark table, aliases an optional response to `label`,
#' and assembles the supplied predictors into a dense `features` vector in
#' their supplied order. Identification columns are retained alongside the
#' vectors.
#'
#' @param sc A Spark connection created by [sparklyr::spark_connect()].
#' @param chr_feat_merge_sptmpl character(1). Name of a table already registered
#'   in Spark. It must contain `site_id`, `time`, `lon`, `lat`, any supplied
#'   response, and the supplied numeric predictor columns.
#' @param chr_feat_merge_xvar Character vector of numeric predictor names,
#'   selected by the caller. Their order determines the positions
#'   in the feature vector and must match the order used to train the model.
#' @param yvar Optional character(1) response column name, aliased to `label`.
#'   Use `NULL` when assembling prediction data without a response.
#' @param config Configuration list, such as that returned by
#'   [spark_environment()], containing `int_maxrows`. A value of 0 applies no
#'   row limit; a positive value applies SQL `LIMIT` without an ordering clause.
#' @details
#' Predictors are temporarily renamed to `feature_0001`, `feature_0002`, and
#' so on before vector assembly. Vectors are converted to dense form through
#' an array so zero values remain explicit when passed to XGBoost.
#'
#' @return A registered Spark table with columns `site_id`, `time`, `lon`,
#'   `lat`, and `features`, plus `label` when `yvar` is supplied. The `features`
#'   column contains dense vectors with one entry per supplied predictor, in
#'   the supplied order.
#' @seealso [fit_xgb_spark()], [predict_xgb_spark()], [spark_environment()]
#' @keywords internal
assemble_xgb_features <- function(
  sc,
  chr_feat_merge_sptmpl,
  chr_feat_merge_xvar,
  yvar = NULL,
  config
) {
  # ----------------------------------------------------------------------------
  # Replace predictor variable names with generic "feature_" labels.
  chr_feat_merge_xalias <- sprintf(
    "feature_%04d",
    seq_along(chr_feat_merge_xvar)
  )
  # ----------------------------------------------------------------------------
  # Define remapping of variable names to "feature_" labels.
  chr_feat_merge_proj <- paste(
    vapply(chr_feat_merge_xvar, sql_identifier, character(1)),
    "AS",
    vapply(chr_feat_merge_xalias, sql_identifier, character(1))
  )
  # ----------------------------------------------------------------------------
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
        if (!is.null(yvar)) paste(sql_identifier(yvar), "AS `label`"),
        chr_feat_merge_proj
      ),
      collapse = ", "
    ),
    "FROM",
    chr_feat_merge_sptmpl,
    if (config$int_maxrows > 0L) paste("LIMIT", config$int_maxrows) else ""
  )
  # ----------------------------------------------------------------------------
  # Query `dt_feat_merge_sptmpl` table for response and predictor variables.
  db_feat_merge_alias <- sparklyr::sdf_sql(sc, chr_feat_model_sql)
  # ----------------------------------------------------------------------------
  # Assemble the Spark feature vector.
  tbl_feat_merge_ftvec <- db_feat_merge_alias |>
    sparklyr::ft_vector_assembler(
      input_cols = chr_feat_merge_xalias,
      output_col = "features"
    ) |>
    dplyr::select(tidyselect::all_of(
      c(
        "site_id",
        "time",
        "lon",
        "lat",
        if (!is.null(yvar)) "label",
        "features"
      )
    ))
  # ----------------------------------------------------------------------------
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
  sparklyr::invoke(
    sprk_feat_merge_ftvec,
    "withColumn",
    "features",
    sprk_feat_merge_dense
  ) |>
    sparklyr::sdf_register()
}

################################################################################
#' Predict with saved XGBoost fold models on Spark
#'
#' Reads native `.ubj` booster files, predicts each fold on the supplied data,
#' and averages folds within each resampling repeat. Predictors are assembled
#' in the order retained during fitting. Results are written as one Parquet
#' dataset with one prediction column per repeat, named from the model index.
#'
#' @param master,home,workspace,database Spark connection and configuration
#'   arguments passed as in [fit_xgb_spark()].
#' @param input Character path or paths to prediction Parquet data. Data must
#'   contain `site_id`, `time`, `lon`, `lat`, and the saved predictor columns,
#'   with one row per `site_id` and `time`. No response column is required.
#'   Usable paths must be supplied despite the `NULL` default.
#' @param models The complete `model` list returned by [fit_xgb_spark()],
#'   supplied directly or read from a saved R object. Prediction uses `index`
#'   and the ordered `xvar` vector. The index must contain `rset`, `fold`,
#'   `prediction`, and `path` columns, with one native booster file per fold
#'   and one prediction-column name per repeat. Paths must be accessible to
#'   the Spark driver.
#' @param pred character(1). Required output Parquet directory, written with
#'   overwrite mode. A usable path must be supplied despite the `NULL` default.
#'   The directory must be accessible to Spark and R and separate from input
#'   data and model files.
#' @details
#' Uses the same Spark and XGBoost JAR configuration as [fit_xgb_spark()].
#' Native boosters are wrapped as Spark regression models without reading
#' Spark model metadata. Features are made dense to preserve zero values.
#' Prediction covers all input rows, regardless of `XGB_MAX_ROWS`.
#'
#' All indexed folds within each repeat contribute equally to its mean
#' prediction. Repeat-specific tables are joined by `site_id`, `time`, `lon`,
#' and `lat`. The saved dataset contains these identifiers and one prediction
#' column per repeat, such as `base_001` or `meta_001`. The Spark connection
#' is disconnected on exit, including on error.
#'
#' @return A character vector of absolute `.parquet` part-file paths beneath
#'   `pred`, suitable for saving as a workflow input to a subsequent stage.
#' @seealso [fit_xgb_spark()], [assemble_xgb_features()]
#' @keywords internal
predict_xgb_spark <- function(
  master,
  home,
  workspace,
  database = "duckdb",
  input = NULL,
  models,
  pred = NULL
) {
  # ----------------------------------------------------------------------------
  # Group saved fold models by resampling repeat.
  list_model_index <- split(models$index, models$index$rset)
  chr_ids <- c("site_id", "time", "lon", "lat")
  # ----------------------------------------------------------------------------
  # Spark configuration.
  config <- spark_environment(workspace = workspace, database = database)
  config$int_maxrows <- 0L
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
  # ----------------------------------------------------------------------------
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
  message("Spark connected; proceeding with prediction...")
  # ----------------------------------------------------------------------------
  # Load .parquet file into Spark memory.
  chr_feat_merge_sptmpl <- "db_feat_merge_sptmpl"
  sparklyr::spark_read_parquet(
    sc,
    name = chr_feat_merge_sptmpl,
    path = input,
    options = list(mergeSchema = "true"),
    memory = FALSE
  )
  # ----------------------------------------------------------------------------
  # Use the predictor order retained during fitting.
  chr_feat_merge_xvar <- models$xvar
  # ----------------------------------------------------------------------------
  # Assemble prediction features without a response column.
  tbl_feat_merge_assemble <- assemble_xgb_features(
    sc = sc,
    chr_feat_merge_sptmpl = chr_feat_merge_sptmpl,
    chr_feat_merge_xvar = chr_feat_merge_xvar,
    yvar = NULL,
    config = config
  )
  # ----------------------------------------------------------------------------
  # Reuse the assembled grid across all fold models.
  tbl_feat_merge_persist <- sparklyr::sdf_persist(
    tbl_feat_merge_assemble,
    storage.level = "MEMORY_AND_DISK",
    name = "beethoven_xgb_grid_features"
  )
  sparklyr::invoke(sparklyr::spark_dataframe(tbl_feat_merge_persist), "count")
  sprk_summary <- sparklyr::invoke_static(sc, "scala.Option", "empty")
  list_grid_pred <- vector("list", length(list_model_index))
  # ----------------------------------------------------------------------------
  for (r in seq_along(list_model_index)) {
    dt_model_repeat <- list_model_index[[r]]
    chr_prediction <- dt_model_repeat$prediction[[1L]]
    list_grid_fold <- vector("list", nrow(dt_model_repeat))
    for (f in seq_len(nrow(dt_model_repeat))) {
      # ------------------------------------------------------------------------
      # Read the native booster and restore its Spark regression-model wrapper.
      sprk_booster <- sparklyr::invoke_static(
        sc,
        "ml.dmlc.xgboost4j.scala.XGBoost",
        "loadModel",
        dt_model_repeat$path[[f]]
      )
      sparklyr::invoke(
        sprk_booster,
        "setParam",
        "nthread",
        as.character(config$int_nthreads)
      )
      sprk_model <- sparklyr::invoke_new(
        sc,
        "ml.dmlc.xgboost4j.scala.spark.XGBoostRegressionModel",
        sprintf("beethoven_xgb_grid_%03d_%02d", r, f),
        sprk_booster,
        sprk_summary
      ) |>
        sparklyr::invoke("setFeaturesCol", "features") |>
        sparklyr::invoke("setPredictionCol", "prediction")
      # ------------------------------------------------------------------------
      # Keep predictions and identifiers together throughout averaging.
      list_grid_fold[[f]] <- sparklyr::invoke(
        sprk_model,
        "transform",
        sparklyr::spark_dataframe(tbl_feat_merge_persist)
      ) |>
        sparklyr::sdf_register() |>
        dplyr::select(tidyselect::all_of(c(chr_ids, "prediction")))
    }
    # --------------------------------------------------------------------------
    # Average folds within this repeat, then materialize the result for joining.
    list_grid_pred[[r]] <- sparklyr::sdf_bind_rows(list_grid_fold) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(chr_ids))) |>
      dplyr::summarise(
        !!chr_prediction := mean(.data$prediction),
        .groups = "drop"
      ) |>
      sparklyr::sdf_persist(
        storage.level = "MEMORY_AND_DISK",
        name = sprintf("beethoven_xgb_grid_repeat_%03d", r)
      )
    sparklyr::invoke(sparklyr::spark_dataframe(list_grid_pred[[r]]), "count")
  }
  message("Predictions completed; writing .parquet file(s)...")
  # ----------------------------------------------------------------------------
  # Join repeat-specific columns by identifiers rather than Spark row order.
  tbl_grid_pred <- Reduce(
    function(x, y) dplyr::left_join(x, y, by = chr_ids),
    list_grid_pred
  )
  # ----------------------------------------------------------------------------
  # Persist full-grid predictions and return paths for the next workflow step.
  dir.create(pred, recursive = TRUE, showWarnings = FALSE)
  pred <- normalizePath(pred, mustWork = TRUE)
  sparklyr::spark_write_parquet(tbl_grid_pred, path = pred, mode = "overwrite")
  list.files(pred, recursive = TRUE, full.names = TRUE, pattern = "\\.parquet$")
}

# nocov end
