################################################################################
# Development script for spark-dispatched XGBoost base learner models.
# Mitchell Manware
# 16 October 2026

################################################################################
# Reference scripts from Kyle:
# -- 01_sparklyr_shuffle.R
# -- 02_create_sedonadb_table.R
# -- 02_sparklyr_xgboost.R

################################################################################
# Load libraries and source local functions.
library(DBI)
library(dplyr)
library(sparklyr)
source("R/spark.R")

################################################################################
# Import configured variables and inputs.
list_base_params_static <- qs2::qs_read(snakemake@input[[
  "list_base_params_static"
]])
chr_path_merge_sptmpl <- snakemake@input[["dt_feat_merge_sptmpl"]]
chr_feat_merge_sptmpl <- gsub("output/|\\.parquet", "", chr_path_merge_sptmpl)

################################################################################
# Import Spark connection details.
chr_spark_master <- readLines("input/.hidden/chr_spark_master.txt")
chr_spark_home <- readLines("input/.hidden/chr_spark_home.txt")

################################################################################
# Populate working environment with environment-defined variables.
environment(workspace = "beethoven/beethoven/", database = "duckdb")

################################################################################
# Configuration.
config_beethoven <- sparklyr::spark_config()
chr_xgboost_jars <- Sys.getenv("XGBOOST_JARS", unset = "")
if (nzchar(chr_xgboost_jars)) {
  config_beethoven[["sparklyr.shell.jars"]] <- chr_xgboost_jars
} else {
  config_beethoven[["sparklyr.shell.packages"]] <- chr_xgboost_package
}

################################################################################
# Connect to local Spark cluster.
sc_beethoven <- sparklyr::spark_connect(
  master = chr_spark_master,
  spark_home = chr_spark_home,
  config = config_beethoven
  # app_name = "beethoven-devel"
)
on.exit()

################################################################################
# Load .parquet file into Spark memory.
db_feat_merge_sptmpl <- sparklyr::spark_read_parquet(
  sc_beethoven,
  name = chr_feat_merge_sptmpl,
  path = chr_path_merge_sptmpl
)

################################################################################
# Read data schema.
dt_feat_merge_schema <- DBI::dbGetQuery(
  sc_beethoven,
  paste0("DESCRIBE TABLE ", chr_feat_merge_sptmpl)
)
dt_feat_merge_schema <- dt_feat_merge_schema[
  nzchar(trimws(dt_feat_merge_schema$col_name)) &
    !startsWith(trimws(dt_feat_merge_schema$col_name), "#"),
  ,
  drop = FALSE
]

################################################################################
# Extract and define numeric features (predictors).
bin_feat_merge_num <- grepl(
  "^(tinyint|smallint|int|bigint|float|double|decimal)",
  tolower(dt_feat_merge_schema$data_type)
)
chr_feat_merge_xvar <- setdiff(
  dt_feat_merge_schema$col_name[bin_feat_merge_num],
  c(list_base_params_static$yvar, list_base_params_static$dvar)
)

################################################################################
# Check derived features identical to manually set `$xvar`.
stopifnot(identical(chr_feat_merge_xvar, list_base_params_static$xvar))

################################################################################
# Replace predictor variable names with generic "feature_" labels.
chr_feat_merge_xalias <- sprintf("feature_%04d", seq_along(chr_feat_merge_xvar))

################################################################################
# Define remapping of variable names to "feature_" labels.
chr_feat_merge_proj <- paste(
  vapply(chr_feat_merge_xvar, sql_identifier, character(1)),
  "AS",
  vapply(chr_feat_merge_xalias, sql_identifier, character(1))
)

################################################################################
# Define SQL query for response and predictor variables with alias renaming
# from the `dt_feat_merge_sptmpl` table, read from .parquet in Line 76.
chr_feat_model_sql <- paste(
  "SELECT",
  paste(
    c(
      paste(sql_identifier(list_base_params_static$yvar), "AS `label`"),
      chr_feat_merge_proj
    ),
    collapse = ", "
  ),
  "FROM",
  chr_feat_merge_sptmpl,
  if (int_maxrows > 0L) paste("LIMIT", int_maxrows) else ""
)

################################################################################
# Query `dt_feat_merge_sptmpl` table for response and predictor variables.
db_feat_merge_alias <- sparklyr::sdf_sql(sc_beethoven, chr_feat_model_sql)

################################################################################
# Assemble the Spark feature vector.
tbl_feat_merge_ftvec <- db_feat_merge_alias |>
  sparklyr::ft_vector_assembler(
    input_cols = chr_feat_merge_xalias,
    output_col = "features"
  ) |>
  dplyr::select(label, features)

################################################################################
# VectorAssembler chooses SparseVector when a row contains many zeros. In
# XGBoost, absent sparse entries can instead mean missing values. Round-trip
# through an array so Spark returns a DenseVector and every zero is retained.
sprk_feat_merge_ftvec <- sparklyr::spark_dataframe(tbl_feat_merge_ftvec)
sprk_feat_merge_features <- sparklyr::invoke(
  sprk_feat_merge_ftvec,
  "col",
  "features"
)
sprk_feat_merge_array <- sparklyr::invoke_static(
  sc_beethoven,
  "org.apache.spark.ml.functions",
  "vector_to_array",
  sprk_feat_merge_features,
  "float64"
)
sprk_feat_merge_dense <- sparklyr::invoke_static(
  sc_beethoven,
  "org.apache.spark.ml.functions",
  "array_to_vector",
  sprk_feat_merge_array
)
tbl_feat_merge_assemble <- sparklyr::invoke(
  sprk_feat_merge_ftvec,
  "withColumn",
  "features",
  sprk_feat_merge_dense
) %>%
  sparklyr::sdf_register()

################################################################################
# Set Spark estimator and invoke XGBoost (manual workers and threads - devel).
int_nworkers <- 1L
int_nthreads <- 1L
sprk_estimator <- sparklyr::invoke_new(
  sc_beethoven,
  "ml.dmlc.xgboost4j.scala.spark.XGBoostRegressor"
)
sprk_estimator <- sprk_estimator %>%
  sparklyr::invoke("setLabelCol", "label") %>%
  sparklyr::invoke("setFeaturesCol", "features") %>%
  sparklyr::invoke("setPredictionCol", "prediction") %>%
  sparklyr::invoke("setObjective", "reg:squarederror") %>%
  sparklyr::invoke("setEvalMetric", "rmse") %>%
  sparklyr::invoke("setTreeMethod", "hist") %>%
  sparklyr::invoke("setDevice", "cpu") %>%
  sparklyr::invoke("setNumWorkers", int_nworkers) %>%
  sparklyr::invoke("setNthread", int_nthreads) %>%
  sparklyr::invoke("setNumRound", int_nround) %>%
  sparklyr::invoke("setMaxDepth", 8L) %>%
  sparklyr::invoke("setEta", 0.1) %>%
  sparklyr::invoke("setSubsample", 0.8) %>%
  sparklyr::invoke("setColsampleBytree", 0.8) %>%
  sparklyr::invoke("setSeed", int_seed) %>%
  sparklyr::invoke("setForceRepartition", TRUE)

################################################################################
# Split data into train and test sets.
list_feat_merge_splits <- sparklyr::sdf_random_split(
  tbl_feat_merge_assemble,
  training = 0.8,
  test = 0.2,
  seed = 12345
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

################################################################################
# Collect and report training/test splits and count.
df_base_splits <- dplyr::bind_rows(
  tbl_feat_merge_train |>
    dplyr::summarise(split = "training", rows = n()) |>
    dplyr::collect(),
  tbl_feat_merge_test |>
    dplyr::summarise(split = "test", rows = n()) |>
    dplyr::collect()
)

################################################################################
# Fit XGBoost model on Spark cluster.
message(
  "Fitting XGBoost with ",
  int_nworkers,
  " workers ",
  int_nthreads,
  " threads and ",
  int_nround,
  " boosting rounds..."
)
time_base_xgb_model <- system.time({
  sprk_base_xgb_model <- sparklyr::invoke(
    sprk_estimator,
    "fit",
    sparklyr::spark_dataframe(tbl_feat_merge_train)
  )
})
message(
  "XGBoost fit elapsed: ",
  unname(time_base_xgb_model[["elapsed"]]),
  " seconds."
)

################################################################################
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

################################################################################
# Evaluate model performance.
eval_base_xgb_pred <- sparklyr::ml_regression_evaluator(
  sc_beethoven,
  label_col = "label",
  prediction_col = "prediction",
  metric_name = "rmse"
)
num_base_xgb_rmse <- sparklyr::ml_evaluate(
  eval_base_xgb_pred,
  sprk_base_xgb_pred
)
cat("Test RMSE:", num_base_xgb_rmse, "\n")

################################################################################
# Pull model predictions and original identification values.
dt_base_xgb_pred <- sprk_base_xgb_pred %>%
  sparklyr::select(label, prediction) %>%
  sparklyr::collect()

################################################################################
# Disconnect from local Spark cluster.
sparklyr::spark_disconnect(sc_beethoven)

################################################################################
qs2::qs_save(dt_base_xgb_pred, snakemake@output[["dt_base_xgb_pred"]])
