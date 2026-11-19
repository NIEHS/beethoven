################################################################################
# Development script for spark-dispatched XGBoost meta learner models.
# Mitchell Manware
# 20 October 2026

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
db_train_pred_base_oof_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_train_pred_base_oof_sptmpl"
]])
list_meta_cv_idx <- qs2::qs_read(snakemake@input[["list_meta_cv_idx"]])

################################################################################
# Import Spark connection details.
chr_spark_master <- readLines("input/.hidden/chr_spark_local.txt")
chr_spark_home <- readLines("input/.hidden/chr_spark_home.txt")

################################################################################
# Set Spark cluster environmental variables.
Sys.setenv(
  SPARK_CORES_MAX = "64",
  SPARK_EXECUTOR_CORES = "2",
  SPARK_EXECUTOR_MEMORY = "4g",
  SPARK_SHUFFLE_PARTITIONS = "128",
  XGB_NUM_WORKERS = "32",
  XGB_NTHREAD = "2",
  XGB_NUM_ROUND = "100"
)
################################################################################
# Extract base learner column names from parquet file.
chr_train_pred_base_names <- grep(
  "base_",
  arrow::open_dataset(
    db_train_pred_base_oof_sptmpl,
    format = "parquet",
    unify_schemas = TRUE
  )$schema$names,
  value = TRUE
)

################################################################################
# Fit XGBoost models in Spark; collect full predictions and performance metrics.
list_meta_xgb_fit <- fit_xgb_spark(
  master = chr_spark_master,
  home = chr_spark_home,
  workspace = file.path(getwd(), "spark-warehouse"),
  database = "duckdb",
  input = db_train_pred_base_oof_sptmpl,
  pred = "output/train/pred/meta/oof/data/",
  model = "output/model/meta/",
  rset = list_meta_cv_idx,
  xvar = c("lon", "lat", chr_train_pred_base_names),
  yvar = list_base_params_static$yvar,
  dvar = list_base_params_static$dvar,
  stage = "meta"
)

################################################################################
qs2::qs_save(
  list_meta_xgb_fit$pred,
  snakemake@output[["db_train_pred_meta_oof_sptmpl"]]
)
qs2::qs_save(list_meta_xgb_fit$eval, snakemake@output[["dt_meta_xgb_eval"]])
qs2::qs_save(
  list_meta_xgb_fit$model,
  snakemake@output[["list_meta_xgb_model"]]
)
