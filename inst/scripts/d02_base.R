################################################################################
# Development script for spark-dispatched XGBoost base learner models.
# Mitchell Manware
# 27 October 2026

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
list_base_cv_idx <- qs2::qs_read(snakemake@input[["list_base_cv_idx"]])
db_train_feat_sptmpl <- qs2::qs_read(snakemake@input[["db_train_feat_sptmpl"]])

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
# Import Spark connection details.
chr_spark_master <- readLines("input/.hidden/chr_spark_local.txt")
chr_spark_home <- readLines("input/.hidden/chr_spark_home.txt")

################################################################################
# Fit XGBoost models in Spark; collect full predictions and performance metrics.
list_base_xgb_fit <- fit_xgb_spark(
  master = chr_spark_master,
  home = chr_spark_home,
  workspace = file.path(getwd(), "spark-warehouse"),
  database = "duckdb",
  input = db_train_feat_sptmpl,
  pred = "output/train/pred/base/oof/data/",
  model = "output/model/base/",
  rset = list_base_cv_idx,
  xvar = list_base_params_static$xvar,
  yvar = list_base_params_static$yvar,
  dvar = list_base_params_static$dvar,
  stage = "base"
)

################################################################################
qs2::qs_save(
  list_base_xgb_fit$pred,
  snakemake@output[["db_train_pred_base_oof_sptmpl"]]
)
qs2::qs_save(list_base_xgb_fit$eval, snakemake@output[["dt_base_xgb_eval"]])
qs2::qs_save(
  list_base_xgb_fit$model,
  snakemake@output[["list_base_xgb_model"]]
)
