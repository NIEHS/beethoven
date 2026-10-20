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
source("R/devel.R")

################################################################################
# Import configured variables and inputs.
list_base_params_static <- qs2::qs_read(snakemake@input[[
  "list_base_params_static"
]])
chr_path_base_sptmpl <- snakemake@input[["dt_base_xgb_pred"]]
list_meta_cv_idx <- qs2::qs_read(snakemake@input[["list_meta_cv_idx"]])

################################################################################
# Import Spark connection details.
chr_spark_master <- readLines("input/.hidden/chr_spark_master.txt")
chr_spark_home <- readLines("input/.hidden/chr_spark_home.txt")

################################################################################
# Increase executor memory for meta learner model with 100 features.
Sys.setenv("SPARK_EXECUTOR_MEMORY" = "4g")

################################################################################
# Extract base learner column names from parquet file.
chr_feat_base_xgb <- grep(
  "base_",
  arrow::open_dataset(
    chr_path_base_sptmpl,
    format = "parquet"
  )$schema$names,
  value = TRUE
)

################################################################################
# Fit XGBoost models in Spark; collect full predictions and performance metrics.
time_meta_fit <- system.time({
  list_meta_xgb_pred <- fit_xgb_spark(
    master = chr_spark_master,
    home = chr_spark_home,
    workspace = file.path(getwd(), "spark-warehouse"),
    database = "duckdb",
    path = chr_path_base_sptmpl,
    rset = list_meta_cv_idx,
    xvar = c("lon", "lat", chr_feat_base_xgb),
    yvar = list_base_params_static$yvar,
    dvar = list_base_params_static$dvar,
    stage = "meta"
  )
})

################################################################################
arrow::write_parquet(
  list_meta_xgb_pred$pred,
  snakemake@output[["dt_meta_xgb_pred"]]
)
qs2::qs_save(list_meta_xgb_pred$eval, snakemake@output[["dt_meta_xgb_eval"]])
