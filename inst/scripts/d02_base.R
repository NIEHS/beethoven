################################################################################
# Development script for spark-dispatched XGBoost base learner models.
# Mitchell Manware
# 16 October 2026

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
chr_path_merge_sptmpl <- snakemake@input[["dt_feat_merge_sptmpl"]]

################################################################################
# Import Spark connection details.
chr_spark_master <- readLines("input/.hidden/chr_spark_master.txt")
chr_spark_home <- readLines("input/.hidden/chr_spark_home.txt")

################################################################################
# Fit XGBoost models in Spark; collect full predictions and performance metrics.
time_base_fit <- system.time({
  list_base_xgb_pred <- fit_base_spark(
    master = chr_spark_master,
    home = chr_spark_home,
    workspace = file.path(getwd(), "spark-warehouse"),
    database = "duckdb",
    path = chr_path_merge_sptmpl,
    iter = 100L,
    xvar = list_base_params_static$xvar,
    yvar = list_base_params_static$yvar,
    dvar = list_base_params_static$dvar,
    stage = "base"
  )
})

################################################################################
arrow::write_parquet(
  list_base_xgb_pred$pred,
  snakemake@output[["dt_base_xgb_pred"]]
)
qs2::qs_save(list_base_xgb_pred$eval, snakemake@output[["dt_base_xgb_eval"]])
