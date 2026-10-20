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
chr_path_base_sptmpl <- snakemake@input[["dt_base_xgb_eval"]]

################################################################################
# Import Spark connection details.
chr_spark_master <- readLines("input/.hidden/chr_spark_master.txt")
chr_spark_home <- readLines("input/.hidden/chr_spark_home.txt")

################################################################################
# Fit XGBoost models in Spark; collect full predictions and performance metrics.
time_meta_fit <- system.time({
  list_meta_xgb_pred <- fit_base_spark(
    master = chr_spark_master,
    home = chr_spark_home,
    workspace = file.path(getwd(), "spark-warehouse"),
    database = "duckdb",
    path = chr_path_base_sptmpl,
    iter = 25L,
    xvar = setdiff(
      names(arrow::read_parquet(chr_path_base_sptmpl)),
      c(list_base_params_static$yvar, list_base_params_static$dvar)
    ),
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
