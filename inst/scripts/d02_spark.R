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
source("R/devel.R")

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
# Fit XGBoost models in Spark; collect full predictions and performance metrics.
list_base_xgb_pred <- fit_base_spark(
  master = readLines("input/.hidden/chr_spark_master.txt"),
  home = readLines("input/.hidden/chr_spark_home.txt"),
  database = "duckdb",
  path = chr_path_merge_sptmpl,
  iter = list_base_params_static$n_models,
  xvar = list_base_params_static$xvar,
  yvar = list_base_params_static$yvar,
  dvar = list_base_params_static$dvar
)

################################################################################
qs2::qs_save(list_base_xgb_pred$pred, snakemake@output[["dt_base_xgb_pred"]])
qs2::qs_save(list_base_xgb_pred$eval, snakemake@output[["dt_base_xgb_eval"]])
