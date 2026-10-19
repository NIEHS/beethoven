################################################################################
# Development script for tidymodels x spark XGBoost model development.
# Mitchell Manware
# 19 October 2026

################################################################################
# Reference scripts from Kyle:
# -- 01_sparklyr_shuffle.R
# -- 02_create_sedonadb_table.R
# -- 02_sparklyr_xgboost.R
# -- d02_spark.R

################################################################################
# Load libraries and source local functions.
library(DBI)
library(dplyr)
library(tidymodels)
library(sparklyr)
source("R/spark.R")

################################################################################
# Import configured variables and inputs.
# list_base_params_static <- qs2::qs_read(snakemake@input[[
#   "list_base_params_static"
# ]])
# list_base_cv_rset <- qs2::qs_read(snakemake@input[["list_base_cv_rset"]])
# chr_path_merge_sptmpl <- snakemake@input[["dt_feat_merge_sptmpl"]]
# chr_feat_merge_sptmpl <- gsub("output/|\\.parquet", "", chr_path_merge_sptmpl)
list_base_params_static <- qs2::qs_read("output/list_base_params_static.qs")
list_base_cv_rset <- qs2::qs_read("output/list_base_cv_rset.qs")
chr_path_merge_sptmpl <- "output/dt_feat_merge_sptmpl.parquet"

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

################################################################################
# Define {xgboost} engine and device.
spec_xgboost <- parsnip::boost_tree(
  mtry = tune::tune(),
  trees = 10,
  tree_depth = tune::tune(),
  learn_rate = tune::tune()
) |>
  parsnip::set_engine("xgboost", device = "cpu") |>
  parsnip::set_mode("regression")

################################################################################
# Apply spark-enabled base learner function.
source("R/devel.R")
spark_results <- fit_base_spark(
  rset = list_base_cv_rset[[1]],
  full_data = list_base_params_static$data,
  xvar = list_base_params_static$xvar,
  yvar = list_base_params_static$yvar,
  normalize = TRUE,
  metric = "rmse",
  model = spec_xgboost,
  sc = sc_beethoven
)

################################################################################
# Inspect results
spark_results
tune::collect_metrics(spark_results)
tune::collect_predictions(spark_results)
