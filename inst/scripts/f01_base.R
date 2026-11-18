################################################################################
# Predict base learners at Uber H3 hexagon grid locations.

################################################################################
# Load libraries and source local functions.
library(beethoven)
source("R/spark.R")

################################################################################
# Import configured variables and inputs.
list_base_xgb_model <- qs2::qs_read(snakemake@input[["list_base_xgb_model"]])
db_grid_feat_sptmpl <- qs2::qs_read(snakemake@input[["db_grid_feat_sptmpl"]])

################################################################################
# Set Spark cluster environmental variables.
Sys.setenv("SPARK_CORES_MAX" = 128L)
Sys.setenv("SPARK_EXECUTOR_MEMORY" = "8g")

################################################################################
# Import Spark connection details.
chr_spark_master <- readLines("input/.hidden/chr_spark_local.txt")
chr_spark_home <- readLines("input/.hidden/chr_spark_home.txt")

################################################################################
# Predict at H3 sites.
db_grid_pred_base_mean_sptmpl <- predict_xgb_spark(
  master = chr_spark_master,
  home = chr_spark_home,
  workspace = file.path(getwd(), "spark-warehouse"),
  database = "duckdb",
  input = db_grid_feat_sptmpl,
  models = list_base_xgb_model,
  pred = "output/grid/pred/base/mean/data/"
)

################################################################################
qs2::qs_save(
  db_grid_pred_base_mean_sptmpl,
  snakemake@output[["db_grid_pred_base_mean_sptmpl"]]
)
