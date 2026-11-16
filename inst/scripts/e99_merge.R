################################################################################
# Merge all calculated covariates at Uber H3 (resolution 8) locations.

################################################################################
# Load libraries and source local functions.
library(beethoven)
library(amadeusdb)

################################################################################
# Import configured variables and inputs.
db_pred_grid_sptmpl <- snakemake@input[["db_pred_grid_sptmpl"]]
db_pred_geos_sptmpl <- qs2::qs_read(snakemake@input[["db_pred_geos_sptmpl"]])
db_pred_hms_sptmpl <- qs2::qs_read(snakemake@input[["db_pred_hms_sptmpl"]])

################################################################################
db_pred_merge_sptmpl <- amadeusdb::join_covariates(
  input = db_pred_grid_sptmpl,
  output = "output/grid/join/",
  hourly = list(geos = db_pred_geos_sptmpl),
  daily = list(hms = db_pred_hms_sptmpl),
  static = NULL,
  locs_id = "site_id",
  prefix = FALSE
)

################################################################################
qs2::qs_save(db_pred_merge_sptmpl, snakemake@output[["db_pred_merge_sptmpl"]])
