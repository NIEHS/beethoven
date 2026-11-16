################################################################################
# Merge all calculated covariates at Uber H3 (resolution 8) locations.

################################################################################
# Load libraries and source local functions.
library(beethoven)
library(amadeusdb)

################################################################################
# Import configured variables and inputs.
db_grid_index_sptmpl <- snakemake@input[["db_grid_index_sptmpl"]]
db_grid_feat_geos_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_grid_feat_geos_sptmpl"
]])
db_grid_feat_hms_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_grid_feat_hms_sptmpl"
]])

################################################################################
db_grid_feat_sptmpl <- amadeusdb::join_covariates(
  input = db_grid_index_sptmpl,
  output = "output/grid/feat/merge/",
  hourly = list(geos = db_grid_feat_geos_sptmpl),
  daily = list(hms = db_grid_feat_hms_sptmpl),
  static = NULL,
  locs_id = "site_id",
  prefix = FALSE
)

################################################################################
qs2::qs_save(db_grid_feat_sptmpl, snakemake@output[["db_grid_feat_sptmpl"]])
