################################################################################
# Merge all calculated covariates at unique AQS locations, fill time.

################################################################################
# Load libraries and source local functions.
library(beethoven)
library(amadeusdb)

################################################################################
# Import configured variables and inputs.
db_feat_aqs_sptmpl <- snakemake@input[["db_feat_aqs_sptmpl"]]
db_feat_geos_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_feat_geos_sptmpl"
]])
db_feat_hms_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_feat_hms_sptmpl"
]])

################################################################################
db_feat_merge_sptmpl <- amadeusdb:::join_covariates(
  input = db_feat_aqs_sptmpl,
  output = "output/join/",
  hourly = list(geos = db_feat_geos_sptmpl[1:4]),
  daily = list(hms = db_feat_hms_sptmpl),
  static = NULL,
  locs_id = "site_id",
  prefix = FALSE
)

################################################################################
qs2::qs_save(db_feat_merge_sptmpl, snakemake@output[["db_feat_merge_sptmpl"]])
