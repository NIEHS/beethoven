################################################################################
# Merge all calculated covariates at unique AQS locations, fill time.

################################################################################
# Load libraries and source local functions.
library(beethoven)
library(amadeusdb)

################################################################################
# Import configured variables and inputs.
db_train_obs_aqs_sptmpl <- snakemake@input[["db_train_obs_aqs_sptmpl"]]
db_train_feat_geos_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_train_feat_geos_sptmpl"
]])
db_train_feat_hms_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_train_feat_hms_sptmpl"
]])
db_train_feat_spacetime_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_train_feat_spacetime_sptmpl"
]])

################################################################################
db_train_feat_sptmpl <- amadeusdb::join_covariates(
  input = db_train_obs_aqs_sptmpl,
  output = "output/train/feat/merge/",
  hourly = list(
    geos = db_train_feat_geos_sptmpl,
    spacetime = db_train_feat_spacetime_sptmpl
  ),
  daily = list(hms = db_train_feat_hms_sptmpl),
  static = NULL,
  locs_id = "site_id",
  prefix = FALSE
)

################################################################################
qs2::qs_save(db_train_feat_sptmpl, snakemake@output[["db_train_feat_sptmpl"]])
