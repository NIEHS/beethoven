################################################################################
# Calculate spatiotemporal covariates at unique AQS locations (spatial only).

################################################################################
# Load libraries and source local functions.
library(beethoven)

################################################################################
# Import configured variables and inputs.
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
sf_train_sites_sp <- qs2::qs_read(snakemake@input[["sf_train_sites_sp"]])

################################################################################
# Calculate spacetime covariates at AQS locations, stored as .parquet.
chr_fl_dates <- beethoven::fl_dates(chr_init_dates)
db_train_feat_spacetime_sptmpl <- beethoven::calculate_spacetime(
  locs = sf_train_sites_sp,
  date = chr_init_dates,
  hour = 0:23,
  trend = c(chr_fl_dates[1], chr_fl_dates[2] + 1),
  output = "output/train/feat/spacetime/db_train_feat_spacetime_sptmpl.parquet",
  overwrite = TRUE
)

################################################################################
qs2::qs_save(
  db_train_feat_spacetime_sptmpl,
  snakemake@output[["db_train_feat_spacetime_sptmpl"]]
)
