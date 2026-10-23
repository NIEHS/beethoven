################################################################################
# Calculate HMS covariates at unique AQS locations (spatial only).

################################################################################
# Load libraries and source local functions.
library(amadeusdb)

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
sf_feat_aqs_sp <- qs2::qs_read(snakemake@input[["sf_feat_aqs_sp"]])

################################################################################
# Dynamic load of GDAL library to be used by SedonaDB.
dyn.load(
  file.path(Sys.getenv("HOME"), ".conda/envs/beethoven/lib/libgdal.so.39"),
  local = FALSE,
  now = TRUE
)

################################################################################
db_feat_hms_sptmpl <- amadeusdb:::sedona_hms(
  date = beethoven::fl_dates(chr_init_dates),
  input = file.path(chr_config_dir, "hms", "data_files"),
  output = "output/hms/",
  locs = sf_feat_aqs_sp,
  locs_id = "site_id",
  radius = 0,
  geom = FALSE,
  batch = 50L
)

################################################################################
qs2::qs_save(db_feat_hms_sptmpl, snakemake@output[["db_feat_hms_sptmpl"]])
