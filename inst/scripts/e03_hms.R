################################################################################
# Calculate HMS covariates at Uber H3 (resolution 8) locations (spatial only).

################################################################################
# Load libraries and source local functions.
library(amadeus)
source("R/amadeus.R")

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
sf_pred_grid_sp <- qs2::qs_read(snakemake@input[["sf_pred_grid_sp"]])

################################################################################
mirai::daemons(100)
dt_pred_hms_sptmpl <- suppressMessages(
  calculate_hms_map(
    from = amadeus::process_hms(
      date = beethoven::fl_dates(chr_init_dates),
      path = file.path(chr_config_dir, "hms", "data_files")
    ),
    locs = sf_pred_grid_sp,
    locs_id = "site_id",
    radius = 0,
    fun = "mean",
    geom = FALSE
  )
)
mirai::daemons(0)

################################################################################
qs2::qs_save(
  data.table::data.table(dt_pred_hms_sptmpl),
  snakemake@output[["dt_pred_hms_sptmpl"]]
)
