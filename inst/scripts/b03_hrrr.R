################################################################################
# Download HRRR data (.grib files) for all dates.

################################################################################
# Load libraries and source local functions.
library(amadeus)

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])

################################################################################
int_ndaemons <- 33L
mirai::daemons(int_ndaemons)
list_dl_hrrr <- amadeus::download_hrrr(
  product = "2d surface",
  sector = "conus",
  cycle_runtime = 0:23L,
  forecast_hour = 0:1L,
  directory_to_save = file.path(chr_config_dir, "hrrr"),
  date = as.character(beethoven::fl_dates(chr_init_dates)),
  acknowledgement = TRUE,
  hash = FALSE,
  rate_limit = 2 / int_ndaemons
)
mirai::daemons(0)

################################################################################
qs2::qs_save(list_dl_hrrr, snakemake@output[["list_dl_hrrr"]])
