################################################################################
# Download HMS data (.shp files) for all dates.

################################################################################
# Load libraries and source local functions.
library(amadeus)
source("R/amadeus.R")
source("R/beethoven.R")

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])

################################################################################
mirai::daemons(100)
list_dl_hms <- download_hms_map(
  directory_to_save = file.path(chr_config_dir, "hms"),
  date = fl_dates(chr_init_dates),
  acknowledgement = TRUE,
  hash = FALSE,
  remove_zip = FALSE
)
mirai::daemons(0)

################################################################################
qs2::qs_save(list_dl_hms, snakemake@output[["list_dl_hms"]])
