################################################################################
# Process AQS locations as {sf} and {data.table}.

################################################################################
# Load libraries and source local functions.
library(amadeus)
source("R/beethoven.R")

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])

################################################################################
# Import download lists.
list_dl_aqs <- qs2::qs_read(snakemake@input[["list_dl_aqs"]])
list_dl_narr <- qs2::qs_read(snakemake@input[["list_dl_narr"]])
list_dl_hms <- qs2::qs_read(snakemake@input[["list_dl_hms"]])
list_dl_collect <- list(
  aqs = list_dl_aqs,
  narr = list_dl_narr,
  hms = list_dl_hms
)

################################################################################
# Import unique AQS monitoring locations across defined time period.
# !!! Does not import the monitored data values - location unique !!!
# !!! identifiers and latitude/longitude coordinates only.        !!!
sf_feat_aqs_sp <- amadeus::process_aqs(
  path = file.path(chr_config_dir, "aqs", "data_files"),
  date = fl_dates(chr_init_dates),
  mode = "location",
  data_field = "Arithmetic.Mean",
  return_format = "sf"
)

################################################################################
# Import spatiotemporal AQS monitoring locations with monitored data values
# across defined time period.
dt_feat_aqs_sptmpl <- amadeus::process_aqs(
  path = list.files(
    path = file.path(chr_config_dir, "aqs", "data_files"),
    pattern = "daily_88101_[0-9]{4}.csv",
    full.names = TRUE
  ),
  date = fl_dates(chr_init_dates),
  mode = "available-data",
  data_field = c("Arithmetic.Mean", "Event.Type"),
  return_format = "data.table"
)

################################################################################
qs2::qs_save(sf_feat_aqs_sp, snakemake@output[["sf_feat_aqs_sp"]])
qs2::qs_save(
  data.table::data.table(dt_feat_aqs_sptmpl),
  snakemake@output[["dt_feat_aqs_sptmpl"]]
)
