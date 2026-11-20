################################################################################
# Process AQS locations as {sf} and {data.table}.

################################################################################
# Load libraries and source local functions.
library(amadeus)

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])

################################################################################
# Import download lists.
list_dl_collect <- list(
  aqs = qs2::qs_read(snakemake@input[["list_dl_aqs"]]),
  geos = qs2::qs_read(snakemake@input[["list_dl_geos"]]),
  hrrr = qs2::qs_read(snakemake@input[["list_dl_hrrr"]]),
  hms = qs2::qs_read(snakemake@input[["list_dl_hms"]])
)

################################################################################
# Import unique AQS monitoring locations across defined time period.
# !!! Does not import the monitored data values - location unique !!!
# !!! identifiers and latitude/longitude coordinates only.        !!!
sf_train_sites_sp <- amadeus::process_aqs(
  path = file.path(chr_config_dir, "aqs", "data_files"),
  date = beethoven::fl_dates(chr_init_dates),
  mode = "location",
  data_field = "Sample.Measurement",
  resolution_temporal = "hourly",
  return_format = "sf",
  tz = "gmt"
)

################################################################################
# Import spatiotemporal AQS monitoring locations with monitored data values
# across defined time period.
dt_train_obs_aqs_sptmpl <- amadeus::process_aqs(
  path = file.path(chr_config_dir, "aqs", "data_files"),
  date = beethoven::fl_dates(chr_init_dates),
  mode = "available-data",
  data_field = "Sample.Measurement",
  resolution_temporal = "hourly",
  return_format = "data.table",
  tz = "gmt"
)
dt_train_obs_aqs_sptmpl[,
  time := as.POSIXct(
    time,
    format = "%Y-%m-%d %H:%M",
    tz = "UTC"
  )
]

################################################################################
qs2::qs_save(sf_train_sites_sp, snakemake@output[["sf_train_sites_sp"]])
arrow::write_parquet(
  data.table::data.table(dt_train_obs_aqs_sptmpl),
  snakemake@output[["db_train_obs_aqs_sptmpl"]]
)
