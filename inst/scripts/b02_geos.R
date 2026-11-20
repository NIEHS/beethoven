################################################################################
# Download GEOS data (.nc4 files) for all variables and all years.

################################################################################
# Load libraries and source local functions.
library(amadeus)

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])

################################################################################
# Define GEOS collections of interest to be included.
chr_iter_geos <- c("aqc_tavg_1hr_g1440x721_v1", "chm_tavg_1hr_g1440x721_v1")

################################################################################
# Import NASA EarthData Token.
chr_nasa_earthdata_token <- readLines(
  "input/.hidden/chr_nasa_earthdata_token.txt"
)

################################################################################
int_ndaemons <- 33L
mirai::daemons(int_ndaemons)
list_dl_geos <- amadeus::download_geos(
  collection = chr_iter_geos,
  nasa_earth_data_token = chr_nasa_earthdata_token,
  directory_to_save = file.path(chr_config_dir, "geos"),
  date = as.character(beethoven::fl_dates(chr_init_dates)),
  acknowledgement = TRUE,
  hash = FALSE,
  rate_limit = 2 / int_ndaemons
)
mirai::daemons(0)

################################################################################
qs2::qs_save(list_dl_geos, snakemake@output[["list_dl_geos"]])
