################################################################################
# Download NARR data (.nc files) for all variables and all years.

################################################################################
# Load libraries and source local functions.
library(amadeus)
source("R/amadeus.R")

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
int_init_years <- qs2::qs_read(snakemake@input[["int_init_years"]])

################################################################################
# Define NARR variables of interest to be included.
chr_iter_narr <- c(
  "air.sfc",
  "albedo",
  "apcp",
  "dswrf",
  "evap",
  "hcdc",
  "hpbl",
  "lcdc",
  "lhtfl",
  "mcdc",
  "pr_wtr",
  "prate",
  "pres.sfc",
  "shtfl",
  "snowc",
  "soilm",
  "tcdc",
  "ulwrf.sfc",
  "uwnd.10m",
  "vis",
  "vwnd.10m",
  "weasd",
  "omega",
  "shum"
)

################################################################################
mirai::daemons(50)
list_dl_narr <- download_narr_map(
  variables = chr_iter_narr,
  directory_to_save = file.path(chr_config_dir, "narr"),
  year = int_init_years,
  acknowledgement = TRUE,
  hash = FALSE
)
mirai::daemons(0)

################################################################################
qs2::qs_save(chr_iter_narr, snakemake@output[["chr_iter_narr"]])
qs2::qs_save(list_dl_narr, snakemake@output[["list_dl_narr"]])
