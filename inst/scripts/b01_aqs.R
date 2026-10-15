################################################################################
# Download AQS data (.csv files) for all dates.

################################################################################
# Load libraries and source local functions.
library(amadeus)

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
int_init_years <- qs2::qs_read(snakemake@input[["int_init_years"]])

################################################################################
list_dl_aqs <- amadeus::download_data(
  dataset_name = "aqs",
  directory_to_save = file.path(chr_config_dir, "aqs"),
  year = int_init_years,
  acknowledgement = TRUE,
  hash = FALSE,
  remove_zip = FALSE
)

################################################################################
qs2::qs_save(list_dl_aqs, snakemake@output[["list_dl_aqs"]])
