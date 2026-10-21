################################################################################
# Calculate GEOS-CF covariates at unique AQS locations (spatial only).

################################################################################
# Load libraries and source local functions.
library(amadeus)
source("R/sedona.R")

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
sf_feat_aqs_sp <- qs2::qs_read(snakemake@input[["sf_feat_aqs_sp"]])

################################################################################
# Set path to previously downloaded GEOS-CF data.
chr_group_store <- readLines("input/.hidden/chr_group_store.txt")

################################################################################
# Define GEOS-CF variables.
chr_iter_geos_aqc <- c("CO", "NO2", "O3", "SO2")
chr_iter_geos_chm <- c(
  "ACET",
  "ALD2",
  "ALK4",
  "BCPI",
  "BCPO",
  "BENZ",
  "C2H6",
  "C3H8",
  "CH4",
  "CO",
  "DST1",
  "DST2",
  "DST3",
  "DST4",
  "EOH",
  "H2O2",
  "HCHO",
  "HNO3",
  "HNO4",
  "ISOP",
  "MACR",
  "MEK",
  "MVK",
  "N2O5",
  "NH3",
  "NH4",
  "NIT",
  "NO",
  "NO2",
  "NOy",
  "OCPI",
  "OCPO",
  "PAN",
  "PM25_RH35_GCC",
  "PM25_RH35_GOCART",
  "PM25bc_RH35_GCC",
  "PM25du_RH35_GCC",
  "PM25ni_RH35_GCC",
  "PM25oc_RH35_GCC",
  "PM25soa_RH35_GCC",
  "PM25ss_RH35_GCC",
  "PM25su_RH35_GCC",
  "PRPE",
  "RCHO",
  "SALA",
  "SALC",
  "SO2",
  "SOAP",
  "SOAS",
  "TOLU",
  "XYLE"
)

################################################################################
list_feat_geos_sptmpl <- sedona_geos(
  date = beethoven::fl_dates(chr_init_dates),
  path = file.path(chr_group_store, "input/geos/aqc_tavg_1hr_g1440x721_v1/"),
  variable = chr_iter_calc[1],
  locs = sf_feat_aqs_sp,
  locs_id = "site_id"
)

################################################################################
df_feat_geos_sptmpl <- beethoven::reduce_merge(list_feat_geos_sptmpl)

################################################################################
arrow::write_parquet(
  data.table::data.table(df_feat_geos_sptmpl),
  snakemake@output[["df_feat_geos_sptmpl"]]
)
