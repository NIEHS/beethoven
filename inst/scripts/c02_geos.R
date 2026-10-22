################################################################################
# Calculate GEOS-CF covariates at unique AQS locations (spatial only).

################################################################################
# Load libraries and source local functions.
library(amadeus)
library(sedonadb)
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
# Dynamic load of GDAL library to be used by SedonaDB.
dyn.load(
  file.path(Sys.getenv("HOME"), ".conda/envs/beethoven/lib/libgdal.so.39"),
  local = FALSE,
  now = TRUE
)

################################################################################
# Define GEOS-CF variables and iteration list.
df_iter_aqc <- data.frame(
  asset = "aqc_tavg_1hr_g1440x721_v1",
  variable = c("CO", "NO2", "O3", "SO2")
)
df_iter_chm <- data.frame(
  asset = "chm_tavg_1hr_g1440x721_v1",
  variable = c(
    "ACET",
    "ALD2",
    "ALK4",
    "BCPI",
    "BCPO",
    "BENZ",
    "C2H6",
    "C3H8",
    "CH4",
    # "CO",
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
    # "NO2",
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
    # "SO2",
    "SOAP",
    "SOAS",
    "TOLU",
    "XYLE"
  )
)
df_iter_geos <- rbind(df_iter_aqc, df_iter_chm)
list_iter_geos <- split(df_iter_geos, seq_len(nrow(df_iter_geos)))

################################################################################
source("R/sedona.R")
list_feat_geos_sptmpl <- lapply(
  list_iter_geos[1:2],
  function(x) {
    sedona_geos(
      date = beethoven::fl_dates(chr_init_dates),
      input = file.path(
        chr_group_store,
        "input/geos/",
        x$asset,
        "/"
      ),
      output = "output/geos/",
      variable = x$variable,
      locs = sf_feat_aqs_sp,
      locs_id = "site_id",
      batch = 240L
    )
  }
)

################################################################################
chr_feat_geos_sptmpl <- unname(do.call(c, list_feat_geos_sptmpl))

################################################################################
qs2::qs_save(
  chr_feat_geos_sptmpl,
  snakemake@output[["chr_feat_geos_sptmpl"]]
)
