################################################################################
# Calculate GEOS covariates at unique AQS locations (spatial only).

################################################################################
# Load libraries and source local functions.
library(amadeusdb)

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
sf_train_sites_sp <- qs2::qs_read(snakemake@input[["sf_train_sites_sp"]])

################################################################################
# Dynamic load of GDAL library to be used by SedonaDB.
dyn.load(
  file.path(Sys.getenv("HOME"), ".conda/envs/beethoven/lib/libgdal.so.39"),
  local = FALSE,
  now = TRUE
)

################################################################################
# Define GEOS variables and iteration list.
# Retain 10 PM2.5 predictors: total mass, seven components, CO, and NO2.
# Other candidates remain commented for later evaluation.
df_iter_aqc <- data.frame(
  asset = "aqc_tavg_1hr_g1440x721_v1",
  variable = c(
    "CO",
    "NO2"
    # "O3",
    # "SO2"
  )
)
df_iter_chm <- data.frame(
  asset = "chm_tavg_1hr_g1440x721_v1",
  variable = c(
    # "ACET",
    # "ALD2",
    # "ALK4",
    # "BCPI",
    # "BCPO",
    # "BENZ",
    # "C2H6",
    # "C3H8",
    # "CH4",
    # "DST1",
    # "DST2",
    # "DST3",
    # "DST4",
    # "EOH",
    # "H2O2",
    # "HCHO",
    # "HNO3",
    # "HNO4",
    # "ISOP",
    # "MACR",
    # "MEK",
    # "MVK",
    # "N2O5",
    # "NH3",
    # "NH4",
    # "NIT",
    # "NO",
    # "NOy",
    # "OCPI",
    # "OCPO",
    # "PAN",
    "PM25_RH35_GCC",
    # "PM25_RH35_GOCART",
    "PM25bc_RH35_GCC",
    "PM25du_RH35_GCC",
    "PM25ni_RH35_GCC",
    "PM25oc_RH35_GCC",
    "PM25soa_RH35_GCC",
    "PM25ss_RH35_GCC",
    "PM25su_RH35_GCC"
    # "PRPE",
    # "RCHO",
    # "SALA",
    # "SALC",
    # "SOAP",
    # "SOAS",
    # "TOLU",
    # "XYLE"
  )
)
df_iter_geos <- rbind(df_iter_aqc, df_iter_chm)
list_iter_geos <- split(df_iter_geos, seq_len(nrow(df_iter_geos)))

################################################################################
# Extract GEOS values at AQS locations, stored as variable-time .parquet.
list_train_feat_geos_sptmpl <- lapply(
  list_iter_geos,
  function(x) {
    amadeusdb::sedona_geos(
      date = as.character(beethoven::fl_dates(chr_init_dates)),
      input = file.path(chr_config_dir, "geos", x$asset),
      output = "output/train/feat/geos/",
      variable = x$variable,
      locs = sf_train_sites_sp,
      locs_id = "site_id",
      batch = 240L,
      radius = 0L
    )
  }
)
names(list_train_feat_geos_sptmpl) <- df_iter_geos$variable

################################################################################
# Group at most 10 variables per join to limit SQL nesting.
list_train_feat_geos_groups_sptmpl <- split(
  list_train_feat_geos_sptmpl,
  ceiling(seq_along(list_train_feat_geos_sptmpl) / 10L)
)
chr_train_feat_geos_tmp_sptmpl <- tempfile(
  pattern = "geos_train_join_",
  tmpdir = tempdir()
)

################################################################################
# Write group joins to temporary storage.
list_train_feat_geos_joined_sptmpl <- lapply(
  seq_along(list_train_feat_geos_groups_sptmpl),
  function(i) {
    amadeusdb::join_variables(
      input = list_train_feat_geos_groups_sptmpl[[i]],
      output = file.path(
        chr_train_feat_geos_tmp_sptmpl,
        sprintf("batch_%02d", i)
      ),
      by = c("site_id", "time", "level")
    )
  }
)

################################################################################
# Persist the final GEOS dataset, then remove intermediate files.
db_train_feat_geos_sptmpl <- amadeusdb::join_variables(
  input = list_train_feat_geos_joined_sptmpl,
  output = "output/train/feat/geos/join",
  by = c("site_id", "time", "level")
)
unlink(chr_train_feat_geos_tmp_sptmpl, recursive = TRUE)

################################################################################
qs2::qs_save(
  db_train_feat_geos_sptmpl,
  snakemake@output[["db_train_feat_geos_sptmpl"]]
)
