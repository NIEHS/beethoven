################################################################################
# Calculate GEOS covariates at Uber H3 locations (spatial only).

################################################################################
# Load libraries and source local functions.
library(amadeusdb)

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
sf_grid_sites_sp <- qs2::qs_read(snakemake@input[["sf_grid_sites_sp"]])

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
list_grid_feat_geos_sptmpl <- lapply(
  list_iter_geos,
  function(x) {
    amadeusdb::sedona_geos(
      date = as.character(beethoven::fl_dates(chr_init_dates)),
      input = file.path(chr_config_dir, "geos", x$asset),
      output = "output/grid/feat/geos/",
      variable = x$variable,
      locs = sf_grid_sites_sp,
      locs_id = "site_id",
      batch = 240L,
      radius = 0L
    )
  }
)
names(list_grid_feat_geos_sptmpl) <- df_iter_geos$variable

################################################################################
# Index each variable's files by the time range of its 240-hour batch.
list_grid_feat_geos_batches_sptmpl <- lapply(
  list_grid_feat_geos_sptmpl,
  function(x) {
    chr_batch <- sub(
      "^.*_([0-9]{12}_[0-9]{12})\\.parquet$",
      "\\1",
      basename(x)
    )
    stopifnot(
      length(x) > 0L,
      all(grepl("^[0-9]{12}_[0-9]{12}$", chr_batch)),
      !anyDuplicated(chr_batch)
    )
    stats::setNames(x, chr_batch)
  }
)
chr_grid_feat_geos_batches <- sort(names(list_grid_feat_geos_batches_sptmpl[[1L]]))
stopifnot(all(vapply(
  list_grid_feat_geos_batches_sptmpl,
  function(x) setequal(names(x), chr_grid_feat_geos_batches),
  FUN.VALUE = logical(1)
)))

################################################################################
# Join and write one time batch at a time; all output files form one dataset.
db_grid_feat_geos_sptmpl <- unlist(
  lapply(
    chr_grid_feat_geos_batches,
    function(x) {
      message("Joining GEOS grid batch ", x, "...")
      chr_parquet <- amadeusdb::join_variables(
        input = lapply(
          list_grid_feat_geos_batches_sptmpl,
          function(y) unname(y[[x]])
        ),
        output = file.path("output/grid/feat/geos/join", x),
        by = c("site_id", "time", "level")
      )
      gc(verbose = FALSE)
      chr_parquet
    }
  ),
  use.names = FALSE
)

################################################################################
qs2::qs_save(
  db_grid_feat_geos_sptmpl,
  snakemake@output[["db_grid_feat_geos_sptmpl"]]
)
