################################################################################
# Calculate HRRR covariates at unique AQS locations (spatial only).

################################################################################
# Load libraries and source local functions.
library(amadeus)
library(beethoven)

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
sf_train_sites_sp <- qs2::qs_read(snakemake@input[["sf_train_sites_sp"]])

################################################################################
# Define explicit path to GDAL directory.
chr_gdal <- path.expand("~/.conda/envs/beethoven/share/gdal")
Sys.setenv(
  GDAL_DATA = chr_gdal,
  GRIB_RESOURCE_DIR = chr_gdal
)

################################################################################
# Define HRRR variables and iteration list.
df_hrrr_metadata <- amadeus:::metadata_hrrr(accumulation = 1L)
df_iter_f00 <- data.frame(
  forecast_hour = 0L,
  variable = c(
    # Near-surface temperature and humidity.
    "2[m] HTGL=Specified height level above ground; Temperature [C]",
    "2[m] HTGL=Specified height level above ground; Dew point temperature [C]",
    "2[m] HTGL=Specified height level above ground; Relative humidity [%]",
    "2[m] HTGL=Specified height level above ground; Specific humidity [kg/kg]",

    # Wind components at 10 m and 80 m.
    "10[m] HTGL=Specified height level above ground; u-component of wind [m/s]",
    "10[m] HTGL=Specified height level above ground; v-component of wind [m/s]",
    "80[m] HTGL=Specified height level above ground; u-component of wind [m/s]",
    "80[m] HTGL=Specified height level above ground; v-component of wind [m/s]",

    # Boundary layer and turbulent surface exchange.
    "SFC=Ground or water surface; Planetary boundary layer height [m]",
    "SFC=Ground or water surface; Sensible heat net flux [W/(m^2)]",
    "SFC=Ground or water surface; Latent heat net flux [W/(m^2)]",
    "SFC=Ground or water surface; Frictional velocity [m/s]",

    # Surface pressure and skin temperature.
    "SFC=Ground or water surface; Pressure [Pa]",
    "SFC=Ground or water surface; Temperature [C]",

    # Downward surface radiation.
    "SFC=Ground or water surface; Downward short-wave radiation flux [W/(m^2)]",
    "SFC=Ground or water surface; Downward long-wave radiation flux [W/(m^2)]",

    # Atmospheric-column moisture and clouds.
    "EATM=Entire atmosphere (considered as a single layer); Precipitable water [kg/(m^2)]",
    "LCY=Low cloud level; Low cloud cover [%]",
    "EATM=Entire Atmosphere; Total cloud cover [%]"
  )
)
df_iter_f01 <- data.frame(
  forecast_hour = 1L,
  variable = c(
    "SFC=Ground or water surface; 01 hr Total precipitation [kg/(m^2)]"
  )
)
df_iter_hrrr <- rbind(df_iter_f00, df_iter_f01)
stopifnot(all(df_iter_hrrr$variable %in% df_hrrr_metadata$layer_name))
list_iter_hrrr <- split(df_iter_hrrr, seq_len(nrow(df_iter_hrrr)))

################################################################################
# Define all relevant time stamps.
df_times <- expand.grid(
  date = chr_init_dates,
  hour = amadeus:::sequence_times(colon = TRUE)
)
df_times$time <- as.POSIXct(
  paste0(df_times$date, " ", df_times$hour, ":00"),
  tz = "UTC"
)
df_times$tchar <- format(df_times$time, "%Y%m%d%H%M", tz = "UTC")

################################################################################
# Extract HRRR values at AQS locations, stored as variable-time .parquet.
int_ndaemons <- 100L
mirai::daemons(int_ndaemons)
list_train_feat_hrrr_sptmpl <- lapply(
  list_iter_hrrr,
  function(x) {
    chr_parquet <- amadeusdb::batch_prep(
      df = df_times,
      batch = list(seq_len(nrow(df_times))),
      output = "output/train/feat/hrrr/",
      source = "hrrr",
      variable = df_hrrr_metadata$short_name[
        match(x$variable, df_hrrr_metadata$layer_name)
      ],
      time_id = "tchar"
    )
    if (length(chr_parquet) == 1L && file.exists(chr_parquet)) {
      message("Skipping calculation for variable: ", x$variable)
    } else {
      message("Performing calculation for variable: ", x$variable)
      df_x <- amadeus:::calculate_hrrr(
        from = amadeus:::process_hrrr(
          date = as.character(beethoven::fl_dates(chr_init_dates)),
          path = file.path(chr_config_dir, "hrrr", "conus"),
          variable = x$variable,
          forecast_hour = x$forecast_hour,
          accumulation = 1L
        ),
        locs = sf_train_sites_sp,
        locs_id = "site_id",
        radius = 0L
      )
      df_x$level <- NULL
      arrow::write_parquet(df_x, chr_parquet)
    }
    chr_parquet
  }
)
mirai::daemons(0)

################################################################################
# Join HRRR variable data manually written to .parquet files.
db_train_feat_hrrr_sptmpl <- amadeusdb::join_variables(
  input = list_train_feat_hrrr_sptmpl,
  output = "output/train/feat/hrrr/join",
  by = c("site_id", "time")
)

################################################################################
qs2::qs_save(
  db_train_feat_hrrr_sptmpl,
  snakemake@output[["db_train_feat_hrrr_sptmpl"]]
)
