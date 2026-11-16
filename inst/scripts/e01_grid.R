################################################################################
# Process Uber H3 hexagon grid locations as an {sf} object.

################################################################################
# Load libraries and source local functions.
library(h3jsr)

################################################################################
# Import configured variables and inputs.
int_grid_res <- snakemake@params[["int_grid_res"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
db_feat_merge_sptmpl <- qs2::qs_read(snakemake@input[["db_feat_merge_sptmpl"]])

################################################################################
# Import fixture boundary.
sf_nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))[, "NAME"]

################################################################################
# Union boundaries.
sf_geometry <- sf::st_sf(
  geometry = sf::st_union(
    sf::st_geometry(
      sf::st_make_valid(
        sf::st_transform(sf_nc, "EPSG:4326")
      )
    )
  )
)

################################################################################
# Generate H3 grid from unioned boundary.
chr_gridid <- unique(
  unlist(
    h3jsr::polygon_to_cells(sf_geometry, res = int_grid_res),
    use.names = FALSE
  )
)
chr_gridid <- chr_gridid[!is.na(chr_gridid)]

################################################################################
# Convert H3 grid to `sf` polygons.
sf_pred_grid_sp <- sf::st_sf(
  site_id = chr_gridid,
  geometry = h3jsr::cell_to_polygon(chr_gridid)
)

################################################################################
# Sites with latitude and longitude.
df_pred_grid_sp <- sf::st_drop_geometry(sf_pred_grid_sp)
df_pred_xy_sp <- sf::st_coordinates(
  h3jsr::cell_to_point(df_pred_grid_sp$site_id)
)
df_pred_grid_sp$lon <- df_pred_xy_sp[, "X"]
df_pred_grid_sp$lat <- df_pred_xy_sp[, "Y"]
dt_pred_grid_sp <- data.table::data.table(df_pred_grid_sp)

################################################################################
# Expand all dates and hours (temporal).
df_pred_grid_tmpl <- expand.grid(
  date = chr_init_dates,
  hour = amadeus:::sequence_times(colon = TRUE)
)
chr_pred_grid_tmpl <- sprintf(
  "%s %s:00",
  df_pred_grid_tmpl$date,
  df_pred_grid_tmpl$hour
)

################################################################################
# Expand all sites and times (spatiotemporal).
dt_pred_grid_sptmpl <- data.table::data.table(
  expand.grid(
    site_id = dt_pred_grid_sp$site_id,
    time = as.POSIXct(chr_pred_grid_tmpl, tz = "UTC")
  )
)
dt_pred_grid_sptmpl2 <- data.table::merge.data.table(
  dt_pred_grid_sptmpl,
  dt_pred_grid_sp,
  by = "site_id"
)[, c("site_id", "lon", "lat", "time")]

################################################################################
qs2::qs_save(sf_pred_grid_sp, snakemake@output[["sf_pred_grid_sp"]])
arrow::write_parquet(
  dt_pred_grid_sptmpl2,
  snakemake@output[["db_pred_grid_sptmpl"]]
)
