################################################################################
# Process Uber H3 hexagon grid locations as an {sf} object.

################################################################################
# Load libraries and source local functions.
library(h3jsr)

################################################################################
# Import configured variables and inputs.
int_grid_res <- snakemake@params[["int_grid_res"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
list_meta_xgb_model <- qs2::qs_read(snakemake@input[["list_meta_xgb_model"]])

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
chr_grid_sites_id <- unique(
  unlist(
    h3jsr::polygon_to_cells(sf_geometry, res = int_grid_res),
    use.names = FALSE
  )
)
chr_grid_sites_id <- chr_grid_sites_id[!is.na(chr_grid_sites_id)]

################################################################################
# Convert H3 grid to `sf` polygons.
sf_grid_sites_sp <- sf::st_sf(
  site_id = chr_grid_sites_id,
  geometry = h3jsr::cell_to_polygon(chr_grid_sites_id)
)

################################################################################
# Sites with latitude and longitude.
df_grid_sites_sp <- sf::st_drop_geometry(sf_grid_sites_sp)
mat_grid_sites_xy_sp <- sf::st_coordinates(
  h3jsr::cell_to_point(df_grid_sites_sp$site_id)
)
df_grid_sites_sp$lon <- mat_grid_sites_xy_sp[, "X"]
df_grid_sites_sp$lat <- mat_grid_sites_xy_sp[, "Y"]
dt_grid_sites_sp <- data.table::data.table(df_grid_sites_sp)

################################################################################
# Expand all dates and hours (temporal).
df_grid_index_tmpl <- expand.grid(
  date = chr_init_dates,
  hour = amadeus:::sequence_times(colon = TRUE)
)
chr_grid_index_tmpl <- sprintf(
  "%s %s:00",
  df_grid_index_tmpl$date,
  df_grid_index_tmpl$hour
)

################################################################################
# Expand all sites and times (spatiotemporal).
dt_grid_index_keys_sptmpl <- data.table::data.table(
  expand.grid(
    site_id = dt_grid_sites_sp$site_id,
    time = as.POSIXct(chr_grid_index_tmpl, tz = "UTC")
  )
)
dt_grid_index_sptmpl <- data.table::merge.data.table(
  dt_grid_index_keys_sptmpl,
  dt_grid_sites_sp,
  by = "site_id"
)[, c("site_id", "lon", "lat", "time")]

################################################################################
qs2::qs_save(sf_grid_sites_sp, snakemake@output[["sf_grid_sites_sp"]])
arrow::write_parquet(
  dt_grid_index_sptmpl,
  snakemake@output[["db_grid_index_sptmpl"]]
)
