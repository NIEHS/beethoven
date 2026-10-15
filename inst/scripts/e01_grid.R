################################################################################
# Process Uber H3 hexagon grid locations as an {sf} object.

################################################################################
# Load libraries and source local functions.
library(amadeus)
library(h3jsr)
source("R/amadeus.R")
source("R/beethoven.R")

################################################################################
# Import configured variables and inputs.
int_grid_res <- snakemake@params[["int_grid_res"]]
dt_feat_merge_sptmpl <- qs2::qs_read(snakemake@input[["dt_feat_merge_sptmpl"]])

################################################################################
sf_nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))[, "NAME"]

################################################################################
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
chr_r8ids <- unique(
  unlist(
    h3jsr::polygon_to_cells(sf_geometry, res = int_grid_res),
    use.names = FALSE
  )
)
chr_r8ids <- chr_r8ids[!is.na(chr_r8ids)]

################################################################################
sf_pred_grid_sp <- sf::st_sf(
  h3r8_id = chr_r8ids,
  geometry = h3jsr::cell_to_polygon(chr_r8ids)
)

################################################################################
qs2::qs_save("h3r8_id", snakemake@output[["chr_pred_h3r8"]])
qs2::qs_save(sf_pred_grid_sp, snakemake@output[["sf_pred_grid_sp"]])
