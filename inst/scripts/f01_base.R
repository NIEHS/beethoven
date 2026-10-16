################################################################################
# Predict base learners at Uber H3 hexagon grid locations.

################################################################################
# Load libraries and source local functions.
library(beethoven)

################################################################################
# Import configured variables and inputs.
list_base_fit_elnet <- qs2::qs_read(snakemake@input[["list_base_fit_elnet"]])
dt_pred_merge_sptmpl <- qs2::qs_read(snakemake@input[["dt_pred_merge_sptmpl"]])

################################################################################
# Post-append latitude and longitude coordinates to H3 grid cells.
dt_pred_xy_sptmpl <- sf::st_coordinates(
  h3jsr::cell_to_point(dt_pred_merge_sptmpl$site_id)
)
dt_pred_merge_sptmpl$lon <- dt_pred_xy_sptmpl[, "X"]
dt_pred_merge_sptmpl$lat <- dt_pred_xy_sptmpl[, "Y"]
names(dt_pred_merge_sptmpl) <- gsub(
  "h3r8_id",
  "site_id",
  names(dt_pred_merge_sptmpl)
)

################################################################################
# Predict at H3 sites, collect predictions, and set unique name.
dt_pred_base_sptmpl <- do.call(
  dplyr::bind_cols,
  lapply(
    list_base_fit_elnet,
    function(x) {
      stats::predict(x$workflow, dt_pred_merge_sptmpl)[, ".pred"]
    }
  )
)
names(dt_pred_base_sptmpl) <- sprintf(
  "pred_%03d",
  seq_along(names(dt_pred_base_sptmpl))
)

################################################################################
# Append to spatiotemporal identifiers (site_id, lon, lat, time).
dt_pred_base_sptmpl <- dplyr::bind_cols(
  dt_pred_merge_sptmpl[, c("site_id", "time", "lon", "lat")],
  dt_pred_base_sptmpl
)

################################################################################
qs2::qs_save(dt_pred_base_sptmpl, snakemake@output[["dt_pred_base_sptmpl"]])
