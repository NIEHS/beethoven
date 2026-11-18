################################################################################
# Map time-averaged grid predictions for the ensemble and each meta learner.

################################################################################
# Import configured variables and inputs.
sf_grid_sites_sp <- qs2::qs_read("output/grid/sites/sf_grid_sites_sp.qs") |>
  sf::st_as_sf()
db_grid_pred_meta_mean_sptmpl <- qs2::qs_read(
  "output/grid/pred/meta/mean/db_grid_pred_meta_mean_sptmpl.qs"
)

################################################################################
# Plot settings.
chr_plot_dir <- "output/plots"
# Both maps share the UTC date and time to the minute for this script run.
chr_plot_timestamp <- format(Sys.time(), "%Y%m%dT%H%MZ", tz = "UTC")
dir.create(chr_plot_dir, recursive = TRUE, showWarnings = FALSE)
chr_meta_names <- sprintf("meta_%03d", seq_len(10))
chr_map_jet <- grDevices::colorRampPalette(
  c("#000080", "#0000FF", "#00FFFF", "#FFFF00", "#FF0000", "#800000")
)(256)

################################################################################
# 2. Time-averaged grid predictions, aggregated in SedonaDB rather than in R.
# Collect one row per site with each learner's mean across all available hours.
sedonadb::sd_read_parquet(db_grid_pred_meta_mean_sptmpl) |>
  sedonadb::sd_to_view("plot_grid_meta", overwrite = TRUE)
df_grid_pred_meta_mean_sp <- sedonadb::sd_sql(paste0(
  "SELECT site_id, MIN(time) AS time_start, MAX(time) AS time_end, ",
  paste(
    sprintf("AVG(%s) AS %s", chr_meta_names, chr_meta_names),
    collapse = ", "
  ),
  " FROM plot_grid_meta GROUP BY site_id ORDER BY site_id"
)) |>
  sedonadb::sd_collect()
df_grid_pred_meta_mean_sp$mean <- rowMeans(
  df_grid_pred_meta_mean_sp[, chr_meta_names]
)
sf_grid_pred_meta_mean_sp <- sf_grid_sites_sp |>
  dplyr::left_join(df_grid_pred_meta_mean_sp, by = "site_id")
sf_grid_pred_meta_members_sp <- sf_grid_sites_sp |>
  dplyr::left_join(
    df_grid_pred_meta_mean_sp |>
      tidyr::pivot_longer(
        cols = tidyselect::all_of(chr_meta_names),
        names_to = "learner",
        values_to = "prediction"
      ),
    by = "site_id"
  )
chr_grid_period <- sprintf(
  "%s to %s UTC",
  format(min(df_grid_pred_meta_mean_sp$time_start), "%Y-%m-%d", tz = "UTC"),
  format(max(df_grid_pred_meta_mean_sp$time_end), "%Y-%m-%d", tz = "UTC")
)
num_grid_plot_limits <- range(
  as.matrix(df_grid_pred_meta_mean_sp[, chr_meta_names]),
  na.rm = TRUE
)

################################################################################
# Choose three reproducible random cells with predictions; label them on the map.
set.seed(20261117)
df_grid_plot_sites_sp <- data.frame(
  site_id = sample(df_grid_pred_meta_mean_sp$site_id, size = 3),
  label = c("A", "B", "C")
)
sf_grid_plot_sites_sp <- sf::st_sf(
  df_grid_plot_sites_sp,
  geometry = h3jsr::cell_to_point(df_grid_plot_sites_sp$site_id)
)

################################################################################
plot_grid_meta_mean <- ggplot2::ggplot(sf_grid_pred_meta_mean_sp) +
  ggplot2::geom_sf(ggplot2::aes(fill = mean), colour = NA) +
  ggplot2::geom_sf(
    data = sf_grid_plot_sites_sp,
    shape = 21,
    fill = "white",
    size = 2
  ) +
  ggplot2::geom_sf_text(
    data = sf_grid_plot_sites_sp,
    ggplot2::aes(label = label),
    fun.geometry = identity,
    nudge_y = 0.12,
    fontface = "bold",
    size = 4
  ) +
  ggplot2::scale_fill_gradientn(
    colours = chr_map_jet,
    limits = num_grid_plot_limits,
    name = expression(PM[2.5] ~ (mu * g / m^3))
  ) +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(
    title = "Mean Ensemble PM2.5 (all learners, all hours)"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
ggplot2::ggsave(
  filename = file.path(
    chr_plot_dir,
    paste0("g02_grid_meta_mean_", chr_plot_timestamp, ".png")
  ),
  plot = plot_grid_meta_mean,
  device = ragg::agg_png,
  width = 12,
  height = 6,
  units = "in",
  dpi = 150,
  bg = "white"
)

################################################################################
plot_grid_meta_members <- ggplot2::ggplot(sf_grid_pred_meta_members_sp) +
  ggplot2::geom_sf(ggplot2::aes(fill = prediction), colour = NA) +
  ggplot2::facet_wrap(ggplot2::vars(learner), nrow = 5, ncol = 2) +
  ggplot2::scale_fill_gradientn(
    colours = chr_map_jet,
    limits = num_grid_plot_limits,
    name = expression(PM[2.5] ~ (mu * g / m^3))
  ) +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(
    title = "Mean PM2.5 by learner (all hours)"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
ggplot2::ggsave(
  filename = file.path(
    chr_plot_dir,
    paste0("g02_grid_meta_members_", chr_plot_timestamp, ".png")
  ),
  plot = plot_grid_meta_members,
  device = ragg::agg_png,
  width = 12,
  height = 12,
  units = "in",
  dpi = 150,
  bg = "white"
)
