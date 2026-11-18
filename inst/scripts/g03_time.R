################################################################################
# Plot hourly ensemble predictions at three reproducible random H3 locations.

################################################################################
# Import configured variables and inputs.
db_grid_pred_meta_mean_sptmpl <- qs2::qs_read(
  "output/grid/pred/meta/mean/db_grid_pred_meta_mean_sptmpl.qs"
)

################################################################################
# Plot settings.
chr_plot_dir <- "output/plots"
# UTC date and time to the minute.
chr_plot_timestamp <- format(Sys.time(), "%Y%m%dT%H%MZ", tz = "UTC")
dir.create(chr_plot_dir, recursive = TRUE, showWarnings = FALSE)
chr_meta_names <- sprintf("meta_%03d", seq_len(10))
chr_map_jet <- grDevices::colorRampPalette(
  c("#000080", "#0000FF", "#00FFFF", "#FFFF00", "#FF0000", "#800000")
)(256)

################################################################################
# Use the same ordered list of prediction sites and seed as g02_map.R.
sedonadb::sd_read_parquet(db_grid_pred_meta_mean_sptmpl) |>
  sedonadb::sd_to_view("plot_grid_meta", overwrite = TRUE)
df_grid_pred_meta_period_sp <- sedonadb::sd_sql(paste0(
  "SELECT site_id, MIN(time) AS time_start, MAX(time) AS time_end ",
  "FROM plot_grid_meta GROUP BY site_id ORDER BY site_id"
)) |>
  sedonadb::sd_collect()
chr_grid_period <- sprintf(
  "%s to %s UTC",
  format(min(df_grid_pred_meta_period_sp$time_start), "%Y-%m-%d", tz = "UTC"),
  format(max(df_grid_pred_meta_period_sp$time_end), "%Y-%m-%d", tz = "UTC")
)
set.seed(20261117)
df_grid_plot_sites_sp <- data.frame(
  site_id = sample(df_grid_pred_meta_period_sp$site_id, size = 3),
  label = c("A", "B", "C")
)

################################################################################
# 3. Hourly ensemble means at the three selected H3 cells, ordered by time in UTC.
# Read only those cells and preserve the complete hourly series, without sampling.
df_grid_pred_meta_selected_sptmpl <- sedonadb::sd_sql(paste0(
  "SELECT site_id, time, (",
  paste(chr_meta_names, collapse = " + "),
  ") / ",
  length(chr_meta_names),
  " AS mean FROM plot_grid_meta ",
  "WHERE site_id IN ('",
  paste(df_grid_plot_sites_sp$site_id, collapse = "', '"),
  "') ORDER BY time, site_id"
)) |>
  sedonadb::sd_collect() |>
  dplyr::left_join(df_grid_plot_sites_sp, by = "site_id") |>
  dplyr::arrange(site_id, time) |>
  dplyr::group_by(site_id) |>
  dplyr::mutate(
    segment = cumsum(c(TRUE, diff(as.numeric(time)) > 3600))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    site_label = factor(
      paste(label, site_id, sep = ": "),
      levels = paste(
        df_grid_plot_sites_sp$label,
        df_grid_plot_sites_sp$site_id,
        sep = ": "
      )
    )
  ) |>
  dplyr::arrange(time, site_label)

################################################################################
plot_grid_meta_timeseries <- ggplot2::ggplot(
  df_grid_pred_meta_selected_sptmpl,
  ggplot2::aes(
    x = time,
    y = mean,
    colour = mean,
    group = interaction(site_id, segment)
  )
) +
  ggplot2::geom_line(linewidth = 0.25) +
  ggplot2::facet_wrap(ggplot2::vars(site_label), ncol = 1) +
  ggplot2::scale_colour_gradientn(
    colours = chr_map_jet,
    name = expression(PM[2.5] ~ (mu * g / m^3))
  ) +
  ggplot2::scale_x_datetime(
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    timezone = "UTC"
  ) +
  ggplot2::labs(
    title = "Hourly predicted PM2.5 at three random H3 locations",
    x = "Time (UTC)",
    y = expression(PM[2.5] ~ (mu * g / m^3)),
    caption = "Each value is the mean of 10 meta predictions; missing hours break the line."
  ) +
  ggplot2::theme_minimal()
ggplot2::ggsave(
  filename = file.path(
    chr_plot_dir,
    paste0("g03_grid_meta_timeseries_", chr_plot_timestamp, ".png")
  ),
  plot = plot_grid_meta_timeseries,
  device = ragg::agg_png,
  width = 12,
  height = 8,
  units = "in",
  dpi = 150,
  bg = "white"
)
