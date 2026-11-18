################################################################################
# Evaluate OOF meta predictions against observed training PM2.5.

################################################################################
# Import configured variables and inputs.
db_train_pred_meta_oof_sptmpl <- qs2::qs_read(
  "output/train/pred/meta/oof/db_train_pred_meta_oof_sptmpl.qs"
)

################################################################################
# Plot settings.
chr_plot_dir <- "output/plots"
# UTC date and time to the minute.
chr_plot_timestamp <- format(Sys.time(), "%Y%m%dT%H%MZ", tz = "UTC")
dir.create(chr_plot_dir, recursive = TRUE, showWarnings = FALSE)
chr_meta_names <- sprintf("meta_%03d", seq_len(10))
int_plot_stride <- 97L
int_plot_n <- 5000L
int_plot_nn <- 30L
chr_map_jet <- grDevices::colorRampPalette(
  c("#000080", "#0000FF", "#00FFFF", "#FFFF00", "#FF0000", "#800000")
)(256)

################################################################################
# 1. OOF predictions versus observed training PM2.5, one panel per meta learner.
# The OOF dataset already retains the matched outcome as Sample_Measurement.
# Summarize all rows in SedonaDB before collecting the small evaluation table.
sedonadb::sd_read_parquet(db_train_pred_meta_oof_sptmpl) |>
  sedonadb::sd_to_view("plot_train_meta", overwrite = TRUE)
chr_train_metrics_sql <- vapply(
  chr_meta_names,
  function(model) {
    sprintf(
      paste0(
        'COUNT(%1$s) AS %1$s_n, ',
        'AVG((%1$s - "Sample_Measurement") * ',
        '(%1$s - "Sample_Measurement")) AS %1$s_mse, ',
        'AVG(ABS(%1$s - "Sample_Measurement")) AS %1$s_mae, ',
        'AVG(%1$s - "Sample_Measurement") AS %1$s_bias'
      ),
      model
    )
  },
  character(1)
)
df_train_meta_metrics <- sedonadb::sd_sql(paste0(
  "SELECT MIN(time) AS time_start, MAX(time) AS time_end, ",
  paste(chr_train_metrics_sql, collapse = ", "),
  ' FROM plot_train_meta WHERE "Sample_Measurement" IS NOT NULL'
)) |>
  sedonadb::sd_collect()
df_train_meta_eval <- dplyr::bind_rows(lapply(chr_meta_names, function(model) {
  data.frame(
    learner = model,
    n = df_train_meta_metrics[[paste0(model, "_n")]],
    rmse = sqrt(df_train_meta_metrics[[paste0(model, "_mse")]]),
    mae = df_train_meta_metrics[[paste0(model, "_mae")]],
    bias = df_train_meta_metrics[[paste0(model, "_bias")]]
  )
}))

################################################################################
# Use the same reproducible site-hour sample for all 10 scatter panels.
# First select every 97th hour, with a different offset per site, across the full
# period; then randomly sample up to int_plot_n rows for plotting.
# Spark Parquet timestamps use microseconds; 3,600,000,000 converts to hours.
# This limits plotting/nearest-neighbour work; the metrics above use all OOF rows.
chr_train_sample_sql <- paste0(
  "SELECT site_id, time, ",
  '"Sample_Measurement" AS actual, ',
  paste(chr_meta_names, collapse = ", "),
  " FROM plot_train_meta WHERE ",
  "(CAST(site_id AS BIGINT) + ",
  "CAST(CAST(time AS BIGINT) / 3600000000 AS BIGINT)) % ",
  int_plot_stride,
  " = 0 ORDER BY time, site_id"
)
df_train_pred_meta_oof_sample_sptmpl <- sedonadb::sd_sql(
  chr_train_sample_sql
) |>
  sedonadb::sd_collect()
set.seed(20261117)
df_train_pred_meta_oof_sample_sptmpl <- df_train_pred_meta_oof_sample_sptmpl |>
  dplyr::slice_sample(
    n = min(int_plot_n, nrow(df_train_pred_meta_oof_sample_sptmpl))
  ) |>
  dplyr::arrange(time, site_id)

################################################################################
# Two-dimensional k-nearest-neighbour density in observed/predicted coordinates.
# Both axes use ug/m3. Density is calculated separately within each learner.
df_train_pred_meta_density_sptmpl <- dplyr::bind_rows(lapply(
  chr_meta_names,
  function(model) {
    df_model <- df_train_pred_meta_oof_sample_sptmpl |>
      dplyr::transmute(
        site_id,
        time,
        actual,
        prediction = .data[[model]],
        learner = model
      ) |>
      dplyr::filter(is.finite(actual), is.finite(prediction))
    num_nn_radius <- FNN::get.knn(
      as.matrix(df_model[, c("actual", "prediction")]),
      k = int_plot_nn
    )$nn.dist[, int_plot_nn]
    # A positive distance floor keeps coincident pairs' density finite.
    df_model$density <- int_plot_nn /
      (nrow(df_model) * pi * pmax(num_nn_radius, sqrt(.Machine$double.eps))^2)
    df_model
  }
)) |>
  dplyr::arrange(time, site_id, learner)
chr_meta_labels <- stats::setNames(
  sprintf(
    "%s\nRMSE %.2f | MAE %.2f",
    gsub("meta_", "Meta: ", df_train_meta_eval$learner),
    df_train_meta_eval$rmse,
    df_train_meta_eval$mae
  ),
  df_train_meta_eval$learner
)
num_train_plot_limits <- range(
  df_train_pred_meta_density_sptmpl$actual,
  df_train_pred_meta_density_sptmpl$prediction
)

################################################################################
plot_meta_observed <- ggplot2::ggplot(
  df_train_pred_meta_density_sptmpl,
  ggplot2::aes(x = actual, y = prediction, colour = density)
) +
  ggplot2::geom_point(size = 0.25, alpha = 0.7) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::facet_wrap(
    ggplot2::vars(learner),
    nrow = 5,
    ncol = 2,
    labeller = ggplot2::labeller(learner = chr_meta_labels)
  ) +
  ggplot2::scale_colour_gradientn(
    colours = chr_map_jet,
    transform = "log10",
    name = "NN density"
  ) +
  ggplot2::coord_equal(
    xlim = num_train_plot_limits,
    ylim = num_train_plot_limits
  ) +
  ggplot2::labs(
    title = "Out-of-fold meta predictions versus observed PM2.5",
    x = expression(Observed ~ PM[2.5] ~ (mu * g / m^3)),
    y = expression(Predicted ~ PM[2.5] ~ (mu * g / m^3)),
    caption = sprintf(
      paste0(
        "Points: up to %s sampled site-hours per learner.\nMetrics: all OOF pairs.\n"
      ),
      format(nrow(df_train_pred_meta_oof_sample_sptmpl), big.mark = ",")
    )
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 8))
ggplot2::ggsave(
  filename = file.path(
    chr_plot_dir,
    paste0("g01_train_meta_oof_observed_", chr_plot_timestamp, ".png")
  ),
  plot = plot_meta_observed,
  device = ragg::agg_png,
  width = 12,
  height = 15,
  units = "in",
  dpi = 150,
  bg = "white"
)
