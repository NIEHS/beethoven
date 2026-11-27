################################################################################
# Stream a time-filtered source to temporary Parquet, without collecting the
# full dataset in R. join_covariates() can then retain its existing checks.
write_grid_merge_slice <- function(input, output, start, end, daily = FALSE) {
  chr_view <- basename(tempfile("grid_merge_slice_"))
  on.exit(amadeusdb::cleanup_views(chr_view), add = TRUE)
  sedonadb::sd_to_view(sedonadb::sd_read_parquet(input), chr_view)
  if (daily) {
    # Retain all dates touched by the hourly window, including partial days.
    chr_filter <- sprintf(
      paste0(
        'CAST("date" AS DATE) >= DATE \'%s\' AND ',
        'CAST("date" AS DATE) <= DATE \'%s\''
      ),
      format(start, "%Y-%m-%d", tz = "UTC"),
      format(end - 1, "%Y-%m-%d", tz = "UTC")
    )
  } else {
    chr_filter <- sprintf(
      paste0(
        'CAST("time" AS TIMESTAMP) >= TIMESTAMP \'%s\' AND ',
        'CAST("time" AS TIMESTAMP) < TIMESTAMP \'%s\''
      ),
      format(start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      format(end, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
  }
  sedonadb::sd_write_parquet(
    sedonadb::sd_sql(sprintf(
      'SELECT * FROM "%s" WHERE %s',
      chr_view,
      chr_filter
    )),
    output,
    single_file_output = TRUE
  )
  output
}
