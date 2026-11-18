#!/usr/bin/env Rscript

# Build the shared Parquet input for the hourly PM2.5 XGBoost benchmark.
#
# Environment variables:
#   PM25_INPUT_YEARS      Comma-separated input years (default: 2024,2025)
#   PM25_INPUT_<YEAR>     CSV for a selected year (default: hourly_88101_<YEAR>.csv)
#   PM25_ANALYSIS_PATH    Output Parquet file
#   PM25_MANIFEST_PATH    Output key/value manifest CSV
#   PM25_FOLDS            Number of site-blocked folds (default: 5)
#   PM25_MAX_SITES        Deterministic site limit for smoke tests; 0 uses all
#   PM25_DUCKDB_THREADS   DuckDB threads (default: 16)

Sys.setenv(
  DUCKPLYR_FALLBACK_COLLECT = "0",
  DUCKPLYR_TEMP_DIR = file.path(tempdir(), "duckplyr")
)

required_packages <- c("DBI", "duckdb")
missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]
if (length(missing_packages) > 0L) {
  stop(
    "Install the required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

env_integer <- function(name, default, minimum = 0L) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    return(as.integer(default))
  }
  value <- suppressWarnings(as.integer(value))
  if (is.na(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, call. = FALSE)
  }
  value
}

env_years <- function(name, default) {
  value <- Sys.getenv(name, unset = default)
  years <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  years <- years[nzchar(years)]
  if (!length(years) || any(!grepl("^[0-9]{4}$", years))) {
    stop(
      name,
      " must be a comma-separated list of four-digit years.",
      call. = FALSE
    )
  }
  if (anyDuplicated(years)) {
    stop(name, " must not contain duplicate years.", call. = FALSE)
  }
  years
}

sql_string <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

atomic_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(x, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    if (!file.copy(temporary, path, overwrite = TRUE)) {
      stop("Could not write ", path, call. = FALSE)
    }
    unlink(temporary)
  }
}

workspace_dir <- normalizePath(
  "/ddn/gs1/home/messierkp/practice/spark",
  mustWork = TRUE
)
input_years <- env_years("PM25_INPUT_YEARS", "2024,2025")
input_year_numbers <- as.integer(input_years)
trend_reference_start <- as.Date(sprintf("%04d-01-01", min(input_year_numbers)))
trend_reference_end <- as.Date(sprintf(
  "%04d-01-01",
  max(input_year_numbers) + 1L
))
trend_span_days <- as.numeric(trend_reference_end - trend_reference_start)
input_paths <- vapply(
  input_years,
  function(year) {
    Sys.getenv(
      paste0("PM25_INPUT_", year),
      unset = file.path(workspace_dir, paste0("hourly_88101_", year, ".csv"))
    )
  },
  FUN.VALUE = character(1),
  USE.NAMES = FALSE
)

missing_inputs <- input_paths[!file.exists(input_paths)]
if (length(missing_inputs) > 0L) {
  stop(
    "Missing input file(s): ",
    paste(missing_inputs, collapse = ", "),
    call. = FALSE
  )
}
input_paths <- normalizePath(input_paths, mustWork = TRUE)

analysis_path <- Sys.getenv(
  "PM25_ANALYSIS_PATH",
  unset = file.path(workspace_dir, "hourly-pm25-analysis.parquet")
)
manifest_path <- Sys.getenv(
  "PM25_MANIFEST_PATH",
  unset = file.path(workspace_dir, "hourly-pm25-analysis-manifest.csv")
)
dir.create(dirname(analysis_path), recursive = TRUE, showWarnings = FALSE)
analysis_path <- normalizePath(analysis_path, mustWork = FALSE)
manifest_path <- normalizePath(manifest_path, mustWork = FALSE)

n_folds <- env_integer("PM25_FOLDS", 5L, minimum = 2L)
max_sites <- env_integer("PM25_MAX_SITES", 0L, minimum = 0L)
duckdb_threads <- env_integer("PM25_DUCKDB_THREADS", 16L, minimum = 1L)

started <- Sys.time()
connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
DBI::dbExecute(connection, paste0("SET threads = ", duckdb_threads))
DBI::dbExecute(
  connection,
  paste0(
    "SET temp_directory = ",
    sql_string(file.path(tempdir(), "pm25-duckdb"))
  )
)

csv_paths_sql <- paste(
  vapply(input_paths, sql_string, character(1)),
  collapse = ", "
)
raw_sql <- paste0(
  "SELECT ",
  "CAST(\"State Code\" AS VARCHAR) AS state_code, ",
  "CAST(\"County Code\" AS VARCHAR) AS county_code, ",
  "CAST(\"Site Num\" AS VARCHAR) AS site_num, ",
  "CAST(\"Latitude\" AS DOUBLE) AS latitude, ",
  "CAST(\"Longitude\" AS DOUBLE) AS longitude, ",
  "CAST(\"Date Local\" AS DATE) AS local_date, ",
  "CAST(\"Time Local\" AS TIME) AS local_time, ",
  "CAST(\"Date GMT\" AS DATE) AS utc_date, ",
  "CAST(\"Time GMT\" AS TIME) AS utc_time, ",
  "CAST(\"Sample Measurement\" AS DOUBLE) AS label ",
  "FROM read_csv([",
  csv_paths_sql,
  "], ",
  "header = true, auto_detect = true, union_by_name = true, sample_size = 100000)"
)

eligible_sql <- paste0(
  "SELECT *, concat_ws('-', state_code, county_code, site_num) AS site_id ",
  "FROM (",
  raw_sql,
  ") raw ",
  "WHERE label IS NOT NULL AND latitude IS NOT NULL AND longitude IS NOT NULL ",
  "AND local_date IS NOT NULL AND local_time IS NOT NULL ",
  "AND utc_date IS NOT NULL AND utc_time IS NOT NULL"
)

message(
  "Counting rows by AQS site across ",
  length(input_paths),
  " input files ..."
)
source_counts <- DBI::dbGetQuery(
  connection,
  paste0(
    "SELECT count(*) AS source_rows, ",
    "count(*) FILTER (WHERE label IS NOT NULL AND latitude IS NOT NULL ",
    "AND longitude IS NOT NULL AND local_date IS NOT NULL ",
    "AND local_time IS NOT NULL AND utc_date IS NOT NULL ",
    "AND utc_time IS NOT NULL) AS eligible_rows ",
    "FROM (",
    raw_sql,
    ") raw"
  )
)

response_stats <- DBI::dbGetQuery(
  connection,
  paste0(
    "WITH labels AS (SELECT label FROM (",
    eligible_sql,
    ") eligible), ",
    "threshold AS (SELECT quantile_cont(label, 0.99) AS upper_clip FROM labels) ",
    "SELECT min(label) AS raw_response_min, max(label) AS raw_response_max, ",
    "upper_clip, count(*) FILTER (WHERE label < 0) AS lower_clipped_rows, ",
    "count(*) FILTER (WHERE label > upper_clip) AS upper_clipped_rows ",
    "FROM labels CROSS JOIN threshold GROUP BY upper_clip"
  )
)
response_upper_clip <- as.numeric(response_stats$upper_clip[[1L]])
if (!is.finite(response_upper_clip) || response_upper_clip <= 0) {
  stop("Could not calculate a positive global response clipping threshold.")
}
message(
  "Response winsorization: [0, ",
  format(response_upper_clip, digits = 10),
  "] ug/m3; ",
  format(response_stats$lower_clipped_rows[[1L]], big.mark = ","),
  " negative and ",
  format(response_stats$upper_clipped_rows[[1L]], big.mark = ","),
  " upper-tail values will be clipped."
)

site_counts <- DBI::dbGetQuery(
  connection,
  paste0(
    "SELECT site_id, count(*) AS n ",
    "FROM (",
    eligible_sql,
    ") eligible ",
    "GROUP BY site_id ORDER BY site_id"
  )
)
site_counts$n <- as.numeric(site_counts$n)

if (max_sites > 0L && max_sites < nrow(site_counts)) {
  site_counts <- site_counts[order(site_counts$site_id), , drop = FALSE]
  site_counts <- site_counts[seq_len(max_sites), , drop = FALSE]
  message("Smoke-test limit: retaining ", nrow(site_counts), " sites.")
}
if (nrow(site_counts) < n_folds) {
  stop("The selected data contain fewer sites than folds.", call. = FALSE)
}

# Greedy bin packing keeps whole sites together while balancing validation rows.
assignment_order <- order(-site_counts$n, site_counts$site_id)
fold_totals <- numeric(n_folds)
site_counts$fold <- NA_integer_
for (i in assignment_order) {
  selected_fold <- which.min(fold_totals)
  site_counts$fold[[i]] <- selected_fold
  fold_totals[[selected_fold]] <- fold_totals[[selected_fold]] +
    site_counts$n[[i]]
}
site_folds <- site_counts[, c("site_id", "fold")]
DBI::dbWriteTable(connection, "site_folds", site_folds, temporary = TRUE)

earth_radius_km <- 6371.0088
reference_latitude <- 37.5
reference_longitude <- -96
rbf_bandwidth_km <- 1200
local_rbf_bandwidth_km <- 150
rbf_knots <- data.frame(
  latitude = c(64.0, 20.0, 47.0, 34.0, 40.0, 32.0, 45.0, 33.0, 42.0, 18.5),
  longitude = c(
    -150.0,
    -157.0,
    -122.0,
    -118.0,
    -105.0,
    -99.0,
    -93.0,
    -84.0,
    -73.0,
    -66.5
  ),
  region = c(
    "Alaska",
    "Hawaii",
    "Pacific Northwest",
    "Southwest",
    "Rockies",
    "Plains and Texas",
    "Upper Midwest",
    "Southeast",
    "Northeast",
    "Puerto Rico"
  )
)
local_rbf_knots <- data.frame(
  latitude = c(40.0, 32.0, 45.0, 33.0),
  longitude = c(-105.0, -99.0, -93.0, -84.0),
  region = c("Rockies", "Plains and Texas", "Upper Midwest", "Southeast")
)

spatial_expressions <- c(
  "longitude AS sp_lon",
  "latitude AS sp_lat",
  sprintf(
    "%.10f * ((longitude - (%.10f)) * pi() / 180.0) * cos((%.10f) * pi() / 180.0) AS sp_eq_x_km",
    earth_radius_km,
    reference_longitude,
    reference_latitude
  ),
  sprintf(
    "%.10f * ((latitude - (%.10f)) * pi() / 180.0) AS sp_eq_y_km",
    earth_radius_km,
    reference_latitude
  ),
  "cos(lat_rad) * cos(lon_rad) AS sp_unit_x",
  "cos(lat_rad) * sin(lon_rad) AS sp_unit_y",
  "sin(lat_rad) AS sp_unit_z"
)

for (i in seq_len(nrow(rbf_knots))) {
  knot_lat_rad <- rbf_knots$latitude[[i]] * pi / 180
  knot_lon_rad <- rbf_knots$longitude[[i]] * pi / 180
  haversine_a <- sprintf(
    paste0(
      "pow(sin((lat_rad - (%.16f)) / 2.0), 2.0) + ",
      "cos(lat_rad) * cos(%.16f) * ",
      "pow(sin((lon_rad - (%.16f)) / 2.0), 2.0)"
    ),
    knot_lat_rad,
    knot_lat_rad,
    knot_lon_rad
  )
  distance <- sprintf(
    "2.0 * %.10f * asin(sqrt(least(1.0, greatest(0.0, %s))))",
    earth_radius_km,
    haversine_a
  )
  spatial_expressions <- c(
    spatial_expressions,
    sprintf(
      "exp(-0.5 * pow((%s) / %.10f, 2.0)) AS sp_rbf_%02d",
      distance,
      rbf_bandwidth_km,
      i
    )
  )
}

# Narrow bases add local spatial resolution to the broad 1,200-km bases. These
# four locations had maximum absolute correlations of only 0.28--0.34 with the
# original feature set in a reproducible 500,000-row screen.
for (i in seq_len(nrow(local_rbf_knots))) {
  knot_lat_rad <- local_rbf_knots$latitude[[i]] * pi / 180
  knot_lon_rad <- local_rbf_knots$longitude[[i]] * pi / 180
  haversine_a <- sprintf(
    paste0(
      "pow(sin((lat_rad - (%.16f)) / 2.0), 2.0) + ",
      "cos(lat_rad) * cos(%.16f) * ",
      "pow(sin((lon_rad - (%.16f)) / 2.0), 2.0)"
    ),
    knot_lat_rad,
    knot_lat_rad,
    knot_lon_rad
  )
  distance <- sprintf(
    "2.0 * %.10f * asin(sqrt(least(1.0, greatest(0.0, %s))))",
    earth_radius_km,
    haversine_a
  )
  spatial_expressions <- c(
    spatial_expressions,
    sprintf(
      "exp(-0.5 * pow((%s) / %.10f, 2.0)) AS sp_local_rbf_%02d",
      distance,
      local_rbf_bandwidth_km,
      i
    )
  )
}

temporal_expressions <- character()
for (harmonic in 1:8) {
  temporal_expressions <- c(
    temporal_expressions,
    sprintf(
      "sin(2.0 * pi() * %d * seconds_of_day / 86400.0) AS tm_diurnal_sin_%d",
      harmonic,
      harmonic
    ),
    sprintf(
      "cos(2.0 * pi() * %d * seconds_of_day / 86400.0) AS tm_diurnal_cos_%d",
      harmonic,
      harmonic
    )
  )
}
for (harmonic in 1:3) {
  temporal_expressions <- c(
    temporal_expressions,
    sprintf(
      "sin(2.0 * pi() * %d * elapsed_days / 7.0) AS tm_weekly_sin_%d",
      harmonic,
      harmonic
    ),
    sprintf(
      "cos(2.0 * pi() * %d * elapsed_days / 7.0) AS tm_weekly_cos_%d",
      harmonic,
      harmonic
    )
  )
}
for (harmonic in 1:6) {
  temporal_expressions <- c(
    temporal_expressions,
    sprintf(
      "sin(2.0 * pi() * %d * elapsed_days / 365.2425) AS tm_annual_sin_%d",
      harmonic,
      harmonic
    ),
    sprintf(
      "cos(2.0 * pi() * %d * elapsed_days / 365.2425) AS tm_annual_cos_%d",
      harmonic,
      harmonic
    )
  )
}
temporal_expressions <- c(
  temporal_expressions,
  "trend_scaled AS tm_trend_linear",
  "0.5 * (3.0 * pow(trend_scaled, 2.0) - 1.0) AS tm_trend_quadratic"
)

space_time_expressions <- c(
  paste0(
    "(latitude - 37.5) * sin(2.0 * pi() * elapsed_days / 365.2425) ",
    "AS st_lat_annual_sin_1"
  ),
  paste0(
    "(latitude - 37.5) * cos(2.0 * pi() * elapsed_days / 365.2425) ",
    "AS st_lat_annual_cos_1"
  ),
  paste0(
    "(longitude + 96.0) * sin(2.0 * pi() * elapsed_days / 365.2425) ",
    "AS st_lon_annual_sin_1"
  ),
  paste0(
    "(longitude + 96.0) * cos(2.0 * pi() * elapsed_days / 365.2425) ",
    "AS st_lon_annual_cos_1"
  )
)

feature_expressions <- c(
  spatial_expressions,
  temporal_expressions,
  space_time_expressions
)
feature_names <- sub("^.* AS ", "", feature_expressions)
if (
  length(spatial_expressions) != 21L ||
    length(temporal_expressions) != 36L ||
    length(space_time_expressions) != 4L ||
    length(feature_names) != 61L
) {
  stop("Internal feature-count error.", call. = FALSE)
}

analysis_query <- paste0(
  "WITH eligible AS (",
  eligible_sql,
  "), ",
  "selected AS (",
  "SELECT e.*, f.fold, ",
  "e.latitude * pi() / 180.0 AS lat_rad, ",
  "e.longitude * pi() / 180.0 AS lon_rad, ",
  "epoch(e.local_time) AS seconds_of_day ",
  "FROM eligible e INNER JOIN site_folds f USING (site_id)",
  "), engineered AS (",
  "SELECT *, date_diff('day', DATE '2024-01-01', local_date) + ",
  "seconds_of_day / 86400.0 AS elapsed_days, ",
  "2.0 * (date_diff('day', DATE ",
  sql_string(trend_reference_start),
  ", local_date) + seconds_of_day / 86400.0) / ",
  sprintf("%.1f", trend_span_days),
  " - 1.0 AS trend_scaled FROM selected",
  ") SELECT CAST(least(greatest(label, 0.0), ",
  sprintf("%.17g", response_upper_clip),
  ") AS DOUBLE) AS label, CAST(fold AS INTEGER) AS fold, ",
  "site_id, local_date, ",
  "CAST(local_date + local_time AS TIMESTAMP) AS local_timestamp, ",
  "CAST(utc_date + utc_time AS TIMESTAMP) AS utc_timestamp, ",
  paste(feature_expressions, collapse = ", "),
  " FROM engineered"
)

temporary_analysis_path <- paste0(
  analysis_path,
  ".tmp-",
  Sys.getpid(),
  ".parquet"
)
on.exit(unlink(temporary_analysis_path), add = TRUE)

message(
  "Writing ",
  length(feature_names),
  " engineered features to ",
  analysis_path,
  " ..."
)
DBI::dbExecute(
  connection,
  paste0(
    "COPY (",
    analysis_query,
    ") TO ",
    sql_string(temporary_analysis_path),
    " (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 122880)"
  )
)

if (!file.rename(temporary_analysis_path, analysis_path)) {
  if (!file.copy(temporary_analysis_path, analysis_path, overwrite = TRUE)) {
    stop(
      "Could not install the completed Parquet file at ",
      analysis_path,
      call. = FALSE
    )
  }
  unlink(temporary_analysis_path)
}

parquet_sql <- paste0("read_parquet(", sql_string(analysis_path), ")")
output_summary <- DBI::dbGetQuery(
  connection,
  paste0(
    "SELECT count(*) AS analysis_rows, count(DISTINCT site_id) AS sites, ",
    "min(label) AS response_min, max(label) AS response_max, ",
    "min(local_date) AS date_min, max(local_date) AS date_max ",
    "FROM ",
    parquet_sql
  )
)
output_folds <- DBI::dbGetQuery(
  connection,
  paste0(
    "SELECT fold, count(*) AS rows, count(DISTINCT site_id) AS sites ",
    "FROM ",
    parquet_sql,
    " GROUP BY fold ORDER BY fold"
  )
)

elapsed_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
manifest <- data.frame(
  key = c(
    "created_at",
    paste0("input_", input_years),
    "source_rows",
    "eligible_rows",
    "analysis_rows",
    "excluded_rows",
    "sites",
    "folds",
    "spatial_features",
    "temporal_features",
    "space_time_features",
    "total_features",
    "feature_names",
    "response_min",
    "response_max",
    "raw_response_min",
    "raw_response_max",
    "response_upper_clip",
    "lower_clipped_rows",
    "upper_clipped_rows",
    "date_min",
    "date_max",
    "rbf_bandwidth_km",
    "local_rbf_bandwidth_km",
    "trend_reference_period",
    "fold_assignment",
    "validation_axis",
    "response_policy",
    "preparation_seconds"
  ),
  value = c(
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    input_paths,
    as.character(source_counts$source_rows[[1L]]),
    as.character(source_counts$eligible_rows[[1L]]),
    as.character(output_summary$analysis_rows[[1L]]),
    as.character(
      source_counts$source_rows[[1L]] - output_summary$analysis_rows[[1L]]
    ),
    as.character(output_summary$sites[[1L]]),
    as.character(n_folds),
    "21",
    "36",
    "4",
    as.character(length(feature_names)),
    paste(feature_names, collapse = "|"),
    as.character(output_summary$response_min[[1L]]),
    as.character(output_summary$response_max[[1L]]),
    as.character(response_stats$raw_response_min[[1L]]),
    as.character(response_stats$raw_response_max[[1L]]),
    as.character(response_upper_clip),
    as.character(response_stats$lower_clipped_rows[[1L]]),
    as.character(response_stats$upper_clipped_rows[[1L]]),
    as.character(output_summary$date_min[[1L]]),
    as.character(output_summary$date_max[[1L]]),
    as.character(rbf_bandwidth_km),
    as.character(local_rbf_bandwidth_km),
    paste0(
      trend_reference_start,
      " through ",
      trend_reference_end - 1,
      " mapped linearly to [-1, 1]"
    ),
    "AQS site blocked; greedy row-balanced assignment",
    "site_id only; every timestamp for a site remains in the same fold",
    "winsorized to [0, global raw-response 99th percentile]; no rows removed",
    sprintf("%.6f", elapsed_seconds)
  ),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(output_folds))) {
  manifest <- rbind(
    manifest,
    data.frame(
      key = c(
        sprintf("fold_%d_rows", output_folds$fold[[i]]),
        sprintf("fold_%d_sites", output_folds$fold[[i]])
      ),
      value = as.character(c(output_folds$rows[[i]], output_folds$sites[[i]])),
      stringsAsFactors = FALSE
    )
  )
}
for (i in seq_len(nrow(rbf_knots))) {
  manifest <- rbind(
    manifest,
    data.frame(
      key = sprintf("rbf_%02d_knot", i),
      value = sprintf(
        "%s|lat=%.4f|lon=%.4f",
        rbf_knots$region[[i]],
        rbf_knots$latitude[[i]],
        rbf_knots$longitude[[i]]
      ),
      stringsAsFactors = FALSE
    )
  )
}
for (i in seq_len(nrow(local_rbf_knots))) {
  manifest <- rbind(
    manifest,
    data.frame(
      key = sprintf("local_rbf_%02d_knot", i),
      value = sprintf(
        "%s|lat=%.4f|lon=%.4f",
        local_rbf_knots$region[[i]],
        local_rbf_knots$latitude[[i]],
        local_rbf_knots$longitude[[i]]
      ),
      stringsAsFactors = FALSE
    )
  )
}
atomic_write_csv(manifest, manifest_path)

cat("\nPrepared hourly PM2.5 analysis data\n")
print(output_summary, row.names = FALSE)
cat("\nFold balance\n")
print(output_folds, row.names = FALSE)
cat("\nFeatures:", length(feature_names), "\n")
cat(paste(feature_names, collapse = ", "), "\n")
cat("Parquet:", analysis_path, "\n")
cat("Manifest:", manifest_path, "\n")
cat("Elapsed seconds:", round(elapsed_seconds, 1), "\n")
