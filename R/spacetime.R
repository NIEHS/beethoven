################################################################################
#' Generate spatial and temporal covariates at sf locations
#'
#' Writes the 61 covariates from the hourly PM2.5 benchmark for every supplied
#' location, date, and time of day. Spatial and temporal features are calculated
#' separately before DuckDB expands their combinations directly into Parquet.
#'
#' @param locs An `sf` object with a defined CRS and one row per location.
#'   Points are used directly. Polygon and multipolygon locations are
#'   represented by their centroids, calculated in the supplied CRS using
#'   [sf::st_centroid()].
#'   Supply points when a particular grid-cell centre is required.
#' @param date A vector of `Date` values, `YYYY-MM-DD` strings, or `YYYYMMDD`
#'   strings, as returned by [amadeus::sequence_dates()]. Every supplied date is
#'   used; a two-element vector is not expanded into a date range.
#' @param output Character(1). Destination `.parquet` file.
#' @param trend Two dates defining the start and exclusive end of the linear
#'   trend reference period. For the original 2024--2025 benchmark, use
#'   `c("2024-01-01", "2026-01-01")`. Reuse the same values for training,
#'   prediction, and every processing batch; they are not inferred from `date`.
#' @param hour Numeric hours from 0 through 23, or character times in `HHMM`,
#'   `HH:MM`, or `HH:MM:SS` format. Defaults to all 24 hourly points. Character
#'   times can also represent subhourly points from amadeus time sequences.
#' @param locs_id Character(1). Column containing unique, nonmissing location
#'   IDs. Its name and values are retained in the output.
#' @param tz Character(1). Time zone of the supplied dates and hours. Defaults
#'   to `"UTC"`, matching the current training and grid indices. Temporal
#'   covariates use this clock; the output `time` represents the corresponding
#'   UTC instant. No location-specific time zone is inferred. The benchmark used
#'   local clock values, so reproduce that convention explicitly when comparing
#'   its results. Repeated daylight-saving hours are not expanded; nonexistent
#'   times are rejected. UTC avoids daylight-saving ambiguity.
#' @param threads Positive integer. Number of DuckDB worker threads.
#' @param overwrite Logical. Whether to replace an existing output file.
#'
#' @details
#' The output contains 21 spatial features (coordinates, equirectangular and
#' spherical coordinates, ten broad and four local radial basis functions),
#' 36 temporal features (eight diurnal, three weekly, and six annual sine/cosine
#' pairs plus linear and quadratic trends), and four coordinate-by-annual-cycle
#' interactions. Knot locations, bandwidths, feature names, and formulas match
#' `inst/devel/07_prepare_hourly_pm25_benchmark.R`.
#'
#' Weekly and annual phases use 2024-01-01 as their fixed origin. Trend values
#' outside the reference period are extrapolated, not clipped. No observations,
#' response values, or fold assignments are required. Input IDs, dates, and
#' hours must each be unique to prevent duplicate location-time keys.
#'
#' @return The normalized output path. The Parquet file contains `locs_id`,
#'   `time`, and 61 numeric covariate columns. Row order is not guaranteed.
#' @examples
#' \dontrun{
#' dates <- amadeus::sequence_dates(c("2018-01-01", "2018-01-31"))
#' reference <- c("2018-01-01", "2020-01-01")
#' calculate_spacetime(
#'   sf_train_sites_sp, dates, "output/train/spacetime.parquet",
#'   trend = reference
#' )
#' calculate_spacetime(
#'   sf_grid_sites_sp, dates, "output/grid/spacetime.parquet",
#'   trend = reference
#' )
#' }
#' @export
calculate_spacetime <- function(
  locs,
  date,
  output,
  trend,
  hour = 0:23,
  locs_id = "site_id",
  tz = "UTC",
  threads = 1L,
  overwrite = FALSE
) {
  for (package in c("sf", "DBI", "duckdb")) {
    checkmate::assert_true(
      requireNamespace(package, quietly = TRUE),
      .var.name = paste0("installed package '", package, "'")
    )
  }
  checkmate::assert_class(locs, "sf")
  checkmate::assert_data_frame(locs, min.rows = 1L)
  checkmate::assert_false(is.na(sf::st_crs(locs)))
  checkmate::assert_string(locs_id)
  checkmate::assert_choice(locs_id, names(locs))
  ids <- locs[[locs_id]]
  checkmate::assert_atomic_vector(ids, any.missing = FALSE, unique = TRUE)
  if (is.factor(ids)) {
    ids <- as.character(ids)
  }
  geometry <- sf::st_geometry(locs)
  types <- as.character(sf::st_geometry_type(geometry))
  checkmate::assert_false(any(sf::st_is_empty(geometry)))
  checkmate::assert_subset(types, c("POINT", "POLYGON", "MULTIPOLYGON"))
  if (any(types != "POINT")) {
    geometry <- sf::st_centroid(geometry)
  }
  coordinates <- sf::st_coordinates(sf::st_transform(geometry, 4326))
  checkmate::assert_numeric(
    coordinates[, "X"],
    lower = -180,
    upper = 180,
    finite = TRUE,
    any.missing = FALSE
  )
  checkmate::assert_numeric(
    coordinates[, "Y"],
    lower = -90,
    upper = 90,
    finite = TRUE,
    any.missing = FALSE
  )

  date <- spacetime_dates(date, "date")
  trend <- spacetime_dates(trend, "trend")
  checkmate::assert_date(date, any.missing = FALSE, unique = TRUE)
  checkmate::assert_date(trend, any.missing = FALSE, len = 2L)
  checkmate::assert_true(trend[2] > trend[1])
  if (is.numeric(hour)) {
    checkmate::assert_integerish(
      hour,
      lower = 0,
      upper = 23,
      tol = 0,
      any.missing = FALSE,
      min.len = 1L
    )
    hour <- sprintf("%02d:00:00", as.integer(hour))
  }
  checkmate::assert_character(hour, any.missing = FALSE, min.len = 1L)
  hour <- sub("^([0-9]{2})([0-9]{2})$", "\\1:\\2", hour)
  hour <- sub("^([0-9]{2}:[0-9]{2})$", "\\1:00", hour)
  checkmate::assert_character(
    hour,
    pattern = "^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$",
    any.missing = FALSE,
    unique = TRUE
  )
  checkmate::assert_string(tz)
  checkmate::assert_choice(tz, c("UTC", OlsonNames()))
  checkmate::assert_int(
    threads,
    lower = 1,
    upper = .Machine$integer.max,
    tol = 0
  )
  checkmate::assert_flag(overwrite)
  checkmate::assert_string(output, pattern = "[.]parquet$", ignore.case = TRUE)
  output <- path.expand(output)
  checkmate::assert_false(dir.exists(output))
  checkmate::assert_true(overwrite || !file.exists(output))

  # Only the date/hour calendar and locations are held in R, not their product.
  calendar_date <- rep(date, each = length(hour))
  calendar_hour <- rep(hour, times = length(date))
  clock <- paste(calendar_date, calendar_hour)
  timestamps <- as.POSIXct(clock, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  checkmate::assert_posixct(timestamps, any.missing = FALSE, unique = TRUE)
  checkmate::assert_true(
    all(format(timestamps, "%Y-%m-%d %H:%M:%S", tz = tz) == clock)
  )
  seconds <- as.integer(substr(calendar_hour, 1, 2)) *
    3600 +
    as.integer(substr(calendar_hour, 4, 5)) * 60 +
    as.integer(substr(calendar_hour, 7, 8))
  calendar <- data.frame(
    time = timestamps,
    local_date = calendar_date,
    seconds_of_day = seconds
  )
  locations <- data.frame(
    site_id = ids,
    longitude = coordinates[, "X"],
    latitude = coordinates[, "Y"]
  )
  expressions <- spacetime_expressions()
  checkmate::assert_disjunct(locs_id, c("time", expressions$names))

  connection <- DBI::dbConnect(
    duckdb::duckdb(shared_home = FALSE),
    dbdir = ":memory:"
  )
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(connection, paste0("SET threads = ", as.integer(threads)))
  DBI::dbWriteTable(connection, "locations", locations, temporary = TRUE)
  DBI::dbWriteTable(connection, "calendar", calendar, temporary = TRUE)
  quote_string <- function(x) as.character(DBI::dbQuoteString(connection, x))
  quote_identifier <- function(x) {
    as.character(DBI::dbQuoteIdentifier(connection, x))
  }
  spatial_names <- sub("^.* AS ", "", expressions$spatial)
  temporal_names <- sub("^.* AS ", "", expressions$temporal)
  query <- paste0(
    "WITH spatial AS MATERIALIZED (SELECT site_id, longitude, latitude, ",
    paste(expressions$spatial, collapse = ", "),
    " FROM (SELECT *, latitude * pi() / 180.0 AS lat_rad, ",
    "longitude * pi() / 180.0 AS lon_rad FROM locations)), ",
    "temporal AS MATERIALIZED (SELECT time, elapsed_days, ",
    paste(expressions$temporal, collapse = ", "),
    " FROM (SELECT *, ",
    "date_diff('day', DATE '2024-01-01', local_date) + ",
    "seconds_of_day / 86400.0 AS elapsed_days, ",
    "2.0 * (date_diff('day', DATE ",
    quote_string(as.character(trend[1])),
    ", local_date) + seconds_of_day / 86400.0) / ",
    sprintf("%.1f", as.numeric(trend[2] - trend[1])),
    " - 1.0 AS trend_scaled FROM calendar)) ",
    "SELECT site_id AS ",
    quote_identifier(locs_id),
    ", to_timestamp(epoch(time)) AS time, ",
    paste(
      c(spatial_names, temporal_names, expressions$interaction),
      collapse = ", "
    ),
    " FROM spatial CROSS JOIN temporal"
  )

  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(output, mustWork = FALSE)
  temporary <- tempfile(
    ".spacetime-",
    tmpdir = dirname(output),
    fileext = ".parquet"
  )
  on.exit(unlink(temporary), add = TRUE)
  DBI::dbExecute(
    connection,
    paste0(
      "COPY (",
      query,
      ") TO ",
      quote_string(temporary),
      " (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 122880)"
    )
  )
  checkmate::assert_true(file.rename(temporary, output))
  normalizePath(output, mustWork = TRUE)
}

################################################################################
#' Parse dates for spatiotemporal covariates
#' @keywords internal
#' @noRd
spacetime_dates <- function(x, name) {
  if (is.character(x)) {
    checkmate::assert_character(
      x,
      any.missing = FALSE,
      min.len = 1L,
      .var.name = name
    )
    text <- sub("^([0-9]{4})([0-9]{2})([0-9]{2})$", "\\1-\\2-\\3", x)
    x <- as.Date(text, format = "%Y-%m-%d")
    checkmate::assert_true(all(format(x, "%Y-%m-%d") == text), .var.name = name)
  }
  checkmate::assert_date(x, any.missing = FALSE, min.len = 1L, .var.name = name)
  checkmate::assert_numeric(as.numeric(x), finite = TRUE, .var.name = name)
  x
}

################################################################################
#' SQL expressions from the hourly PM2.5 benchmark
#' @keywords internal
#' @noRd
spacetime_expressions <- function() {
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
      # nolint start
      "%.10f * ((longitude - (%.10f)) * pi() / 180.0) * cos((%.10f) * pi() / 180.0) AS sp_eq_x_km",
      # nolint end
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
  checkmate::assert_character(
    spatial_expressions,
    len = 21L,
    any.missing = FALSE
  )
  checkmate::assert_character(
    temporal_expressions,
    len = 36L,
    any.missing = FALSE
  )
  checkmate::assert_character(
    space_time_expressions,
    len = 4L,
    any.missing = FALSE
  )
  checkmate::assert_character(feature_names, len = 61L, any.missing = FALSE)

  list(
    spatial = spatial_expressions,
    temporal = temporal_expressions,
    interaction = space_time_expressions,
    names = feature_names
  )
}

################################################################################
