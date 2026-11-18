# Generate spatial and temporal covariates at sf locations

Writes the 61 covariates from the hourly PM2.5 benchmark for every
supplied location, date, and time of day. Spatial and temporal features
are calculated separately before DuckDB expands their combinations
directly into Parquet.

## Usage

``` r
calculate_spacetime(
  locs,
  date,
  output,
  trend,
  hour = 0:23,
  locs_id = "site_id",
  tz = "UTC",
  threads = 1L,
  overwrite = FALSE
)
```

## Arguments

- locs:

  An `sf` object with a defined CRS and one row per location. Points are
  used directly. Polygon and multipolygon locations are represented by
  their centroids, calculated in the supplied CRS using
  [`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html).
  Supply points when a particular grid-cell centre is required.

- date:

  A vector of `Date` values, `YYYY-MM-DD` strings, or `YYYYMMDD`
  strings, as returned by
  [`amadeus::sequence_dates()`](https://niehs.github.io/amadeus/reference/sequence_dates.html).
  Every supplied date is used; a two-element vector is not expanded into
  a date range.

- output:

  Character(1). Destination `.parquet` file.

- trend:

  Two dates defining the start and exclusive end of the linear trend
  reference period. For the original 2024–2025 benchmark, use
  `c("2024-01-01", "2026-01-01")`. Reuse the same values for training,
  prediction, and every processing batch; they are not inferred from
  `date`.

- hour:

  Numeric hours from 0 through 23, or character times in `HHMM`,
  `HH:MM`, or `HH:MM:SS` format. Defaults to all 24 hourly points.
  Character times can also represent subhourly points from amadeus time
  sequences.

- locs_id:

  Character(1). Column containing unique, nonmissing location IDs. Its
  name and values are retained in the output.

- tz:

  Character(1). Time zone of the supplied dates and hours. Defaults to
  `"UTC"`, matching the current training and grid indices. Temporal
  covariates use this clock; the output `time` represents the
  corresponding UTC instant. No location-specific time zone is inferred.
  The benchmark used local clock values, so reproduce that convention
  explicitly when comparing its results. Repeated daylight-saving hours
  are not expanded; nonexistent times are rejected. UTC avoids
  daylight-saving ambiguity.

- threads:

  Positive integer. Number of DuckDB worker threads.

- overwrite:

  Logical. Whether to replace an existing output file.

## Value

The normalized output path. The Parquet file contains `locs_id`, `time`,
and 61 numeric covariate columns. Row order is not guaranteed.

## Details

The output contains 21 spatial features (coordinates, equirectangular
and spherical coordinates, ten broad and four local radial basis
functions), 36 temporal features (eight diurnal, three weekly, and six
annual sine/cosine pairs plus linear and quadratic trends), and four
coordinate-by-annual-cycle interactions. Knot locations, bandwidths,
feature names, and formulas match
`inst/devel/07_prepare_hourly_pm25_benchmark.R`.

Weekly and annual phases use 2024-01-01 as their fixed origin. Trend
values outside the reference period are extrapolated, not clipped. No
observations, response values, or fold assignments are required. Input
IDs, dates, and hours must each be unique to prevent duplicate
location-time keys.

## Examples

``` r
if (FALSE) { # \dontrun{
dates <- amadeus::sequence_dates(c("2018-01-01", "2018-01-31"))
reference <- c("2018-01-01", "2020-01-01")
calculate_spacetime(
  sf_train_sites_sp, dates, "output/train/spacetime.parquet",
  trend = reference
)
calculate_spacetime(
  sf_grid_sites_sp, dates, "output/grid/spacetime.parquet",
  trend = reference
)
} # }
```
