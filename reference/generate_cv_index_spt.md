# Generate spatio-temporal cross-validation index with `spatialsample::spatial_block_cv` and year-based temporal folds

This function generates spatio-temporal cross-validation indices with
`v` spatial blocks and year-based temporal folds. The spatial blocks are
generated with
[`spatialsample::spatial_block_cv`](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html)
function, and the temporal folds are generated based on the years
availble in `data$time`. Total number of folds is equal to
`v * length(unique(substr(data$time, 1, 4)))`.

## Usage

``` r
generate_cv_index_spt(
  data,
  crs = NULL,
  cellsize = NULL,
  locs_id = "site_id",
  coords = c("lon", "lat"),
  v = 10L,
  time_id = "time"
)
```

## Arguments

- data:

  data.table or data.frame with `id`, `coords`, and `time` columns.

- crs:

  The coordinate reference system (CRS) of the spatial object

- cellsize:

  The size of each spatial block in meters. Uses the crs units

- locs_id:

  The column name in `data` that represents the location identifier.

- coords:

  The column names in the spatial object that represent the XY
  coordinates. Default is `c("lon", "lat")`.

- v:

  integer(1). The number of partitions for the resampling.

- time_id:

  The column name in `data` that represents the time values.

## See also

[`spatialsample::spatial_block_cv`](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html)
