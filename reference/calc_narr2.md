# Calculate aggregated values for specified locations

This function calculates aggregated values for specified locations from
a raster dataset.

## Usage

``` r
calc_narr2(from, locs, locs_id = NULL, radius = 0, fun = "mean", ...)
```

## Arguments

- from:

  The raster dataset from which to extract values.

- locs:

  A data frame containing the locations for which to calculate
  aggregated values. It should have a column in `locs_id` value that
  contains unique identifiers for each location.

- locs_id:

  An optional column name in the `locs` data frame that contains
  additional location identifiers.

- radius:

  The radius within which to include neighboring locations for
  aggregation. Default is 0.

- fun:

  The aggregation function to use. It can be a character string
  specifying a function name (e.g., "mean", "sum"), or it can be a
  custom function. Default is "mean".

- ...:

  Additional arguments to be passed to the aggregation function.

## Value

A data frame containing the aggregated values for each location and time
point.
