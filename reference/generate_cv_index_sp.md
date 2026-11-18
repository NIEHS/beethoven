# Prepare spatial and spatiotemporal cross validation sets

Prepare spatial and spatiotemporal cross validation sets

## Usage

``` r
generate_cv_index_sp(data, target_cols = c("lon", "lat"), ...)
```

## Arguments

- data:

  data.table with X, Y, and time information.

- target_cols:

  character(3). Names of columns for X, Y. Default is `c("lon", "lat")`.
  It is passed to sf::st_as_sf to subsequently generate spatial
  cross-validation indices using
  [`spatialsample::spatial_block_cv`](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html).

- ...:

  Additional arguments to be passed to `patialsample::spatial_block_cv`.

## Value

A list of numeric vectors with in- and out-of-sample row indices or a
numeric vector with out-of-sample indices.

## See also

[`spatialsample::spatial_block_cv`](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html).
