# Process atmospheric composition data by chunks (v2)

Returning a single `SpatRasterDataset` object.

## Usage

``` r
process_geos_bulk(path = NULL, date = c("2018-01-01", "2018-01-01"), ...)
```

## Arguments

- path:

  character(1). Directory with downloaded netCDF (.nc4) files. or netCDF
  file paths.

- date:

  character(2). length of 10. Format "YYYY-MM-DD".

- ...:

  Arguments passed to
  [`terra::rast`](https://rspatial.github.io/terra/reference/rast.html).

## Value

a `SpatRaster` object;

## Note

Layer names of the returned `SpatRaster` object contain the variable,
pressure level, date Reference duration: 1 day summary, all layers: 115
seconds Superseded by
[`calc_geos_strict`](https://niehs.github.io/beethoven/reference/calc_geos_strict.md).

## Author

Mitchell Manware, Insang Song
