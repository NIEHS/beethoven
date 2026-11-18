# Process atmospheric composition data by chunks

Returning a single `SpatRasterDataset` object. Removed `tapp` for
performance; impose a strict assumption that there are no missing values

## Usage

``` r
calc_geos_strict(
  path = NULL,
  date = c("2018-01-01", "2018-01-01"),
  locs = NULL,
  locs_id = NULL,
  ...
)
```

## Arguments

- path:

  character(1). Directory with downloaded netCDF (.nc4) files. or netCDF
  file paths.

- date:

  character(2). length of 10. Format "YYYY-MM-DD".

- locs:

  Locations to extract.

- locs_id:

  character(1). Location identifier.

- ...:

  Arguments passed to
  [`terra::rast`](https://rspatial.github.io/terra/reference/rast.html).

## Value

a `SpatRaster` object;

## Note

Layer names of the returned `SpatRaster` object contain the variable,
pressure level, date Reference duration: 1 day summary, all layers: 106
seconds hard-coded subsets for subdataset selection

## Author

Mitchell Manware, Insang Song
