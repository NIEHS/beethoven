# Reflown gmted processing

Reflown gmted processing

## Usage

``` r
calc_gmted_direct(
  variable = NULL,
  path = NULL,
  locs = NULL,
  locs_id = NULL,
  win = c(-126, -62, 22, 52),
  radius = 0,
  fun = "mean",
  ...
)
```

## Arguments

- variable:

  character(2). Statistic and resolution.

- path:

  character(1). Directory with downloaded GMTED files.

- locs:

  data.frame/SpatVector/sf. Locations.

- locs_id:

  character(1). Location identifier.

- win:

  numeric(4). Window for the raster.

- radius:

  numeric(1). Radius for the extraction.

- fun:

  character(1). Function to apply.

- ...:

  Additional parameters to be passed to other functions.

## Value

A data.frame containing the extracted GMTED data.
