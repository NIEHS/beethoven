# Generate argument list for raw data download

Generate argument list for raw data download

## Usage

``` r
set_args_download(
  char_period = c("2018-01-01", "2022-10-31"),
  char_input_dir = "input",
  nasa_earth_data_token = NULL,
  mod06_filelist = NULL,
  year_nlcd = c(2019, 2021),
  export = FALSE,
  path_export = "inst/targets/download_spec.qs"
)
```

## Arguments

- char_period:

  Character(2) vector specifying the time period. Default is
  c("2018-01-01", "2022-10-31").

- char_input_dir:

  Character string specifying the input path. Default is "input".

- nasa_earth_data_token:

  Character string specifying the NASA Earth Data token.

- mod06_filelist:

  character(1). File path to a CSV file with MOD06 download URLs.

- year_nlcd:

  numeric(2). Numeric vector specifying the NLCD years. Default is
  c(2019, 2021).

- export:

  logical(1). If TRUE, the list is saved to `path_export`. Default is
  `TRUE`.

- path_export:

  Character string specifying the export path. Default is
  "inst/targets/download_spec.qs".
