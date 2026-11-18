# Calculate MODIS product covariates in multiple CPU threads

`calculate_modis` essentially runs
[`calculate_modis_daily`](https://niehs.github.io/amadeus/reference/calculate_modis_daily.html)
function in each thread (subprocess). Based on daily resolution, each
day's workload will be distributed to each thread. With `product`
argument, the files are processed by a customized function where the
unique structure and/or characteristics of the products are considered.
`nthreads` argument should be carefully selected in consideration of the
machine's CPU and memory capacities as products have their own memory
pressure. `locs` should be `sf` object as it is exportable to parallel
workers.

## Usage

``` r
calculate_modis(
  from = NULL,
  locs = NULL,
  locs_id = "site_id",
  radius = c(0L, 1000L, 10000L, 50000L),
  preprocess = amadeus::process_modis_merge,
  name_covariates = NULL,
  subdataset = NULL,
  fun_summary = "mean",
  package_list_add = NULL,
  export_list_add = NULL,
  max_cells = 3e+07,
  geom = FALSE,
  ...
)
```

## Arguments

- from:

  character. List of paths to MODIS/VIIRS files.

- locs:

  sf/SpatVector object. Unique locs where covariates will be calculated.

- locs_id:

  character(1). Site identifier. Default is `"site_id"`

- radius:

  numeric. Radii to calculate covariates. Default is
  `c(0, 1000, 10000, 50000)`.

- preprocess:

  function. Function to handle HDF files.

- name_covariates:

  character. Name header of covariates. e.g., `"MOD_NDVIF_0_"`. The
  calculated covariate names will have a form of
  "`{name_covariates}{zero-padded buffer radius in meters}`", e.g.,
  'MOD_NDVIF_0_50000' where 50 km radius circular buffer was used to
  calculate mean NDVI value.

- subdataset:

  Indices, names, or search patterns for subdatasets. Find detail usage
  of the argument in notes.

- fun_summary:

  character or function. Function to summarize extracted raster values.

- package_list_add:

  character. A vector with package names to load these in each thread.
  Note that `sf`, `terra`, `exactextractr`, `doParallel`, `parallelly`
  and `dplyr` are the default packages to be loaded.

- export_list_add:

  character. A vector with object names to export to each thread. It
  should be minimized to spare memory.

- max_cells:

  integer(1). Maximum number of cells to be read at once. Higher values
  will expedite processing, but will increase memory usage. Maximum
  possible value is `2^31 - 1`. See
  [`exactextractr::exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  for details.

- geom:

  FALSE/"sf"/"terra".. Should the function return with geometry? Default
  is `FALSE`, options with geometry are "sf" or "terra". The coordinate
  reference system of the `sf` or `SpatVector` is that of `from.`

- ...:

  Arguments passed to `preprocess`.

## Value

A data.frame or SpatVector with an attribute:

- `attr(., "dates_dropped")`: Dates with insufficient tiles. Note that
  the dates mean the dates with insufficient tiles, not the dates
  without available tiles.

## Note

Overall, this function and dependent routines assume that the file
system can handle concurrent access to the (network) disk by multiple
processes. File system characteristics, package versions, and hardware
settings and specification can affect the processing efficiency. `locs`
is expected to be convertible to `sf` object. `sf`, `SpatVector`, and
other class objects that could be converted to `sf` can be used. Common
arguments in `preprocess` functions such as `date` and `path` are
automatically detected and passed to the function. Please note that
`locs` here and `path` in `preprocess` functions are assumed to have a
standard naming convention of raw files from NASA. The argument
`subdataset` should be in a proper format depending on `preprocess`
function:

- `process_modis_merge()`: Regular expression pattern. e.g., `"^LST_"`

- `process_modis_swath()`: Subdataset names. e.g.,
  `c("Cloud_Fraction_Day", "Cloud_Fraction_Night")`

- `process_blackmarble()`: Subdataset number. e.g., for VNP46A2 product,
  3L. Dates with less than 80 percent of the expected number of tiles,
  which are determined by the mode of the number of tiles, are removed.
  Users will be informed of the dates with insufficient tiles. The
  result data.frame will have an attribute with the dates with
  insufficient tiles.

## See also

This function leverages the calculation of single-day MODIS covariates:

- [`calculate_modis_daily()`](https://niehs.github.io/amadeus/reference/calculate_modis_daily.html)

Also, for preprocessing, please refer to:

- [`process_modis_merge()`](https://niehs.github.io/amadeus/reference/process_modis_merge.html)

- [`process_modis_swath()`](https://niehs.github.io/amadeus/reference/process_modis_swath.html)

- [`process_blackmarble()`](https://niehs.github.io/amadeus/reference/process_blackmarble.html)

## Examples

``` r
## NOTE: Example is wrapped in `\dontrun{}` as function requires a large
##       amount of data which is not included in the package.
if (FALSE) { # \dontrun{
locs <- data.frame(lon = -78.8277, lat = 35.95013, id = "001")
locs <- terra::vect(locs, geom = c("lon", "lat"), crs = "EPSG:4326")
calculate_modis(
  from =
    list.files("./data", pattern = "VNP46A2.", full.names = TRUE),
  locs = locs,
  locs_id = "site_id",
  radius = c(0L, 1000L),
  preprocess = process_modis_merge,
  name_covariates = "cloud_fraction_0",
  subdataset = "Cloud_Fraction",
  fun_summary = "mean"
)
} # }
```
