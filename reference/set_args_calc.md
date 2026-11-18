# Set arguments for the calculation process

This function sets the arguments for the calculation process. It takes
several parameters including site ID, time ID, time period, extent, user
email, export path, and input path.

## Usage

``` r
set_args_calc(
  char_siteid = "site_id",
  char_timeid = "time",
  char_period = c("2018-01-01", "2022-10-31"),
  num_extent = c(-126, -62, 22, 52),
  char_user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
  export = FALSE,
  path_export = "inst/targets/calc_spec.qs",
  char_input_dir = "input",
  nthreads_nasa = 14L,
  nthreads_tri = 5L,
  nthreads_geoscf = 10L,
  nthreads_gmted = 4L,
  nthreads_narr = 24L,
  nthreads_groads = 3L,
  nthreads_population = 3L,
  nthreads_append = 8L,
  nthreads_impute = 64L
)
```

## Arguments

- char_siteid:

  Character string specifying the site ID. Default is "site_id".

- char_timeid:

  Character string specifying the time ID. Default is "time".

- char_period:

  Character vector specifying the time period. Default is
  c("2018-01-01", "2022-10-31").

- num_extent:

  Numeric vector specifying the extent. Default is c(-126, -62, 22, 52).

- char_user_email:

  Character string specifying the user email. Default is the current
  user's email with nih.gov domain.

- export:

  logical(1). If TRUE, the list for the calculation process is exported
  to `path_export`. Default is FALSE.

- path_export:

  Character string specifying the export path. Default is
  "inst/targets/calc_spec.qs".

- char_input_dir:

  Character string specifying the input path. Default is "input".

- nthreads_nasa:

  integer(1). Number of threads for NASA data. Default is 14L.

- nthreads_tri:

  integer(1). Number of threads for TRI data. Default is 5L.

- nthreads_geoscf:

  integer(1). Number of threads for GEOSCF data. Default is 10L.

- nthreads_gmted:

  integer(1). Number of threads for GMTED data. Default is 4L.

- nthreads_narr:

  integer(1). Number of threads for NARR data. Default is 24L.

- nthreads_groads:

  integer(1). Number of threads for GROADS data. Default is 3L.

- nthreads_population:

  integer(1). Number of threads for population data. Default is 3L.

- nthreads_append:

  integer(1). Number of threads for appending data. Default is 8L.

- nthreads_impute:

  integer(1). Number of threads for imputing data. Default is 64L.

## Value

A list of arguments for common use in the calculation process. A \*.qs
or \*.rds file defined in `path_export` is saved if `export` is TRUE.

- char_siteid: Character string specifying the site ID.

- char_timeid: Character string specifying the time ID.

- char_period: Character vector specifying the time period.

- num_extent: Numeric vector specifying the extent.

- char_user_email: Character string specifying the user email.

- char_input_dir: Character string specifying the input path.

- nthreads_nasa: Number of threads for NASA data.

- nthreads_tri: Number of threads for TRI data.

- nthreads_geoscf: Number of threads for GEOS-CF data.

- nthreads_gmted: Number of threads for GMTED data.

- nthreads_narr: Number of threads for NARR data.

- nthreads_groads: Number of threads for SEDAC Groads data.

- nthreads_population: Number of threads for population data.

- nthreads_append: Number of threads for appending data.

- nthreads_impute: Number of threads for imputing data.

## Note

The number of threads used is fixed as 1L otherwise specified in
`nthreads_*` arguments. path_input should contain the following
subdirectories:

- modis/raw/61/MOD11A1

- modis/raw/61/MOD06_L2

- modis/raw/61/MOD09GA

- modis/raw/61/MCD19A2

- modis/raw/61/MOD13A2

- modis/raw/5000/VNP46A2

- aqs

- nlcd

- geos/aqc_tavg_1hr_g1440x721_v1

- geos/chm_tavg_1hr_g1440x721_v1

- HMS_Smoke/data

- gmted

- nei

- narr

- HMS_Smoke

- koppen_geiger

- ecoregions

- sedac_groads

- sedac_population

## Author

Insang Song
