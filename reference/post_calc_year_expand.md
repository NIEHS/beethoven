# Map the available raw data years over the given period

Many raw datasets are periodically updated and the period could be
longer than a year. This function maps the available years over the
given period.

## Usage

``` r
post_calc_year_expand(
  time_start = NULL,
  time_end = NULL,
  time_unit = "year",
  time_available = NULL
)
```

## Arguments

- time_start:

  integer(1). Starting year.

- time_end:

  integer(1). Ending year.

- time_unit:

  character(1). Time unit. Default is `"year"`.

- time_available:

  vector. Available years.

## Value

integer vector of length (time_end - time_start + 1). Each element will
get the nearest preceeding available year.

## Note

The minimum of `time_available` will be filled in front of the first
available year when the minimum of `time_available` is greater than
`time_start`.

## Examples

``` r
if (FALSE) { # \dontrun{
process_calc_year_expand(2018, 2022, "year", c(2017, 2020, 2021))
process_calc_year_expand(2018, 2022, "year", c(2020, 2021))
} # }
```
