# Expand a data frame by year

This function expands a data frame by year, creating multiple rows for
each year based on the time period specified.

## Usage

``` r
post_calc_df_year_expand(
  df,
  locs_id = "site_id",
  time_field = "time",
  time_start = NULL,
  time_end = NULL,
  time_unit = "year",
  time_available = NULL,
  ...
)
```

## Arguments

- df:

  The input data frame. The data frame should have the same number of
  rows per year, meaning that it assumes this argument is a spatial-only
  feature data.frame.

- locs_id:

  The column name of the location identifier in the data frame.

- time_field:

  The column name of the time field in the data frame.

- time_start:

  The start of the time period.

- time_end:

  The end of the time period.

- time_unit:

  The unit of time to expand the data frame. Only for record.

- time_available:

  A vector of available time periods.

- ...:

  Placeholders.

## Value

The expanded data frame with multiple rows for each year.

## Note

Year expansion rule is to assign the nearest past year in the available
years; if there is no past year in the available years, the first
available year is rolled back to the start of the time period.

## See also

[`post_calc_year_expand()`](https://niehs.github.io/beethoven/reference/post_calc_year_expand.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(year = c(2010, 2010, 2011, 2012),
                 value = c(1, 2, 3, 4))
df_expanded <-
  post_calc_df_year_expand(df, locs_id = "site_id", time_field = "year",
                           time_start = 2011, time_end = 2012,
                           time_unit = "year")
print(df_expanded)
} # }
```
