# Automatic joining by the time and spatial identifiers

The key assumption is that all data frames will have time field and
spatial field and the data should have one of date or year. Whether the
input time unit is year or date is determined by the coercion of the
**first row value** of the time field into a character with
[`as.Date()`](https://rdrr.io/r/base/as.Date.html). This function will
fail if it gets year-like string with length 4.

## Usage

``` r
post_calc_autojoin(
  df_fine,
  df_coarse,
  field_sp = "site_id",
  field_t = "time",
  year_start = 2018L,
  year_end = 2022L
)
```

## Arguments

- df_fine:

  The fine-grained data frame.

- df_coarse:

  The coarse-grained data frame.

- field_sp:

  The name of the spatial field in the data frames.

- field_t:

  The name of the time field in the data frames.

- year_start:

  The starting year of the time period.

- year_end:

  The ending year of the time period.

## Value

A merged data table.

## Examples

``` r
if (FALSE) { # \dontrun{
df_fine0 <- data.frame(site_id = c("A", "B", "B", "C"),
                      time = as.Date(c("2022-01-01", "2022-01-02", "2021-12-31", "2021-01-03")),
                      value = c(1, 2, 3, 5))
df_coarse0 <- data.frame(site_id = c("A", "B", "C"),
                        time = c("2022", "2022", "2021"),
                        other_value = c(10, 20, 30))
jdf <- post_calc_autojoin(df_fine0, df_coarse0)
print(jdf)
} # }
```
