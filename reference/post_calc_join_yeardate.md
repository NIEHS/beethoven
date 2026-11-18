# Join a data.frame with a year-only date column to that with a full date column

The full date column will be converted to a year column as a new column,
then the data.frame with the year-only column will be joined.

## Usage

``` r
post_calc_join_yeardate(
  df_year,
  df_date,
  field_year = "time",
  field_date = "time",
  spid = "site_id"
)
```

## Arguments

- df_year:

  data.frame with a year-only date column

- df_date:

  data.frame with a full date column

- field_year:

  character(1). Year column in `df_year`

- field_date:

  character(1). Date column in `df_date`

- spid:

  character(1). Name of the unique location identifier field.

## Value

data.frame
