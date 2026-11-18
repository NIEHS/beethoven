# Split a date range into subranges

Split a date range into subranges of equal length as a list.

## Usage

``` r
split_dates(dates, n, year = TRUE, julian = FALSE)
```

## Arguments

- dates:

  character(2). date to query. `"YYYY-MM-DD"` format.

- n:

  integer(1). Number of dates in each subrange.

- year:

  logical(1). If `TRUE`, sublists will contain only one year. This may
  result in sublists with fewer than `n` dates.

- julian:

  logical(1). If `TRUE`, dates are in Julian format.

## Value

a list object, with date ranges split into subranges.
