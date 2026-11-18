# Post-calculation column renaming

This function renames the columns of the input `data` based on the
`prefix` and original column names.

## Usage

``` r
post_calc_cols(data, prefix = NULL, skip = c("site_id", "time"))
```

## Arguments

- data:

  data.frame(1)

- prefix:

  character(1). The prefix to be added to the column names.

- skip:

  character. The column names to be skipped from renaming. Default is
  `c("site_id", "time")`.

## Value

data.frame with renamed columns.
