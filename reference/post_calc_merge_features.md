# Merge input data.frame objects

Merge input data.frame objects

## Usage

``` r
post_calc_merge_features(by = c("site_id"), time = FALSE, ...)
```

## Arguments

- by:

  character. Joining keys. See
  [`data.table::merge`](https://rdrr.io/pkg/data.table/man/merge.html)
  for details.

- time:

  logical(1). Whether or not include time identifier. Set this `TRUE`
  will supersede `by` value by appending time identifier.

- ...:

  data.frame objects to merge

## Value

data.table
