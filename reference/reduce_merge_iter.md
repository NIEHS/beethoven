# Reduce and merge a list of data tables

This function iteratively runs `reduce_merge` on a list of `data.table`
objects for merging large `data.tables` that may not fit into memory
with `reduce_merge` directly.

## Usage

``` r
reduce_merge_iter(
  list_in,
  by = c("site_id", "time"),
  all.x = TRUE,
  all.y = FALSE
)
```

## Arguments

- list_in:

  A list of data tables to be merged.

- by:

  The columns to merge the data tables on. If `NULL`, the function will
  automatically detect the common column names.

- all.x:

  logical(1). Keeping all rows from the first input.

- all.y:

  logical(1). Keeping all rows from the second input.

## Value

A merged data table.
