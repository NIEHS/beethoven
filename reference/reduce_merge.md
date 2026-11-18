# Reduce and merge a list of data tables

This function takes a list of data tables and merges them together using
the specified columns. It uses the `merge.data.table` function from the
`data.table` package to perform the merge.

## Usage

``` r
reduce_merge(list_in, by = c("site_id", "time"), all.x = TRUE, all.y = FALSE)
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

## Examples

``` r
if (FALSE) { # \dontrun{
# Create example data tables
dt1 <- data.table(a = 1:3, b = 4:6)
dt2 <- data.table(a = 2:4, c = 7:9)
dt3 <- data.table(a = 3:5, d = 10:12)

# Merge the data tables
reduce_merge(list(dt1, dt2, dt3), by = "a")
} # }
```
