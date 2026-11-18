# Combine dynamically branched sublists based on common column names

This function combines the sublists of dynamically branched targets
object as a preprocessor for `reduce_merge` function.

## Usage

``` r
reduce_list(df_list)
```

## Arguments

- df_list:

  list(1). List of data frames with common column names. Typically the
  output of a dynamically branched target.

## Value

a list object, with data frames combined by common column names.
