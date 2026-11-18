# Remove columns from a data frame based on regular expression patterns. This function removes columns from a data frame that match any of the specified regular expression patterns. By default, it removes columns with names that match the patterns "^lon\$\|^lat\$\|geoid\|year\$\|description".

Remove columns from a data frame based on regular expression patterns.
This function removes columns from a data frame that match any of the
specified regular expression patterns. By default, it removes columns
with names that match the patterns
"^lon\$\|^lat\$\|geoid\|year\$\|description".

## Usage

``` r
post_calc_drop_cols(
  df,
  candidates = "(^lon$|^lat$|geoid|year$|description|geometry)",
  strict = FALSE
)
```

## Arguments

- df:

  The input data frame.

- candidates:

  A character vector of regular expression patterns to match against
  column names. Columns that match any of the patterns will be removed.
  The default value is "^lon\$\|^lat\$\|geoid\|year\$\|description".

- strict:

  logical(1). If `TRUE`, only `c("site_id", "time")` will be kept.

## Value

The modified data frame with the specified columns removed.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(lon = 1:5, lat = 6:10, geoid = 11:15, year = 2010:2014,
                 description = letters[1:5], other = 16:20)
post_calc_drop_cols(df)
} # }
```
