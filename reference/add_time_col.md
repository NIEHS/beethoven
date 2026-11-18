# Add Time Column

This function adds a time column to a data frame.

## Usage

``` r
add_time_col(df, time_value, time_id = "time")
```

## Arguments

- df:

  The data frame to which the time column will be added.

- time_value:

  The value to be assigned to the time column.

- time_id:

  The name of the time column (default is "time").

## Value

The data frame with the added time column.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(x = 1:5, y = letters[1:5])
add_time_col(df, "2022-01-01")
} # }
```
