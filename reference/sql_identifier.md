# Quote Spark SQL identifiers

Wraps identifiers in backticks and escapes embedded backticks by
doubling them.

## Usage

``` r
sql_identifier(x)
```

## Arguments

- x:

  Character vector of table or column names.

## Value

A character vector of quoted identifiers.
