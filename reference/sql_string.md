# Quote SQL character literals

Wraps strings in single quotes and doubles embedded single quotes. This
helper does not escape backslashes; their interpretation depends on the
SQL parser configuration.

## Usage

``` r
sql_string(x)
```

## Arguments

- x:

  Character vector of values to quote.

## Value

A character vector of quoted string literals.
