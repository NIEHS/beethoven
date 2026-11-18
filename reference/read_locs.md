# Read AQS data

Read AQS data

## Usage

``` r
read_locs(fun_aqs = amadeus::process_aqs, export = FALSE, ...)
```

## Arguments

- fun_aqs:

  function to import AQS data. Default is
  [`amadeus::process_aqs`](https://niehs.github.io/amadeus/reference/process_aqs.html)

- export:

  Export the file to qs. Default is FALSE.

- ...:

  Passed arguments to `fun_aqs`

## Value

Depending on `fun_aqs` specification.
