# Spatiotemporal covariate calculation

Spatiotemporal covariate calculation

## Usage

``` r
calculate(
  domain = NULL,
  domain_name = "year",
  process_function = amadeus::process_covariates,
  calc_function = amadeus::calculate_covariates,
  ...
)
```

## Arguments

- domain:

  vector of integer/character/Date. Depending on temporal resolution of
  raw datasets. Nullable; If `NULL`, it will be set to `c(1)`.

- domain_name:

  character(1). Name of the domain. Default is `"year"`.

- process_function:

  Raw data processor. Default is
  [`amadeus::process_covariates`](https://niehs.github.io/amadeus/reference/process_covariates.html)

- calc_function:

  Function to calculate covariates.
  [`amadeus::calculate_covariates`](https://niehs.github.io/amadeus/reference/calculate_covariates.html)

- ...:

  Arguments passed to `process_function` and `calc_function`

## Value

A data.table object.
