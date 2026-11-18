# Merge spatial and spatiotemporal covariate data

Merge spatial and spatiotemporal covariate data

## Usage

``` r
post_calc_merge_all(
  locs,
  locs_id,
  time_id,
  target_years = seq(2018, 2022),
  df_sp,
  df_spt
)
```

## Arguments

- locs:

  Location. e.g., AQS sites.

- locs_id:

  character(1). Location identifier.

- time_id:

  character(1). Location identifier.

- target_years:

  integer. Used to dummify nominal year.

- df_sp:

  data.frame. Spatial-only covariates.

- df_spt:

  data.frame. Spatiotemporal covariates.

## Value

data.frame

## Note

This version assumes the time_id contains Date-like strings.
