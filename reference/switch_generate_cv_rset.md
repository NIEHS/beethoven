# Choose cross-validation strategy for the base learner

Choose cross-validation strategy for the base learner

## Usage

``` r
switch_generate_cv_rset(
  learner = c("spatial", "temporal", "spatiotemporal"),
  ...
)
```

## Arguments

- learner:

  character(1). Learner type. Should be one of:

  - "spatial": spatial cross-validation.

  - "temporal": temporal cross-validation.

  - "spatiotemporal": spatiotemporal cross-validation.

- ...:

  Additional arguments to be passed.

## Value

[`rsample::manual_rset()`](https://rsample.tidymodels.org/reference/manual_rset.html)
output object.

## Note

This function's returned value is used as an input for
`fit_base_brulee`, `fit_base_lightgbm`, and `fit_base_elnet`. Learner
values can be used as a branching point for the cross-validation
strategy.
