# Combine base learner prediction values with testing data.

This function combines outcome data (observations) with the prediction
values from each base learner.

## Usage

``` r
attach_pred(
  pred = NULL,
  test = NULL,
  target_cols = c("site_id", "time", "lon", "lat"),
  yvar = "Arithmetic.Mean"
)
```

## Arguments

- pred:

  data.table(1). Predicted values from `fit_prediction`

- test:

  data.table(1). `data.table` with with testing dataset.

- target_cols:

  characters(1). Columns to retain from the full data.frame. DO NOT
  include `yvar` variable in the `target_cols`.

- yvar:

  character(1). Outcome variable name.

## Value

a data.frame object, including the target columns from `data` and the
predictions for each base learner.
