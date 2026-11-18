# Predict base learners at for cross validation folds.

Predict base learners at for cross validation folds.

## Usage

``` r
fit_prediction(
  fit = NULL,
  test = NULL,
  target_cols = c("site_id", "time", "lon", "lat"),
  name = NULL
)
```

## Arguments

- fit:

  list(1). List with 1) trained workflow and 2) performance metrics.
  Workflow must be first item in list.

- test:

  data.table(1). `data.table` with un-trained testing data.

- target_cols:

  characters(1). Columns to retain from the full test data
  \`data.frame“.

- name:

  character(1). Name for prediction column. Should identify the model
  and engine and/or hyperparameters.

## Value

`data.table` with attached site identifiers
