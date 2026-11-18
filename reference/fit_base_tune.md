# Tune base learner

Tune base learner

## Usage

``` r
fit_base_tune(
  recipe,
  model,
  resample,
  tune_mode = c("bayes", "grid"),
  grid = NULL,
  iter_bayes = 10L,
  trim_resamples = TRUE,
  return_best = TRUE,
  workflow = TRUE,
  data_full = NULL,
  metric = "rmse"
)
```

## Arguments

- recipe:

  The recipe object.

- model:

  The model object.

- resample:

  The resample object. It is expected to be generated from the
  subsamples.

- tune_mode:

  character(1). Hyperparameter tuning mode. Default is "bayes", "grid"
  is acceptable.

- grid:

  The grid object for hyperparameter tuning.

- trim_resamples:

  logical(1). Default is TRUE, which replaces the actual data.frames in
  splits column of `tune_results` object with NA.

- return_best:

  logical(1). If TRUE, the best tuned model is returned.

- workflow:

  logical(1). If TRUE, the best fit model workflow is returned.

- data_full:

  The full data frame to be used for prediction.

- metric:

  character(1). The metric to be used for selecting the best. Must be
  one of "rmse", "rsq", "mae". Default = "rmse"

## Value

List of 3:

- `base_prediction`: `data.frame` of the best model prediction.

- `base_parameter`: `tune_results` object of the best model.

- `best_performance`: `data.frame` of the performance metrics. It
  includes RMSE, MAPE, R-squared, and MAE for **all** tuned models.
