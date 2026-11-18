# Fit meta learner

This function subsets the full data by column subsamples (rate=50%) The
optimal hyperparameter search is performed based on spatiotemporal
cross-validation schemes. As of version 0.4.5, users can define metric
used for selecting best hyperparameter set (default = "rmse").

## Usage

``` r
fit_meta_learner(
  data,
  c_subsample = 0.5,
  r_subsample = 1,
  yvar = "Arithmetic.Mean",
  target_cols = c("site_id", "time", "lon", "lat", "Event.Type"),
  args_generate_cv = list(),
  tune_iter = 50L,
  trim_resamples = FALSE,
  return_best = TRUE,
  metric = "rmse"
)
```

## Arguments

- data:

  data.frame. Full data.frame of base learner predictions and AQS
  spatiotemporal identifiers.
  [`attach_pred`](https://niehs.github.io/beethoven/reference/attach_pred.md).

- c_subsample:

  numeric(1). Rate of column resampling. Default is 0.5.

- r_subsample:

  numeric(1). The proportion of rows to be used. Default is 1.0, which
  uses full dataset but setting is required to balance groups generated
  with
  [`make_subdata`](https://niehs.github.io/beethoven/reference/make_subdata.md)

- yvar:

  character(1). Outcome variable name

- target_cols:

  characters(1). Columns from `data` to be retained during column
  resampling. Default is c("site_id", "time", "Event.Type", "lon",
  "lat").

- args_generate_cv:

  List of arguments to be passed to `switch_generate_cv_rset` function.

- tune_iter:

  integer(1). Bayesian optimization iterations. Default is 50.

- trim_resamples:

  logical(1). Default is TRUE, which replaces the actual data.frames in
  splits column of `tune_results` object with NA. Passed to
  [`fit_base_tune`](https://niehs.github.io/beethoven/reference/fit_base_tune.md).

- return_best:

  logical(1). If TRUE, the best tuned model is returned. Passed to
  [`fit_base_tune`](https://niehs.github.io/beethoven/reference/fit_base_tune.md).

- metric:

  character(1). The metric to be used for selecting the best. Must be
  one of "rmse", "rsq", "mae". Default = "rmse". Passed to
  [`fit_base_tune`](https://niehs.github.io/beethoven/reference/fit_base_tune.md).

## Value

List of 3, including the best-fit model, the best hyperparameters, and
the all performance records from
[`tune::tune_bayes()`](https://tune.tidymodels.org/reference/tune_bayes.html).
Note that the meta learner function returns the best-fit model, not
predicted values.

## See also

[`fit_base_tune`](https://niehs.github.io/beethoven/reference/fit_base_tune.md)
[`make_subdata`](https://niehs.github.io/beethoven/reference/make_subdata.md)
