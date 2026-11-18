# Fit and evaluate XGBoost regression models on Spark

Reads Parquet data into Spark, assembles numeric predictors, and fits
one XGBoost regression model per fold of each resampling repeat. Each
native booster is saved as a `.ubj` file. Out-of-fold predictions are
combined into one Parquet dataset, and evaluation metrics and a model
index are returned in R.

## Usage

``` r
fit_xgb_spark(
  master,
  home,
  workspace,
  database = "duckdb",
  input = NULL,
  pred,
  model,
  rset,
  xvar,
  yvar,
  dvar,
  stage = "base",
  ...
)
```

## Arguments

- master:

  character(1). Spark master URL or local master specification, passed
  to
  [`sparklyr::spark_connect()`](https://rdrr.io/pkg/sparklyr/man/spark-connections.html).

- home:

  character(1). Spark installation directory, passed as `spark_home` to
  [`sparklyr::spark_connect()`](https://rdrr.io/pkg/sparklyr/man/spark-connections.html).

- workspace:

  character(1). Base directory for default Spark paths.

- database:

  character(1). Default database name passed to
  [`spark_environment()`](https://niehs.github.io/beethoven/reference/spark_environment.md).
  Currently retained as configuration metadata only.

- input:

  Character path or paths to input Parquet data, passed to
  [`sparklyr::spark_read_parquet()`](https://rdrr.io/pkg/sparklyr/man/spark_read_parquet.html).
  A usable path must be supplied despite the `NULL` default. Data must
  contain `site_id`, `time`, `lon`, `lat`, the response, and numeric
  predictor columns.

- pred:

  character(1). Directory for the combined out-of-fold prediction
  dataset, written with overwrite mode. It must be accessible to Spark
  and the local R process for returned file discovery.

- model:

  character(1). Directory for native booster files named
  `rset_NNN_fold_NN.ubj`. It is created if needed and must be accessible
  to the Spark driver and later prediction jobs. Keep it separate from
  `pred` so overwriting prediction data does not remove saved models.

- rset:

  Nested list of resampling repeats and folds. Each `rset[[r]][[f]]`
  must contain `analysis` and `assessment` vectors of `site_id` values
  used for training and testing, respectively. These are site
  identifiers, not row positions or `rsample` split objects.

- xvar:

  Character vector of predictor names. Must be identical, including
  order, to the numeric columns in the Spark schema after excluding
  `yvar` and `dvar`.

- yvar:

  character(1). Response column name, aliased to `label` in Spark.

- dvar:

  Character vector of columns excluded from predictor discovery,
  typically site identifiers and time columns.

- stage:

  character(1). Prefix for prediction columns, which are named using
  `sprintf("%s_%03d", stage, r)` for repeat `r`. Use `"base"` or
  `"meta"` to distinguish the two learner stages.

- ...:

  Reserved for future use. Currently ignored.

## Value

A list with:

- `pred`: character vector of `.parquet` paths beneath `pred` that were
  not present before writing. Paths with unchanged names are excluded.

- `eval`: data frame with `rset` and `fold` identifiers, `metric` and
  `value` columns for RMSE, MSE, R-squared, and MAE, and `time`, the
  elapsed fitting time in seconds repeated for each metric within a
  fold.

- `model`: list containing `index`, a data frame with integer `rset` and
  `fold`, character `prediction`, and absolute native-booster `path`
  columns; `xvar`, the ordered predictor names; `yvar`, the response
  name; and `stage`, the prediction prefix. The caller can save this
  list for later reuse.

## Details

Requires the sparklyr and DBI packages, a working Spark installation,
and compatible XGBoost4J-Spark JARs. A nonempty `XGBOOST_JARS`
environment variable supplies JAR paths through `sparklyr.shell.jars`;
otherwise the Maven coordinate from
[`spark_environment()`](https://niehs.github.io/beethoven/reference/spark_environment.md)
is used. See that helper for resource limits, boosting rounds, seed,
warehouse path, and row-limit settings. A positive row limit uses SQL
`LIMIT` without an ordering clause.

Models use CPU histogram training with squared-error loss, maximum depth
8, learning rate 0.1, and row and column sampling fractions of 0.8.
Feature vectors are made dense so zero values remain explicit. Each
fold's native booster is saved immediately after fitting, before
held-out prediction. The Spark connection is disconnected on exit,
including on error.

Within each repeat, held-out predictions are row-bound across folds.
Repeats are then left-joined by `site_id` and `time`, retaining
identifiers and the response from the first repeat. The dataset beneath
`pred` contains `site_id`, `time`, `lon`, `lat`, the response under its
original name, and one prediction column per repeat. It can contain
multiple Parquet part files.

The returned model metadata retains the predictor order and maps each
repeat and fold to its saved booster path and prediction-column name.
Pass this complete `model` list to
[`predict_xgb_spark()`](https://niehs.github.io/beethoven/reference/predict_xgb_spark.md)
for subsequent prediction.

## See also

[`spark_environment()`](https://niehs.github.io/beethoven/reference/spark_environment.md),
[`assemble_xgb_features()`](https://niehs.github.io/beethoven/reference/assemble_xgb_features.md),
[`predict_xgb_spark()`](https://niehs.github.io/beethoven/reference/predict_xgb_spark.md)
