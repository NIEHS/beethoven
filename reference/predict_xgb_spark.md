# Predict with saved XGBoost fold models on Spark

Reads native `.ubj` booster files, predicts each fold on the supplied
data, and averages folds within each resampling repeat. Predictors are
assembled in the order retained during fitting. Results are written as
one Parquet dataset with one prediction column per repeat, named from
the model index.

## Usage

``` r
predict_xgb_spark(
  master,
  home,
  workspace,
  database = "duckdb",
  input = NULL,
  models,
  pred = NULL
)
```

## Arguments

- master, home, workspace, database:

  Spark connection and configuration arguments passed as in
  [`fit_xgb_spark()`](https://niehs.github.io/beethoven/reference/fit_xgb_spark.md).

- input:

  Character path or paths to prediction Parquet data. Data must contain
  `site_id`, `time`, `lon`, `lat`, and the saved predictor columns, with
  one row per `site_id` and `time`. No response column is required.
  Usable paths must be supplied despite the `NULL` default.

- models:

  The complete `model` list returned by
  [`fit_xgb_spark()`](https://niehs.github.io/beethoven/reference/fit_xgb_spark.md),
  supplied directly or read from a saved R object. Prediction uses
  `index` and the ordered `xvar` vector. The index must contain `rset`,
  `fold`, `prediction`, and `path` columns, with one native booster file
  per fold and one prediction-column name per repeat. Paths must be
  accessible to the Spark driver.

- pred:

  character(1). Required output Parquet directory, written with
  overwrite mode. A usable path must be supplied despite the `NULL`
  default. The directory must be accessible to Spark and R and separate
  from input data and model files.

## Value

A character vector of absolute `.parquet` part-file paths beneath
`pred`, suitable for saving as a workflow input to a subsequent stage.

## Details

Uses the same Spark and XGBoost JAR configuration as
[`fit_xgb_spark()`](https://niehs.github.io/beethoven/reference/fit_xgb_spark.md).
Native boosters are wrapped as Spark regression models without reading
Spark model metadata. Features are made dense to preserve zero values.
Prediction covers all input rows, regardless of `XGB_MAX_ROWS`.

All indexed folds within each repeat contribute equally to its mean
prediction. Repeat-specific tables are joined by `site_id`, `time`,
`lon`, and `lat`. The saved dataset contains these identifiers and one
prediction column per repeat, such as `base_001` or `meta_001`. The
Spark connection is disconnected on exit, including on error.

## See also

[`fit_xgb_spark()`](https://niehs.github.io/beethoven/reference/fit_xgb_spark.md),
[`assemble_xgb_features()`](https://niehs.github.io/beethoven/reference/assemble_xgb_features.md)
