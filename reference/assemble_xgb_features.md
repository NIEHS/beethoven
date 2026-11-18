# Assemble dense Spark feature vectors for XGBoost

Projects an existing Spark table, aliases an optional response to
`label`, and assembles the supplied predictors into a dense `features`
vector in their supplied order. Identification columns are retained
alongside the vectors.

## Usage

``` r
assemble_xgb_features(
  sc,
  chr_feat_merge_sptmpl,
  chr_feat_merge_xvar,
  yvar = NULL,
  config
)
```

## Arguments

- sc:

  A Spark connection created by
  [`sparklyr::spark_connect()`](https://rdrr.io/pkg/sparklyr/man/spark-connections.html).

- chr_feat_merge_sptmpl:

  character(1). Name of a table already registered in Spark. It must
  contain `site_id`, `time`, `lon`, `lat`, any supplied response, and
  the supplied numeric predictor columns.

- chr_feat_merge_xvar:

  Character vector of numeric predictor names, selected by the caller.
  Their order determines the positions in the feature vector and must
  match the order used to train the model.

- yvar:

  Optional character(1) response column name, aliased to `label`. Use
  `NULL` when assembling prediction data without a response.

- config:

  Configuration list, such as that returned by
  [`spark_environment()`](https://niehs.github.io/beethoven/reference/spark_environment.md),
  containing `int_maxrows`. A value of 0 applies no row limit; a
  positive value applies SQL `LIMIT` without an ordering clause.

## Value

A registered Spark table with columns `site_id`, `time`, `lon`, `lat`,
and `features`, plus `label` when `yvar` is supplied. The `features`
column contains dense vectors with one entry per supplied predictor, in
the supplied order.

## Details

Predictors are temporarily renamed to `feature_0001`, `feature_0002`,
and so on before vector assembly. Vectors are converted to dense form
through an array so zero values remain explicit when passed to XGBoost.

## See also

[`fit_xgb_spark()`](https://niehs.github.io/beethoven/reference/fit_xgb_spark.md),
[`predict_xgb_spark()`](https://niehs.github.io/beethoven/reference/predict_xgb_spark.md),
[`spark_environment()`](https://niehs.github.io/beethoven/reference/spark_environment.md)
