# Evaluate Spark regression predictions

Uses Spark's regression evaluator to collect metrics from `label` and
`prediction` columns without collecting individual predictions in R.

## Usage

``` r
evaluate(sc, r, f, pred, metric = c("rmse", "mse", "r2", "mae"))
```

## Arguments

- sc:

  A Spark connection created by
  [`sparklyr::spark_connect()`](https://rdrr.io/pkg/sparklyr/man/spark-connections.html).

- r:

  Resample identifier, typically a zero-padded character string.

- f:

  Fold identifier, typically a zero-padded character string.

- pred:

  A Spark table containing numeric `label` and `prediction` columns.

- metric:

  Character vector of metrics to compute. Supported values are `"rmse"`
  (root mean squared error), `"mse"` (mean squared error), `"r2"`
  (coefficient of determination), `"mae"` (mean absolute error), and
  `"var"` (explained variance). Defaults to the first four.

## Value

A data frame with one row per requested metric and columns `rset`,
`fold`, `metric`, and `value`, in the requested metric order.
