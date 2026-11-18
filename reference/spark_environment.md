# Read Spark and XGBoost settings from environment variables

Collects configuration for
[`fit_xgb_spark()`](https://niehs.github.io/beethoven/reference/fit_xgb_spark.md)
and
[`predict_xgb_spark()`](https://niehs.github.io/beethoven/reference/predict_xgb_spark.md)
and checks that worker and thread counts fit within the available cores.
Settings are returned in a list for explicit use by the caller.

## Usage

``` r
spark_environment(workspace = Sys.getenv("HOME"), database = "sedonadb")
```

## Arguments

- workspace:

  character(1). Base directory for default warehouse and results paths.
  Defaults to the `HOME` environment variable.

- database:

  character(1). Default database name, overridden by `XGB_DATABASE`.
  Currently retained as configuration metadata only.

## Value

Invisibly, a named list with elements `chr_workdir`, `int_maxcores`,
`int_executorcores`, `int_executormem` (a character memory
specification), `int_partitions`, `int_nround`, `int_seed`,
`int_maxrows`, `int_maxfeatures`, `chr_database`, `chr_xgboost_package`,
`chr_resultspath`, `int_nworkers`, `int_nthreads`, and `chr_warehouse`.

## Details

The following environment variables control configuration:

- `SPARK_CORES_MAX`: maximum cores; defaults to 64.

- `SPARK_EXECUTOR_CORES`: cores per executor; defaults to 2.

- `SPARK_EXECUTOR_MEMORY`: memory per executor; defaults to `"2g"`.

- `SPARK_SHUFFLE_PARTITIONS`: shuffle partitions; defaults to 32.

- `XGB_NUM_ROUND`: boosting rounds; defaults to 50.

- `XGB_SEED`: nonnegative random seed; defaults to 20260904.

- `XGB_MAX_ROWS`: nonnegative training-row limit; 0 means no limit.
  [`predict_xgb_spark()`](https://niehs.github.io/beethoven/reference/predict_xgb_spark.md)
  overrides this setting to process all input rows.

- `XGB_NUM_WORKERS`: workers; defaults to maximum cores divided by cores
  per executor, rounded down, with a minimum of 1.

- `XGB_NTHREAD`: threads per worker; defaults to cores per executor.

- `XGB_WAREHOUSE_DIR`: warehouse directory; defaults to
  `file.path(workspace, "spark-warehouse")`.

- `XGBOOST_MAVEN_PACKAGE`: Maven coordinate; defaults to
  `"ml.dmlc:xgboost4j-spark_2.13:3.4.0"`.

- `XGB_DATABASE`: database name; defaults to `database`.

- `XGB_MAX_FEATURES`: nonnegative feature limit; defaults to 0. This
  setting is retained but is not currently applied by
  [`fit_xgb_spark()`](https://niehs.github.io/beethoven/reference/fit_xgb_spark.md).

- `XGB_RESULTS_PATH`: results path; defaults to
  `file.path(workspace, "xgboost-results.rds")`. This setting is
  retained but
  [`fit_xgb_spark()`](https://niehs.github.io/beethoven/reference/fit_xgb_spark.md)
  returns metrics without writing an RDS file.

Integer settings have a minimum of 1 unless described as nonnegative. An
error is raised if executor cores exceed maximum cores, threads per
worker exceed executor cores, or workers times threads exceed maximum
cores.

## See also

[`env_integer()`](https://niehs.github.io/beethoven/reference/env_integer.md)
