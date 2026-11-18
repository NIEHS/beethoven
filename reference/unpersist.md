# Release cached Spark tables

Calls Spark's `unpersist` method with blocking enabled for each table,
waiting for cached blocks to be removed before continuing.

## Usage

``` r
unpersist(tables)
```

## Arguments

- tables:

  List of Spark tables accepted by
  [`sparklyr::spark_dataframe()`](https://rdrr.io/pkg/sparklyr/man/spark_dataframe.html).

## Value

Invisibly, `NULL`. Called for its cache-release side effect.
