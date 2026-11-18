# Impute missing values and attach lagged features

Impute missing values and attach lagged features

## Usage

``` r
impute_all(
  dt,
  period,
  nthreads_dt = 32L,
  nthreads_collapse = 32L,
  nthreads_imputation = 32L
)
```

## Arguments

- dt:

  The input data table to be imputed.

- period:

  The period for lagged features.

- nthreads_dt:

  The number of threads to be used for data.table operations.

- nthreads_collapse:

  The number of threads to be used for collapse operations.

- nthreads_imputation:

  The number of threads to be used for the imputation process.

## Value

The imputed data table with lagged features.

## Note

This function performs imputation on a given data table by replacing
missing values with imputed values. It follows a series of steps
including data cleaning, name cleaning, geoscf column renaming, NDVI
16-day backward filling, zero-variance exclusion, excessive "true zeros"
exclusion, and imputation using missRanger. A few points should be
discussed to sophisticate the imputation process: exclusion threshold
for rates of zero observations, which might lead to significant
improvement in the imputation process especially in terms of speed and
accuracy.
