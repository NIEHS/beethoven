# Generate temporal cross-validation index

Generate temporal cross-validation index

## Usage

``` r
generate_cv_index_ts(data, time_col = "time", cv_fold = 10L, window = 14L)
```

## Arguments

- data:

  data.table with X, Y, and time information.

- time_col:

  character(1). Field name with time information.

- cv_fold:

  integer(1). Number of cross-validation folds.

- window:

  integer(1). Window size for each fold. Simply meaning overlaps between
  folds. Unit is the base unit of temporal values stored in `time_col`.
  Window size is put into `as.difftime` function, then the half of it
  (if odd, rounded number + 1 is applied) is used for overlaps in the
  middle folds.

## Value

List of numeric vector with out-of-sample indices.

## Examples

``` r
data <- data.frame(
 time = seq.Date(from = as.Date("2021-01-01"), by = "day", length.out = 100),
 value = rnorm(100)
)
rset_ts <-
  generate_cv_index_ts(data, time_col = "time", cv_fold = 10, window = 14)
```
