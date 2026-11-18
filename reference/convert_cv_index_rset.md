# Generate manual rset object from spatiotemporal cross-validation indices

Generate manual rset object from spatiotemporal cross-validation indices

## Usage

``` r
convert_cv_index_rset(
  cvindex,
  data,
  ref_list = attr(cvindex, "ref_list"),
  cv_mode = c("spatiotemporal", "spatial", "temporal")
)
```

## Arguments

- cvindex:

  One of:

  - integer row indices for `id_out` in a `rset` object.

  - List of integer row indices stored in elements named `analysis` and
    `assessment`.

- data:

  data.frame object from which the `cvindex` is used to create `rset`
  object

- ref_list:

  List of custom reference group indices. Default is
  `attr(cvindex, "ref_list")`, where it is assumed that `cvindex`
  contains an `list` attribute named "ref_list". if not NULL, it will be
  used as a reference instead of max(cvindex).

- cv_mode:

  character(1). Spatiotemporal cross-validation indexing method label.

## Value

rset object of `rsample` package. A tibble with a list column of
training-test data.frames and a column of labels.

## Author

Insang Song
