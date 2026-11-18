# Perform Principal Component Analysis

Perform Principal Component Analysis

## Usage

``` r
reduce_pca(data, num_comp = 5, threshold = NA, kernel = FALSE)
```

## Arguments

- data:

  data.frame or data.table

- num_comp:

  integer(1). The number of components to retain as new predictors. If
  `threshold` is defined, `num_comp` will be overridden.

- threshold:

  numeric(1). A fraction of the total variance that should be covered by
  the components.

- kernel:

  logical(1). Whether to use a kernel PCA with
  [`recipes::step_kpca()`](https://recipes.tidymodels.org/reference/step_kpca.html).
  Default is `FALSE`.

## Value

data.table with Principal Components sufficient to satisfy the
`threshold`.\`

## See also

[`recipes::step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html)
[`recipes::step_kpca()`](https://recipes.tidymodels.org/reference/step_kpca.html)
