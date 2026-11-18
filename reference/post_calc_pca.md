# Post-calculation Principal Component Analysis

This function performs PCA on the input data frame to reduce number of
predictors.

## Usage

``` r
post_calc_pca(
  data,
  locs_id = "site_id",
  time_id = "time",
  yvar = "Arithmetic.Mean",
  coords = c("lon", "lat"),
  num_comp = 5,
  threshold = NA,
  pattern = "FUGITIVE|STACK",
  groups = NULL,
  prefix = "PCA",
  kernel = FALSE
)
```

## Arguments

- data:

  data.frame or data.table

- locs_id:

  The column name in the spatial object that represents the location
  identifier.

- time_id:

  The column name in the data frame that represents the time identifier.

- yvar:

  The target variable.

- coords:

  The column names that represent the XY coordinates. Default is
  `c("lon", "lat")`.

- num_comp:

  integer(1). The number of components to retain as new predictors. If
  `threshold` is defined, `num_comp` will be overridden.

- threshold:

  numeric(1). A fraction of the total variance that should be covered by
  the components.

- pattern:

  character(1). A regular expression pattern to match the columns that
  should be included in the PCA.

- groups:

  character. A character vector of groups to perform PCA on. Each
  character should be a regular expression pattern to match the columns
  that should be included in the PCA. Default is `NULL`.

- prefix:

  character(1). A prefix to be added to the column names of the
  Principal Components. Default is `NULL`.

- kernel:

  logical(1). Whether to use a kernel PCA with
  [`recipes::step_kpca()`](https://recipes.tidymodels.org/reference/step_kpca.html).
  Default is `FALSE`.

## Value

data.table with Principal Components sufficient to satisfy the
`threshold`, merged with `*_id` and `yvar` columns from original `data`.

## Note

If `threshold` is defined, `num_comp` will be overridden.

## See also

[`reduce_pca()`](https://niehs.github.io/beethoven/reference/reduce_pca.md)
[`recipes::step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html)
[`recipes::step_kpca()`](https://recipes.tidymodels.org/reference/step_kpca.html)
