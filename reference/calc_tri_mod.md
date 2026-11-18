# Modified isotropic Sum of Exponentially Decaying Contributions (SEDC) covariates

Calculate toxic release values for polygons or isotropic buffer point
locations. Returns a `data.frame` object containing `locs_id` and
variables for each chemical in `from`.

## Usage

``` r
calc_tri_mod(from = NULL, locs, locs_id = "site_id", radius = 1000L, ...)
```

## Arguments

- from:

  SpatVector(1). Output of `process_tri()`.

- locs:

  sf/SpatVector. Locations where TRI variables are calculated.

- locs_id:

  character(1). Unique site identifier column name. Default is
  `"site_id"`.

- radius:

  integer(1). Circular buffer radius.

- ...:

  Placeholders.

## Value

a data.frame or SpatVector object

## Note

U.S. context.

## Author

Insang Song, Mariana Kassien
