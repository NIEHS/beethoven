# Injects geographic information into a data frame

This function injects geographic information into a data frame using the
`calc_geos_strict` function. The injected information includes latitude
and longitude coordinates based on the specified locations, a location
ID column, a window range, and a snapping option.

## Usage

``` r
inject_geos(locs, injection, ...)
```

## Arguments

- locs:

  A data frame containing the locations for which geographic information
  needs to be injected.

- injection:

  A list of additional arguments to be passed to the `calc_geos_strict`
  function.

- ...:

  Placeholders

## Value

A modified data frame with injected geographic information.
