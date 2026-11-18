# Injects GMTED data into specified locations

This function injects GMTED (Global Multi-resolution Terrain Elevation
Data) into specified locations. It calculates the GMTED values for each
location within different radii and returns the merged results.

## Usage

``` r
inject_gmted(locs, variable, radii, injection)
```

## Arguments

- locs:

  A data frame/sf/SpatVector containing the locations where GMTED
  variables needs to be calculated

- variable:

  The variable for which GMTED data needs to be calculated.

- radii:

  A vector of radii for which GMTED data needs to be calculated.

- injection:

  A list of additional arguments to be passed to the `calc_gmted_direct`
  function.

## Value

A data frame containing the merged results of GMTED data for each
location within different radii.
