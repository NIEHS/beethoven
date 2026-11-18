# Identify MODIS files

This function identifies the relevant MODIS file paths based on path,
list of julian dates, and index. Designed to help set arguments for the
`inject_modis_par` function.

## Usage

``` r
query_modis_files(path, list, index)
```

## Arguments

- path:

  A character vector specifying the path to the MODIS data.

- list:

  A list of julian dates.

- index:

  An integer specifying the index of the julian date to use.

## Value

A character vector of MODIS file paths.
