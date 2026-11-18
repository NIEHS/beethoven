# Parallelize NARR feature calculation

This function parallelizes the processing and calculation of NARR data
for multiple domains.

## Usage

``` r
par_narr(domain, path, date, locs)
```

## Arguments

- domain:

  A character vector specifying the domains to process.

- path:

  A character vector specifying the path to the NARR data.

- date:

  A character vector specifying the date of the NARR data to process.

- locs:

  A data frame specifying the locations to calculate NARR data for.

## Value

A list of results from the parallel processing.
