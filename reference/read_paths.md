# Read paths from a directory with a specific file extension

Read paths from a directory with a specific file extension

## Usage

``` r
read_paths(
  path,
  extension = ".hdf",
  target_dates = c("2020-01-01", "2020-01-15"),
  julian = FALSE
)
```

## Arguments

- path:

  The directory path from which to read the paths.

- extension:

  The file extension to match. Defaults to ".hdf".

- target_dates:

  A character vector of length 2 containing the start and end dates.

- julian:

  logical(1). If `TRUE`, the dates are in Julian format.

## Value

A character vector containing the full paths of the matching files.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read paths from a directory with default extension
read_paths("/path/to/directory")

# Read paths from a directory with custom extension
read_paths("/path/to/directory", ".txt")
} # }
```
