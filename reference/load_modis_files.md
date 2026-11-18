# Load MODIS files from a specified path.

This function takes a path and an optional pattern as input and returns
a list of MODIS files found in the specified path.

## Usage

``` r
load_modis_files(path, pattern = "hdf$", date = character(2))
```

## Arguments

- path:

  The path where the MODIS files are located.

- pattern:

  An optional regular expression pattern to filter the files. The
  default pattern is "hdf\$".

- date:

  A vector of two dates to filter the files by. The default is an empty
  character vector.

## Value

A list of full file names of the MODIS files found in the specified
path.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load MODIS files from the current directory
modis_files <-
  load_modis_files(
    ".",
    date = c("2018-01-01", "2018-01-31")
  )

# Load MODIS files from a specific directory with a custom pattern
modis_files <-
  load_modis_files(
    "/path/to/files",
    pattern = "MOD.*hdf$",
    date = c("2018-01-01", "2018-01-31")
  )
} # }
```
