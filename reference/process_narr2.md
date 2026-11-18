# Process NARR Data (v2)

This function processes NARR2 data based on the specified parameters.

## Usage

``` r
process_narr2(
  date = c("2023-09-01", "2023-09-01"),
  variable = NULL,
  path = NULL,
  ...
)
```

## Arguments

- date:

  A character vector specifying the start and end dates in the format
  "YYYY-MM-DD".

- variable:

  A character vector specifying the variable of interest.

- path:

  A character vector specifying the path to the data files.

- ...:

  Additional parameters to be passed to other functions.

## Value

A SpatRaster object containing the processed NARR2 data.

## Details

This function performs the following steps:

1.  Sets up the directory path.

2.  Checks for null parameters.

3.  Identifies file paths based on the specified variable.

4.  Generates a date sequence based on the specified start and end
    dates.

5.  Filters the file paths to include only dates of interest.

6.  Sets up the search abbreviation and target variable.

7.  Imports and processes the data for each file path.

8.  Subsets the data to include only dates of interest.

9.  Returns the processed data as a SpatRaster object.

## Examples

``` r
# Process NARR2 data for the variable "PRATE" from
# September 1, 2023 to September 1, 2023
if (FALSE) { # \dontrun{
data <-
  process_narr2(
    date = c("2023-09-01", "2023-09-01"),
    variable = "PRATE",
    path = "/path/to/data"
  )
} # }
```
