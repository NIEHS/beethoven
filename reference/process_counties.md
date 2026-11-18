# Load county sf object

Load county sf object

## Usage

``` r
process_counties(
  year = 2020,
  exclude = c("02", "15", "60", "66", "68", "69", "72", "78")
)
```

## Arguments

- year:

  integer(1). Year of the county shapefile.

- exclude:

  character. State FIPS codes to exclude. Default is c("02", "15", "60",
  "66", "68", "69", "72", "78").

## Value

sf object
