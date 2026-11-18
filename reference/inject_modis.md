# Injects arguments into MODIS/VIIRS data processing function

Injects arguments into MODIS/VIIRS data processing function

## Usage

``` r
inject_modis(locs, injection)
```

## Arguments

- locs:

  A data frame containing the locations for which MODIS features need to
  be calculated.

- injection:

  **List** of dditional parameters to be passed to the
  `calculate_modis_par` function.

## Value

MODIS/VIIRS feature data.frame.

## Examples

``` r
if (FALSE) { # \dontrun{
files <-
  c(
    "/downloads/modis/mod06/MOD06_L2.A2022001.0000.061.2022001160000.hdf",
   "/downloads/modis/mod06/MOD06_L2.A2022001.0005.061.2022001160000.hdf"
  )
my_locs <- data.frame(site_id = 1:2, lon = c(-88, -87), lat = c(35, 35))
my_locs <- sf::st_as_sf(my_locs, coords = c("lon", "lat"))
inject_modis(
  locs = my_locs,
  injection = list(path = files, subdataset = "Cloud_Fraction_Day",
     name_covariates = "MOD_CLCVD_0_",
     preprocess = amadeus::process_modis_swath, radius = c(1000)))
} # }
```
