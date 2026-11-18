# Export preprocessed GeoTIFF file of one day MODIS product files

Export preprocessed GeoTIFF file of one day MODIS product files

## Usage

``` r
export_tif(path_in, product_code, pat, subdataset = "", dest = ".")
```

## Arguments

- path_in:

  character. File paths of MODIS product files on a day

- product_code:

  character(1). Product code. e.g., "VNP46A2".

- pat:

  character. Filter of `path_in`.

- subdataset:

  character. Subdatasets to merge. It is ignored when `product_code` is
  one of `"MOD06_L2"` or `"VNP46A2"`.

- dest:

  character(1). Directory path to export the result file.

## Value

A GeoTIFF file is saved to `{dest}/{product_code}_processed_{date}.tif`
and this path is returned.

## Author

Insang Song

## Examples

``` r
if (FALSE) { # \dontrun{
export_tif(c("A2018001.001.hdf", "A2018001.002.hdf"),
           product_code = "MOD11A1",
           pat = "A\\d{7,7}",
           subdataset = "16 day NDVI",
           dest = ".")
# mirai run
library(mirai)
basedir61 <- "/mnt/modis/raw/61"
product <- "MCD19A2"
paths <-
  list.files(
    file.path(basedir61, product),
    pattern = "*.*.hdf$",
    full.names = TRUE,
    recursive = TRUE
  )
pcode <- "MCD19A2"
pattern <- "A\\d{7,7}"
subdataset <- c("Optical_Depth_055")
dest = "mydir"

run_mirai <-
  mirai_map(
    paths,
    .f = export_tif,
    .args = list(
       product_code = product,
       pat = pattern,
       subdataset = subdataset,
       dest = dest
    )
  )
run_mirai[.progress]
} # }
```
