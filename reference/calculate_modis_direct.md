# Calculate MODIS from preprocessed file

Calculate MODIS from preprocessed file

## Usage

``` r
calculate_modis_direct(file, site, site_id, radius, colheader, mark = TRUE)
```

## Arguments

- file:

  character(1). File path of preprocessed GeoTIFF file.

- site:

  data.frame/sf. AQS sites. data.frame will be converted to sf.

- site_id:

  character(1). Unique identifier of `site`

- radius:

  numeric(1). Circular buffer radius. Distance unit is inherited from
  the sites' coordinate system.

- colheader:

  character. Column name header.

- mark:

  logical(1). Mark zero-padded (length of 5) radius in the column name.
