# Attach XY coordinates to a data frame

This function attaches XY coordinates to a data frame based on a spatial
object containing the coordinates. It performs a left join operation to
match the coordinates with the corresponding locations in the data
frame.

## Usage

``` r
attach_xy(data_full, data_sf, locs_id = "site_id", time_id = "time")
```

## Arguments

- data_full:

  The full data frame to which XY coordinates will be attached.

- data_sf:

  The spatial object containing the XY coordinates.

- locs_id:

  The column name in the spatial object that represents the location
  identifier.

- time_id:

  The column name in the data frame that represents the time identifier.

## Value

A data frame with the XY coordinates attached.
