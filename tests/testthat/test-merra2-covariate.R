#' @author Kyle P Messier, Insang Song
#' @description
#' unit testing for MERRA2 covariates
#' test that the covariate calculation does not have unexpected NA
#' 
#' 
#'
test_that("MERRA2 covariate is not NA", {
  library(terra)
  library(dplyr)
  
  aqs.sftime <- terra::vect("../testdata/aqs-test-data.gpkg")
  
  merra = terra::rast("../testdata/merra2_tavg2_US_mainland_20220820_daily.tif")

  extract_with_buffer.flat <- function(
          points, surf, radius, id, qsegs, func = mean, kernel = NULL, bandwidth = NULL
      ) {
      # generate buffers
      bufs = terra::buffer(points, width = radius, quadsegs = qsegs)
      # crop raster
      bufs_extent = terra::ext(bufs)
      surf_cropped = terra::crop(surf, bufs_extent)
      name_surf_val = names(surf)
      # extract raster values
      surf_at_bufs = terra::extract(surf_cropped, bufs)
      surf_at_bufs_summary = 
          surf_at_bufs |> 
              group_by(ID) |> 
              summarize(across(all_of(name_surf_val), ~mean(.x, na.rm = TRUE))) |> 
              ungroup()
      return(surf_at_bufs_summary)
  }

  MERRA2.var <- extract_with_buffer.flat(aqs.sftime, merra, 2e4L, "ID.Code", 90L)

  expect_equal(any(is.na(as.vector(MERRA2.var))), FALSE)
})
