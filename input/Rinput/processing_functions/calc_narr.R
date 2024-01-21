################################################################################
# Date created: 2023-11-28
# Packages required: stringr
################################################################################

################################################################################
#' calc_narr: Calculate meteorological covariates from NOAA NCEP North American
#' Regional Reanalysis.
#' @param year_start integer(1). length of 4. Start of year range for
#' downloading data.
#' @param year_end integer(1). length of 4. End of year range for downloading
#' data.
#' @param month_start integer(1). 1 = January; 12 = December
#' @param month_end integer(1). 1 = January; 12 = December
#' @param variable character(1).
#' @param sites
#' @param identifiier character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param level integer(1). Pressure level of data.
#' @param directory_with_data character(1). Directory with downloaded NARR
#' monolevel netCDF files.
#' @author Mitchell Manware
#' @return a data.frame object
#' @export
calc_narr <- function(
    year_start = 2018,
    year_end = 2022,
    month_start = NULL,
    month_end = NULL,
    variable = NULL,
    sites,
    identifier = "site_id",
    level = NULL,
    directory_with_data = "../../data/covariates/narr/") {
  #### 1. directory setup
  chars_dir_data <- nchar(directory_with_data)
  if (substr(directory_with_data, chars_dir_data, chars_dir_data) != "/") {
    directory_with_data <- paste0(directory_with_data, "/", sep = "")
  }
  #### 2. assign variable code
  code <- gsub("\\.", "", variable)
  code <- gsub("_", "", code)
  code <- substr(toupper(paste0(code, code)), 1, 5)
  #### 3. NARR-specific coordinate reference system
  crs_narr <- paste0("PROJCRS[\"unnamed\",\n
                     BASEGEOGCRS[\"WGS 84\",\n
                     DATUM[\"World Geodetic System 1984\",\n
                     ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n
                     LENGTHUNIT[\"metre\",1]]],\n
                     PRIMEM[\"Greenwich\",0,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433]],\n
                     ID[\"EPSG\",4326]],\n
                     CONVERSION[\"unnamed\",\n
                     METHOD[\"Lambert Conic Conformal (2SP)\",\n
                     ID[\"EPSG\",9802]],\n
                     PARAMETER[\"Latitude of false origin\",50,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433],\n
                     ID[\"EPSG\",8821]],\n
                     PARAMETER[\"Longitude of false origin\",-107,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433],\n
                     ID[\"EPSG\",8822]],\n
                     PARAMETER[\"Latitude of 1st standard parallel\",50,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433],\n
                     ID[\"EPSG\",8823]],\n
                     PARAMETER[\"Latitude of 2nd standard parallel\",50,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433],\n
                     ID[\"EPSG\",8824]],\n
                     PARAMETER[\"Easting at false origin\",5632642.22547,\n
                     LENGTHUNIT[\"metre\",1],\n
                     ID[\"EPSG\",8826]],\n
                     PARAMETER[\"Northing at false origin\",4612545.65137,\n
                     LENGTHUNIT[\"metre\",1],\n
                     ID[\"EPSG\",8827]]],\n
                     CS[Cartesian,2],\n
                     AXIS[\"easting\",east,\n
                     ORDER[1],\n
                     LENGTHUNIT[\"metre\",1,\n
                     ID[\"EPSG\",9001]]],\n
                     AXIS[\"northing\",north,\n
                     ORDER[2],\n
                     LENGTHUNIT[\"metre\",1,\n
                     ID[\"EPSG\",9001]]]]")
  #### 4. sites as SpatVector
  sites_v <- terra::vect(sites,
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )
  sites_v <- terra::project(sites_v, crs_narr)
  #### 5. site identifiers
  sites_id <- terra::as.data.frame(sites_v)
  #### 6. define year or year+month sequence
  sequence <- NULL
  years <- seq(year_start, year_end, 1)
  if (!is.null(c(month_start, month_end))) {
    months <- seq(month_start, month_end, 1)
    for (y in seq_along(years)) {
      year <- years[y]
      for (m in seq_along(months)) {
        year_month <- paste0(
          year,
          stringr::str_pad(months[m],
            width = 2,
            pad = "0",
            side = "left"
          )
        )
        sequence <- c(sequence, year_month)
      }
    }
  } else {
    sequence <- c(sequence, years)
  }
  #### 7. define file paths
  paths <- paste0(
    directory_with_data,
    variable,
    "/",
    variable,
    ".",
    sequence,
    ".nc"
  )
  #### 8. empty data.frame for extracted values
  sites_extracted_df <- NULL
  #### 9. import SpatRaster data
  for (p in seq_along(paths)) {
    data_ym <- terra::rast(paths[p])
    #### 10. subset to pressure level if not null
    if (!is.null(level)) {
      data_ym <- terra::subset(data_ym,
        subset = grep(
          paste0(
            "level=",
            level
          ),
          names(data_ym)
        )
      )
    }
    #### 11. define varname based on variable and date
    names(data_ym) <- paste0(
      variable,
      "_",
      gsub(
        "-",
        "",
        as.Date(terra::time(data_ym),
          format = "%Y%d%m"
        )
      )
    )
    for (l in seq_len(terra::nlyr(data_ym))) {
      #### 12. store data date
      date <- as.Date(terra::time(data_ym[[l]]),
        format = "%Y%m%d"
      )
      cat(paste0("Extracting ", variable, " data for date: ", date, "\n"))
      #### 13. extract data
      sites_extracted_date <- terra::extract(data_ym[[l]],
        sites_v,
        method = "simple",
        ID = FALSE,
        bind = FALSE
      )
      #### 14. combine with identifier
      sites_extracted_date <- data.frame(cbind(
        sites_id,
        paste0(date),
        sites_extracted_date
      ))
      colnames(sites_extracted_date) <- c(
        identifier,
        "date",
        paste0(
          "MET_",
          code,
          "_0_00000"
        )
      )
      #### 15. replace missing data flag with NA
      sites_extracted_date[, 3] <-
        replace(
          sites_extracted_date[, 3],
          sites_extracted_date[, 3] == 9.96921e+36,
          NA
        )
      #### 16. combine with locations data.frame
      sites_extracted_df <- rbind(sites_extracted_df, sites_extracted_date)
    }
  }
  #### 17. return data frame with extracted values
  return(sites_extracted_df)
}
