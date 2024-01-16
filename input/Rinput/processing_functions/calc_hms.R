################################################################################
# Date created: 2024-01-09
# Packages required: stringr
################################################################################

################################################################################
#' calc_hms: Calculate wildfire smoke plume binary covariates from NOAA HMS
#' Smoke Product.
#' @param date_start character(1). length of 10. Start date of downloaded data. 
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date of downloaded data. 
#' Format YYYY-MM-DD (ex. September 10, 2023 = "2023-09-10").
#' @param data SpatVector(1). SpatVector object produced by `process_hms`
#' containing density level-specific polygons.
#' @param density_level character(1). "Light", "Medium", or "Heavy".
#' @param sites
#' @param identifiier character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @author Mitchell Manware.
#' @return a data.frame object;
#' @importFrom terra vect
#' @importFrom terra project
#' @importFrom terra extract
#' @importFrom terra as.data.frame
#' @export
calc_hms <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-02",
    data,
    density_level = c("Light", "Medium", "Heavy"),
    sites,
    identifier = "site_id"
) {
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. sites as SpatVector
  sites_v <- terra::vect(
    sites,
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )
  sites_v <- terra::project(sites_v, "EPSG:4326")
  #### 4. site identifiers
  sites_id <- terra::as.data.frame(sites_v)
  #### 5. define date sequence
  date_sequence <- generate_date_sequence(
    date_start,
    date_end,
    sub_hyphen = TRUE
  )
  #### 6. empty data.frame for extracted values
  sites_extracted <- NULL
  for (d in seq_along(date_sequence)) {
    cat(paste0(
      "Extracting ",
      tolower(density_level),
      " smoke plume data for date: ",
      date_sequence[d],
      "...\n"))
    #### 7. subset to date
    data_date <- data[data$Date == date_sequence[d],]
    #### 8. check for DENSITY SPECFIIC polygons
    if (nrow(data_date) == 0) {
      cat(paste0(
        density_level,
        " smoke plume polygons absent for date: ",
        date_sequence[d],
        ". Returning 0 (NO SMOKE PRESENT).\n"))
    }
    #### 9. extract data at sites
    sites_extracted_date <- terra::extract(
      data_date,
      sites_v
    )
    #### 10. binary identifier (0 = NO SMOKE PRESENT; 1 = SMOKE PRESENT)
    sites_extracted_date$binary12 <- match(
      as.character(sites_extracted_date$Density),
      c(NA, density_level)
    )
    sites_extracted_date$binary01 <- sub(1, 0, sites_extracted_date$binary12)
    sites_extracted_date$binary01 <- sub(2, 1, sites_extracted_date$binary01)
    #### 11. combine with site identifier
    sites_extracted_date_id <- data.frame(cbind(
      sites_id,
      paste0(as.Date(
        date_sequence[d],
        format = "%Y%m%d")),
      as.numeric(sites_extracted_date$binary01)
    ))
    #### 12. set column names
    colnames(sites_extracted_date_id) <- c(
      identifier,
      "date",
      paste0(
        "OTH_HMSW",
        substr(density_level, 1, 1),
        "_0_00000")
    )
    sites_extracted <- rbind(sites_extracted, sites_extracted_date_id)
  }
  return(sites_extracted)
}
