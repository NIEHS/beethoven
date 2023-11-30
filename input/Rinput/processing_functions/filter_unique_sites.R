## Date: 2023-11-24
## Description: Get the unique site locations and the WGS84 coordinates of those
check_installed_load <- function(lib) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

pkgs <- c("data.table", "sf", "terra")
invisible(suppressMessages(sapply(pkgs, check_installed_load)))
options(sf_use_s2 = FALSE)



#' Filter unique sites with or without temporal information
#' @param path_measurement character(1). Path to daily measurement data.
#' @param include_time logical(1). Should the output include
#'  temporal information?
#' @param date_start character(1). Start date.
#'  Should be in "YYYY-MM-DD" format.
#' @param date_end character(1). End date.
#'  Should be in "YYYY-MM-DD" format.
#' @returns data.table object with three or four fields.
#' - "site_id"
#' - "lon": in WGS 1984 (EPSG:4326)
#' - "lat": in WGS 1984 (EPSG:4326)
#' - "date"
#' "date" field will be attached only when include_time == TRUE.
#' @import data.table
#' @note \code{include_time = TRUE} will return a massive data.table
#' object. Please choose proper \code{date_start} and \code{date_end} values.
#' @export
filter_unique_sites <-
  function(
    path_measurement = "./tests/testdata/daily_88101_2018-2022.rds",
    path_stdt_functions = "./R/manipulate_spacetime_data.R",
    include_time = FALSE,
    date_start = "2018-01-01",
    date_end = "2022-12-31"
  ) {
    sites <- readRDS(path_measurement)
    # data manipulation functions
    source(path_stdt_functions)

    ## get unique sites
    ##
    sites$site_id <-
      sprintf("%02d%03d%04d%05d",
              sites$State.Code,
              sites$County.Code,
              sites$Site.Num,
              sites$Parameter.Code)

    # select relevant fields only
    sites_v <- unique(sites[, c("site_id", "Longitude", "Latitude", "Datum")])
    names(sites_v)[2:3] <- c("lon", "lat")
    sites_v <- as.data.table(sites_v)

    # subset mainland
    sites_v <- sites_v[!grepl("^(02|15|72|78|6)", site_id), ]

    # NAD83 to WGS84
    sites_v_nad <-
      project_dt(sites_v[Datum == "NAD83"],
                 "EPSG:4269", "EPSG:4326")
    sites_v_nad <- sites_v_nad[, c(3, 6, 5)]
    sites_v_wgs <- sites_v[Datum == "WGS84"][, -4]
    final_sites <- rbind(sites_v_wgs, sites_v_nad)

    if (include_time) {
      date_start <- as.Date(date_start)
      date_end <- as.Date(date_end)
      date_sequence <- seq(date_start, date_end, "day")
      final_sites <-
        split(date_sequence, date_sequence) |>
        lapply(function(x) {
          fs_time <- final_sites
          fs_time$date <- x
          return(fs_time)
        })
      final_sites <- Reduce(rbind, final_sites)
    }

    return(final_sites)
}
# File ends