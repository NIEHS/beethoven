#' Convert spatio-temporal object to a datatable with lon, lat, time, predictors
#' columns. It also returns the crs.
#'
#' @param stobj object containing space-time data. It can be a data.frame,
#' a data.table, an sf or sftime, a SpatVector or a SpatRastDataset.
#' @return a list with a "stdt" a data.table of locations identified by
#' lat, lon, time columns and "crs_dt" the crs of the data.
#' @import data.table
#' @importFrom terra crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_crs
#' @author Eva Marques, Insang Song
#' @export
convert_stobj_to_stdt <- function(stobj) {

  format <- class(stobj)[[1]]

  if (format == "data.frame" || format == "data.table") {
    if (any(!(c("lon", "lat", "time") %in% colnames(stobj)))) {
      stop("Error: stobj does not contain lon, lat, time columns")
    }
    stdt <- data.table::as.data.table(stobj)
    crs_dt <- NA
  } 
  else if (format == "sf" || format == "sftime") {
    if (any(!(c("geometry", "time") %in% colnames(stobj)))) {
      stop("Error: stobj does not contain geometry and time columns")
    }
    crs_dt <- as.character(sf::st_crs(stobj))[1]
    stobj[, c("lon", "lat")] <- sf::st_coordinates(stobj)
    stobj <- sf::st_drop_geometry(stobj)
    stdt <- data.table::as.data.table(stobj)
  } else if (format == "SpatVector") {
    if (!("time") %in% names(stobj)) {
      stop("stobj does not contain time column")
    }
    crs_dt <- terra::crs(stobj)
    crs_dt <- terra::crs(stobj)
    stdf <- as.data.frame(stobj, geom = "XY")
    names(stdf)[names(stdf) == "x"] <- "lon"
    names(stdf)[names(stdf) == "y"] <- "lat"
    stdt <- data.table::as.data.table(stdf)
  } else if (format == "SpatRasterDataset") {
    crs_dt <- terra::crs(stobj)
    stdf <- as.data.frame(stobj[1], xy = TRUE)
    colnames(stdf)[1] <- "lon"
    colnames(stdf)[2] <- "lat"
    # -- tranform from wide to long format
    stdf <- stdf |>
      data.table::as.data.table() |>
      data.table::melt(
        measure.vars = names(stdf)[-1:-2],
        variable.name = "time",
        value.name = names(stobj)[1]
      )

    for (var in seq(2, length(names(stobj)))){
      # test that the ts is identical to the ts of the 1st variable
      if (!(identical(names(stobj[var]), names(stobj[1])))) {
        stop("Error in SpatRastDataset: 
        time series is different for at least 
             2 variables - or not ordered for one of these.")
      }

      varname_original <- names(stobj)[var]
      df_var <- as.data.frame(stobj[var], xy = TRUE)
      # -- tranform from wide to long format
      df_var <- df_var |>
        data.table::as.data.table() |>
        data.table::melt(
          measure.vars = names(df_var)[-1:-2],
          variable.name = "time",
          value.name = varname_original) |>
        as.data.frame()
      stdf[, varname_original] <- df_var[, 4]
    }
    stdt <- data.table::as.data.table(stdf)
  } else {
    stop("Error: stobj class not accepted")
  }

  # sort stdt
  stdt <- data.table::setorderv(stdt, cols = c("lon", "lat", "time"))
  stdt <- stdt[order(stdt, na.last = TRUE)]

  stdt_result <- list("stdt" = stdt, "crs_dt" = crs_dt)
  class(stdt_result) <- c("list", "stdt")
  return(stdt_result)

}
