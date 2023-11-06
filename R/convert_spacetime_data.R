
#' Convert spatio-temporal object to a datatable with lon, lat, time, predictors
#' columns. It also returns the crs.
#'
#' @param stobj object containing space-time data. It can be a data.frame,
#' a data.table, an sf or sftime, a SpatVector or a SpatRastDataset.
#' @return a list with a "stdt" a data.table of locations identified by
#' lat, lon, time columns and "crs_dt" the crs of the data.
#' @author Eva Marques
#' @importFrom terra crs
#' @importFrom tidyr pivot_longer
#' @export
convert_stobj_to_stdt <- function(stobj) {
  
  format <- class(stobj)[[1]]
  if (!format %in%
  c("data.frame", "data.table", "sf",
  "sftime", "SpatVector", "SpatRasterDataset")) {
    stop("Error: stobj class not accepted")
  }

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
  }
  
  else if (format == "SpatVector") {
    if (!("time") %in% names(stobj)) {
      stop("stobj does not contain time column")
    }
    crs_dt <- terra::crs(stobj)
    stdf <- as.data.frame(stobj, geom = "XY")
    names(stdf)[names(stdf) == "x"] <- "lon"
    names(stdf)[names(stdf) == "y"] <- "lat"
    stdt <- data.table::as.data.table(stdf)
  }
  
  else if (format == "SpatRasterDataset") {
    crs_dt <- terra::crs(stobj)
    stdf <- as.data.frame(stobj[1], xy = TRUE)
    colnames(stdf)[1] <- "lon"
    colnames(stdf)[2] <- "lat"
    # -- tranform from wide to long format
    stdf <- stdf |>
      tidyr::pivot_longer(
        cols = seq(3, ncol(stdf)),
        names_to = "time",
        values_to = names(stobj)[1])
    
    for (var in seq(2, length(names(stobj)))){
      # test that the ts is identical to the ts of the 1st variable
      if (!(identical(names(stobj[var]), names(stobj[1])))) {
        stop("Error in SpatRastDataset: time series is different for at least 
             2 variables - or not ordered for one of these.")
      }
      df_var <- as.data.frame(stobj[var], xy = TRUE)
      # -- tranform from wide to long format
      df_var <- df_var |>
        tidyr::pivot_longer(
          cols = seq(3, ncol(df_var)),
          names_to = "time",
          values_to = names(stobj)[var])
      varname_original <- names(stobj)[var]
      stdf[, varname_original] <- df_var[, varname_original]
    }
    stdt <- data.table::as.data.table(stdf)
  }

  # sort stdt
  stdt <- data.table::setorder(stdt, lon, lat, time)
  stdt <- stdt[order(stdt, na.last = TRUE)]

  stdt_result <- list("stdt" = stdt, "crs_dt" = crs_dt)
  class(stdt_result) <- c("list", "stdt")
  return(stdt_result)
} 

