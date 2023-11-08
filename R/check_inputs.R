## A suite of functions to check input data
## Last edited 11/06/2023

#' Type check of internal class stdt
#' @param input stdt object, which is a result of convert_stobj_to_stdt
#' @return logical(1). TRUE (stdt is in one of class names) or
#' FALSE (otherwise)
#' @author Insang Song
#' @description Detects the class whether it is subject to stdt.
#' @export
check_input_is_stdt <- function(
  input
) {
  input_class <- class(input)
  detected <- any(grepl("stdt", input_class))
  return(detected)
}


#' Check if the input raster is in the expected extent
#'
#' @param input_raster SpatRaster/stars object
#' @param spatial_domain sf/sftime/SpatVector object of spatial domain
#' @param domain_tolerance numeric(1). Extrusion (in meters) from the bbox
#' of spatial_domain.
#' @return A numeric value reporting the extent of input_raster is
#' within the extent of spatial_domain
#' @description \code{domain_tolerance} will have "meters" is converted to arc degrees approximately 100,000 meters = 1 arc degree
#' @author Kyle Messier, Insang Song
#' @import sf
#' @import terra
#' @import units
#' @export
check_input_raster_in_extent <- function(
    input_raster,
    spatial_domain,
    domain_tolerance = 3e5L) {
  
  if (methods::is(spatial_domain, "SpatVector")) {
    message("The spatial_domain is SpatVector. We will convert it to sf...\n")
    spatial_domain <- sf::st_as_sf(spatial_domain)
  }
  domain_tolerance_deg <- domain_tolerance / 1e5L

  input_extent <- input_raster |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_transform("EPSG:4326")

  domain_bbox <- spatial_domain |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_transform("EPSG:4326") |>
    sf::st_buffer(dist = domain_tolerance_deg)

  iswithin <- sf::st_covered_by(input_extent, domain_bbox)
  iswithin <- length(iswithin[[1]])

  return(iswithin)
}
