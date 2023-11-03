## A suite of functions to check input data
## Last edited 11/03/2023

#' Type check of internal class stdt
#' @param input data.frame or data.table
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
#' @param input_raster stars object
#' @param spatial_domain sf/sftime object of spatial domain
#' @param domain_tolerance numeric(1). Extrusion (in meters) from the bbox
#' of spatial_domain.
#' @return A numeric value reporting the extent of input_raster is
#' within the extent of spatial_domain
#' @author Kyle Messier, Insang Song
#' @export
check_input_raster_in_extent <- function(
    input_raster,
    spatial_domain,
    domain_tolerance = 3e5L) {
  input_extent <- input_raster |>
    sf::st_transform("EPSG:4326") |>
    sf::st_bbox() |>
    sf::st_as_sfc()

  domain_bbox <- spatial_domain |>
    sf::st_transform("EPSG:4326") |>
    sf::st_buffer(domain_tolerance) |>
    sf::st_bbox() |>
    sf::st_as_sfc()

  iswithin <- sf::st_covered_by(input_extent, domain_bbox)
  iswithin <- length(iswithin[[1]])

  return(iswithin)
}
