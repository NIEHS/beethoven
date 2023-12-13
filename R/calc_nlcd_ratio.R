
#' Compute land cover classes ratio in circle buffers around points
#'
#' @param data_vect terra::SpatVector of points geometry
#' @param buf_radius numeric (non-negative) giving the
#' radius of buffer around points
#' @param year numeric giving the year of NLCD data used
#' @param nlcd_path character giving nlcd data path
#' @importFrom utils read.csv
#' @importFrom utils data
#' @import terra
#' @import sf
#' @import spData
#' @export
calc_nlcd_ratio <- function(data_vect,
                            buf_radius = 1000,
                            year = 2021,
                            nlcd_path) {
  # check inputs
  if (!is.numeric(buf_radius)) {
    stop("buf_radius is not a numeric.")
  }
  if (buf_radius <= 0) {
    stop("buf_radius has not a likely value.")
  }
  if (!is.numeric(year)) {
    stop("year is not a numeric.")
  }
  if (class(data_vect)[1] != "SpatVector") {
    stop("data_vect is not a terra::SpatVector.")
  }
  if (!is.character(nlcd_path)) {
    stop("nlcd_path is not a character.")
  }
  if (!file.exists(nlcd_path)) {
    stop("nlcd_path does not exist.")
  }
  # open nlcd file corresponding to the year
  nlcd_file <- list.files(nlcd_path,
                          pattern = paste0("nlcd_", year, "_.*.tif$"),
                          full.names = TRUE)
  if (length(nlcd_file) == 0) {
    stop("NLCD data not available for this year.")
  }
  nlcd <- terra::rast(nlcd_file)
  # select points within mainland US and reproject on nlcd crs if necessary
  # need spData library
  utils::data("us_states", package = "spData")
  us_main <- sf::st_union(get("us_states")) |>
    terra::vect() |>
    terra::project(y = terra::crs(data_vect))
  data_vect_b <- data_vect |>
    terra::intersect(x = us_main)
  if (!terra::same.crs(data_vect_b, nlcd)) {
    data_vect_b <- terra::project(data_vect_b, terra::crs(nlcd))
  }
  # create circle buffers with buf_radius
  bufs_pol <- terra::buffer(data_vect_b, width = buf_radius) |>
    sf::st_as_sf()
  # ratio of each nlcd class per buffer
  nlcd_at_bufs <- exactextractr::exact_extract(nlcd,
                                               sf::st_geometry(bufs_pol),
                                               fun = "frac",
                                               stack_apply = TRUE,
                                               progress = FALSE)
  # select only the columns of interest
  nlcd_at_bufs <- nlcd_at_bufs[names(nlcd_at_bufs)[grepl("frac_",
                                                         names(nlcd_at_bufs))]]
  # change column names
  fpath <- system.file("extdata", "nlcd_classes.csv", package = "NRTAPmodel")
  nlcd_classes <- utils::read.csv(fpath)
  nlcd_names <- names(nlcd_at_bufs)
  nlcd_names <- sub(pattern = "frac_", replacement = "", x = nlcd_names)
  nlcd_names <- as.numeric(nlcd_names)
  nlcd_names <- nlcd_classes[nlcd_classes$value %in% nlcd_names, c("class")]
  new_names <- sapply(
    nlcd_names,
    function(x) {
      paste0("frac_", x, "_", year, "_", buf_radius, "m")
    }
  )
  names(nlcd_at_bufs) <- new_names
  # merge data_vect with nlcd class fractions (and reproject)
  new_data_vect <- cbind(data_vect_b, nlcd_at_bufs)
  new_data_vect <- terra::project(new_data_vect, terra::crs(data_vect))
  return(new_data_vect)
}
