################################################################################
# Title: Function to join point receptors to source polygons
# Date modified: 2023-08-17
## Added roxygen documentation, 2023-08-29
# Script description: Contains function to join point receptors to polygons
#   sources. Required function arguments are receptor point data and polygon
#   source data as simple features with respective unique identifying codes (id). 
#   Function returns a data frame containing a crosswalk of point receptor id
#   to source polygon id.
# Packages required: logr, sf, tidyverse
################################################################################

################################################################################
# Function: join_point_receptor_to_polygon_source
################################################################################
#' join_point_receptor_to_polygon_source
#'
#' @param receptor_sf 
#' @param receptor_id 
#' @param source_polygon_sf 
#' @param source_polygon_id 
#' @param add_all_to_output 
#' @param print_log_to_console 
#' @param write_log_to_file 
#'
#' @return a data frame: receptor_point_to_source_polygon_crosswalk 
#' @export
#'
#' @examples
join_point_receptor_to_polygon_source <- function(receptor_sf = NULL,
                                                  receptor_id = NULL,
                                                  source_polygon_sf = NULL,
                                                  source_polygon_id = NULL,
                                                  add_all_to_output = TRUE,
                                                  print_log_to_console = TRUE, 
                                                  write_log_to_file = TRUE) {

  # Check input arguments format ----------------------------------------------
  if(is.null(receptor_sf)) {
    stop("Required argument 'receptor_sf' is missing.")
  }
  if(("sf" %in% class(receptor_sf)) == FALSE) {
    stop("Required argument 'receptor_sf' must be in simple features format.")
  }
  
  if(is.null(receptor_id)) {
    stop("Required argument 'receptor_id' is missing.")
  }
  if(!is.character(receptor_id)) {
    stop("Required argument 'receptor_id' must be in character format.")
  }
  if((receptor_id %in% colnames(receptor_sf)) == FALSE) {
    stop("Required argument 'receptor_id' must be a column name in 'receptor_sf'.")
  }
  
  if(is.null(source_polygon_sf)) {
    stop("Required argument 'source_polygon_sf' is missing.")
  }
  if(("sf" %in% class(source_polygon_sf)) == FALSE) {
    stop("Required argument 'source_polygon_sf' must be in simple features format.")
  }
  
  if(is.null(source_polygon_id)) {
    stop("Required argument 'source_polygon_id' is missing.")
  }
  if(!is.character(source_polygon_id)) {
    stop("Required argument 'source_polygon_id' must be in character format.")
  }
  if((source_polygon_id %in% colnames(source_polygon_sf)) == FALSE) {
    stop("Required argument 'source_polygon_id' must be a column name in 'source_polygon_sf'.")
  }
  
  if(!is.logical(add_all_to_output)) {
    stop("Optional argument 'add_all_to_output' must be logical (i.e., TRUE or FALSE).")
  }
  
  if(!is.logical(print_log_to_console)) {
    stop("Optional argument 'print_log_to_console' must be logical (i.e., TRUE or FALSE).")
  }
  
  if(!is.logical(write_log_to_file)) {
    stop("Optional argument 'write_log_to_file' must be logical (i.e., TRUE or FALSE).")
  }
  
  # Open log -------------------------------------------------------------------
  
  if(write_log_to_file == TRUE) {
    if(logr::log_status() != "open") {
      logr::log_open(show_notes = FALSE)
    }
    logr::sep("Spatial join of receptor points to source polygons.",
              console = print_log_to_console)
  }
  
  # Spatial join points to polygons --------------------------------------------
  
  # Transform point receptor coordinate reference system to match source polygons
  if(st_crs(receptor_sf) != st_crs(source_polygon_sf)) {
    receptor_sf <- sf::st_transform(receptor_sf, crs = st_crs(source_polygon_sf))
    if(write_log_to_file == TRUE) {
      logr::log_print("Point receptor coordinate reference system transformed to match source.",
                console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Point receptor coordinate reference system transformed to match source.")
    }
  } else if(st_crs(receptor_sf) != st_crs(source_polygon_sf)) {
    stop("Unable to transform point receptor coordinate reference system to match source.")
  }
  
  # Code runs faster assuming points and polygons are planar 
  sf::sf_use_s2(FALSE) 
  
  # Print update to console and log
  if(write_log_to_file == TRUE) {
    logr::log_print("Status update: Joining point receptors to source polygons...",
              console = print_log_to_console)
  } else if(print_log_to_console == TRUE) {
    message("Status update: Joining point receptors to source polygons...")
  }
  
  # Spatial join
  receptor_point_to_source_polygon_join <- 
    sf::st_join(receptor_sf, source_polygon_sf, left = TRUE)
  
  # Add a flag for receptor points not within a polygon
  receptor_point_to_source_polygon_crosswalk <- 
    sf::st_drop_geometry(receptor_point_to_source_polygon_join) %>%
    dplyr::mutate(not_in_polygon = dplyr::if_else(is.na(get(source_polygon_id)), 1, 0)) %>%
    dplyr::arrange(id)
  
  receptors_not_joined <- sum(receptor_point_to_source_polygon_crosswalk$not_in_polygon)
  
  # Print update to console and log
  if(receptors_not_joined > 0) { 
    if(write_log_to_file == TRUE) {
      logr::log_print(stringr::str_c('Warning:', receptors_not_joined, 
                                     'receptor points not joined to polygons.', sep = " "),
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message(stringr::str_c('Warning:', receptors_not_joined, 
                             'receptor points not joined to polygons.', sep = " "),
              console = print_log_to_console)
    }
  } 
  
  if(write_log_to_file == TRUE) {
    logr::log_print("Status update: Completed join of receptor points to source polygons.",
                    console = print_log_to_console)
  } else if(print_log_to_console == TRUE) {
    message("Status update: Completed join of receptor points to source polygons.")
  }

  # Remove extra columns from output
  if(add_all_to_output == FALSE) {
    receptor_point_to_source_polygon_crosswalk <-  
      receptor_point_to_source_polygon_crosswalk %>%
      dplyr::select(all_of(c(receptor_id, source_polygon_id, "not_in_polygon")))
  }
  
  # Return data frame with point receptor id and source receptor polygon id
  receptor_point_to_source_polygon_crosswalk
}
