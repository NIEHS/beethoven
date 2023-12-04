# needs terra, exactextractr, spData, sf


nlcd_classes <- list(value = c(0, 11, 21, 22, 23, 24, 31, 41, 42, 43, 52,
                               71, 81, 82, 90, 95),
                     class = c("Unc", "WTR", "OSD", "LID", "MID", "HID",
                               "BRN", "DFO", "EFO", "MFO", "SHB",
                               "GRS", "PAS", "CRP", "WDW", "EHW"),
                     names = c("Unclassified",
                               "Open Water",
                               "Developed, Open Space",
                               "Developed, Low Intensity",
                               "Developed, Medium Intensity",
                               "Developed, High Intensity",
                               "Barren Land",
                               "Deciduous Forest",
                               "Evergreen Forest",
                               "Mixed Forest",
                               "Shrub/Scrub",
                               "Herbaceous",
                               "Hay/Pasture",
                               "Cultivated Crops",
                               "Woody Wetlands",
                               "Emergent Herbaceous Wetlands"),
                     col = c("white", "#476ba1", "#decaca", "#d99482", "#ee0000",
                             "#ab0000", "#b3aea3", "#68ab63", "#1c6330",
                             "#b5ca8f", "#ccba7d",  "#e3e3c2", "#dcd93d", 
                             "#ab7028", "#bad9eb", "#70a3ba")) 
nlcd_classes <- as.data.frame(nlcd_classes)


#' Compute land cover classes ratio in circle buffers around points
#'
#' @param data_vect terra::SpatVector of points geometry
#' @param buf_radius numeric (non-negative) giving the radius of buffer around points 
#' @param year numeric giving the year of NLCD data used
#' @export
calc_nlcd_ratio <- function(data_vect, 
                            buf_radius = 1000, 
                            year = 2021) {
  
  # check inputs 
  if (!is.numeric(buf_radius)){
    stop("buf_radius is not a numeric.")
  }
  if (buf_radius <= 0) {
    stop("buf_radius has not a likely value.")
  }
  if (!is.numeric(year)){
    stop("year is not a numeric.")
  }
  if (class(data_vect)[1] != "SpatVector") {
    stop("data_vect is not a terra::SpatVector")
  }
  
  # open nlcd file corresponding to the year
  path_data <- "./input/nlcd/raw/"
  nlcd_file <- list.files(path_data, 
                          pattern = paste0("nlcd_", year, "_.*.tif$"), 
                          full.names = TRUE) 
  #nlcd_file <- list.files("/Volumes/set/NLCD/", 
  #                        pattern = paste0("nlcd_", year, "_.*.tif$"), 
  #                        full.names = TRUE) 
  if (length(nlcd_file) == 0) {
    stop("NLCD data not available for this year.")
  }
  nlcd <- rast(nlcd_file)
  
  # select points within mainland US and reproject on nlcd crs if necessary
  # need spData library
  data(us_states)
  us_main <- st_union(us_states) %>%
    terra::vect() %>%
    terra::project(x = ., y = crs(data_vect))
  data_vect_b <- data_vect %>%
    terra::intersect(us_main, .)
  if (!same.crs(data_vect_b, nlcd)) {
    data_vect_b <- project(data_vect_b, crs(nlcd))
  }
  
  # create circle buffers with buf_radius 
  bufs_pol <- terra::buffer(data_vect_b, width = buf_radius) %>%
    sf::st_as_sf()
  
  # ratio of each nlcd class per buffer
  nlcd_at_bufs <- exact_extract(nlcd, 
                                st_geometry(bufs_pol), 
                                fun = "frac",
                                stack_apply = T, 
                                progress = F)
  
  # select only the columns of interest
  nlcd_at_bufs <- nlcd_at_bufs[names(nlcd_at_bufs)[grepl("frac_",
                                                       names(nlcd_at_bufs))]]
  # change column names
  nlcd_names <- names(nlcd_at_bufs) %>%
    sub("frac_", "", .) %>%
    as.numeric()
  nlcd_names <- nlcd_classes[nlcd_classes$value %in% nlcd_names, c("class")]
  new_names <- sapply(
    nlcd_names,
    function(x) {
      paste0("frac_", x, "_", year, "_", buf_radius, "m")
    }
  )
  names(nlcd_at_bufs) <- new_names
  
  # merge data_vect with nlcd class fractions (and reproject)
  new_data_vect <- cbind(data_vect_b, nlcd_at_bufs) %>% 
      project(., crs(data_vect)) 
  
  return(new_data_vect)
}


