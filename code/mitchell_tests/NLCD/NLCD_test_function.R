###############################################################################
## Script to test function to download, project and clip NCLD data
## at state boundaries using terra and tigris packages


nlcd_state <- function(year = 2021, state,
                       green_space = FALSE, download = FALSE){
  ## set the working directory for the function to pull data
  setwd("/Volumes/SET/Projects/NRT-AP-Model/code/mitchell_tests/NLCD")
  
  ## load required packages
  library(terra); library(tigris); options(tigris_use_cache = TRUE)
  
  ## if...else statement to read in data from folder
  ## default is set to 2021
  if(2019 == year){
    
    nlcd <- terra::rast("nlcd_2019_land_cover_l48_20210604.img")
    
  } else {
    
    nlcd <- terra::rast("nlcd_2021_land_cover_l48_20230630.img")
    
  }
  
  ## read in state data from tigris
  states <- tigris::states(year = year, progress_bar = FALSE)
  
  ## subset to state of interest and create SpatVector object
  state_poly <- terra::vect(subset(states,
                                   subset=NAME==state))
  
  ## reproject state boundary to NLCD projection
  state_proj <- project(state_poly, nlcd)
  
  ## crop NLCD data to state boundary
  nlcd_crop <- crop(nlcd, state_proj, mask = TRUE)
  
  
  ## if...else statement to create a binary green space and non-green space
  ## variable and map
  if(TRUE == green_space){
    
    ## variable for creating export file name
    green <- "green_space"
    
    ## list of all values of NLCD data
    nlcd_values <- c("Unclassified", "Open Water", "Perennial Snow/Ice",
                     "Developed, Open Space", "Developed, Low Intensity",
                     "Developed, Medium Intensity",
                     "Developed, High Intensity", "Barren Land",
                     "Deciduous Forest", "Evergreen Forest", "Mixed Forest",
                     "Shrub/Scrub", "Herbaceous", "Hay/Pasture",
                     "Cultivated Crops", "Woody Wetlands",
                     "Emergent Herbaceous Wetlands")
    
    ## binary indicator of whether NLCD data type is green space (1) or
    ## non green space (0)
    nlcd_binary <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    
    ## substitute 1 for green space and 0 for non-green space
    nlcd_subst <- subst(nlcd_crop, nlcd_values, nlcd_binary)
    
    ## matrix needed to rewrite 0 and 1 binary data
    nlcd_rcl <- matrix(c(0, 1, 0, 1), ncol = 2, byrow = TRUE)
    
    ## reclassify data as green space and non-green space
    nlcd_classify <- classify(nlcd_subst, rcl = nlcd_rcl)
    
    ## plot binary green space indicator
    nlcd_plot_green <- plot(nlcd_classify, col = c("grey", "forestgreen"),
                            type = "classes",
                            levels = c("Non-Green Space", "Green Space"),
                            main = paste0("Green Space Coverage for ",
                                          state, " in ", year),
                            axes = FALSE)
    nlcd_plot_green <- plot(state_proj, add=TRUE, border = "black")
    
    return(
      
      ## if green_space = TRUE, return the green space variable plot
      nlcd_plot_green
      
    )
  } else {
    
    ## variable for creating export file name
    green <- ""
    
    ## create plot to be returned
    nlcd_plot <- plot(nlcd_crop,
                      main = paste0("NLCD Data for ", state, " in ", year),
                      axes = FALSE)
    nlcd_plot <- plot(state_proj, add=TRUE, border="black")
    
    return(
      
      ## if green_space = FALSE, return the NLCD data plot
      nlcd_plot
      
    )
    
  }
  
  # ## if...else statement to export data to local
  # if(TRUE == download){
  # 
  #   ## set name for file based on selected data attributes
  #   file_name <- print(paste0("nlcd_", year, "_", state, "_", green, ".tif"))
  #   file_path <- file.path(getwd(), file.name)
  #   return(
  #     print(paste0(file_name))
  #   )
  # 
  #   if(TRUE == green_space){
  # 
  #     ## write raster data to file
  #     writeRaster(nlcd_classify, file_path)
  # 
  #   } else {
  # 
  #   ## write raster data to file
  #   writeRaster(nlcd_crop, file_path)
  # 
  #   }
  # 
  #   ## communicate where data was exported
  #   print(paste0("Data exported to folder ", getwd()))
  # 
  # } else {
  # 
  #   ## communicate that data was not exported
  #   print(paste0("Data not exported to folder"))
  #}

}
