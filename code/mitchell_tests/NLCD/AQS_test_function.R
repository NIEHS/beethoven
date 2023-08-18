###############################################################################
## Script to test function to download, project and clip EPA monitor data
## at state boundaries using sf and tigris packages


epa_monitors <- function(state, parameter=""){
  
  ## set working directory
  setwd("/Volumes/SET/Projects/NRT-AP-Model/code/mitchell_tests/NLCD")
  
  ## load required packages
  library(sf); library(tigris); options(tigris_use_cache = TRUE)
  
  ## load monitoring data
  monitors <- st_read(paste0(getwd(), "/aqs_monitors.csv"))
  
  ## remove monitors with missing coordinates
  monitors <- subset(monitors, subset=Longitude!="")
  monitors <- subset(monitors, subset=Latitude!="")
  
  
  ## subset to state of interest
  monitors_state <- subset(monitors, subset=State.Name==state)
  
  ## subset to parameter of interest if selected
  ## default is all parameter types
  if("" == parameter){
    monitors_state <- monitors_state
  } else {
    monitors_state <- subset(monitors_state,
                             subset=Parameter.Name==parameter)
  }
  
  ## convert data frame with lat and long to sf object
  monitors_sf <- st_as_sf(monitors_state, coords = c("Longitude", 
                                                       "Latitude"),
                              crs = "EPSG:4326")

  ## set monitor data to Albers Equal Area
  monitors_proj <- st_transform(monitors_sf, "ESRI:102008")
  
  ## import state data
  states <- tigris::states(year = 2021, progress_bar = FALSE)
  
  ## subset to state of interest
  state_poly <- sf::st_as_sf(subset(states,
                                   subset=NAME==state))
  
  ## set state data to Albers Equal Area
  state_poly <- st_transform(state_poly, crs(monitors_proj))
    
  ## plot
  monitor_plot <- plot(st_geometry(monitors_proj),
                       pch = 16, col = "blue", cex=0.25)
  
  monitor_plot <- plot(st_geometry(state_poly), add = TRUE, border = "black")

}
