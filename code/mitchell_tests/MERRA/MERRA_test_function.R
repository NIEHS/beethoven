###############################################################################
## Test function for reading and manipulating MERRA2 satellite data
## library(terra); library(tigris); library(sf); library(stringr)


merra_temperature <- function(start="06/29/2023",
                              end="06/30/2023",
                              state="",
                              unit="Kelvin",
                              variable="mean temperature",
                              username, password
                              ){

  ## set variables for searching for temperature data

  start_character <- as.character(start)
  start_date <- as.Date(start, format = "%m-%d-%Y")

  end_character <- as.character(end)
  end_date <- as.Date(end, format = "%m-%d-%Y")

  ## create list of dates based on user input
  dates_list <- format(seq(start_date, end_date, by="day"))

  ## create empty SpatRast to store downloaded data
  data_rast <- terra::rast()

  ## for each date in the date list, download MERRA2 temperature data
  for(i in 1:length(dates_list)){

    ## fixed character date for each date in list
    date <- as.character(dates_list[i])
    date <- gsub("-", "", date)

    ## define url to download data
    url <- paste0("https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/M2SDNXSLV.5.12.4/",
                  substr(date, 1, 4), "/",
                  substr(dates_list[i], 5, 6), "/MERRA2_400.statD_2d_slv_Nx.",
                  date, ".nc4")

    ## system() function to run Linux command in R script
    download_command <- paste0("wget --user=", username, "--password=",
                               password, " ", url, '"')
    system(download_command)

    ## set file name
    file_name <- paste0("MERRA2_400.statD_2d_slv_Nx.", date, ".nc4")

    ## read .nc4 data into raster
    data[i] <- terra::rast(file_name)

    ## system() function to remove data from local
    ## file once copied into R environment
    system(paste0("rm ", file_name))



    ###########################################################################
    ## FOR TEST SCRIPT, READ FROM LOCAL FILE, NOT DOWNLOAD
    # file_name <- file_name <- paste0("MERRA2_400.statD_2d_slv_Nx.",
    #                                  date, ".nc4")
    #
    # data <- terra::rast(file_name)
    ###########################################################################


    ## for loop to select variables of interest
    if("mean temperature" == variable){
      variable_data <- data$T2MMEAN
      print(paste0("Mean daily temperature on ", dates_list[i]))
    } else if ("max temperature" == variable){
      variable_data <- data$T2MMAX
      print(paste0("Maximum daily temperature on ", dates_list[i]))
    } else if ("min temperature" == variable){
      variable_data <- data$T2MMIN
      print(paste0("Minimum daily temperature on ", dates_list[i]))
    } else if ("hours without rain" == variable){
      variable_data <- data$HOURNORAIN
      print(paste0("Hours without rain on ", dates_list[i]))
    }

    ## add each imported layer to the store SpatRast
    data_rast <- c(data_rast, variable_data, warn = FALSE)

  }

  ## create a SpatRasterCollection from all layers
  data_sprc <- data_rast

  ## set units to Kelvin, Celsius or Fahrenheit
  if("Kelvin" == unit){

    ## Kelvin is default
    data_final <- data_sprc

  } else if ("Celsius" == unit){

    ## convert Kelvin to Celsius
    data_final <- data_sprc - 273.15

  } else if ("Fahrenheit" == unit){

    ## convert Kelvin to Fahrenheit
    data_final <- ((data_sprc - 273.15) * (9/5) + 32)

  }

  ## crop to CONUS or single state boundary
  ## import state boundary data
  states <- tigris::states(progress_bar = FALSE)

  ## list of Alaska, Hawaii and territories to be removed from CONUS
  territories <- c("Guam",
                   "Puerto Rico",
                   "American Samoa",
                   "Commonwealth of the Northern Mariana Islands",
                   "United States Virgin Islands",
                   "Alaska",
                   "Hawaii")

  ## default setting is to crop to CONUS boundary
  if("" == state){

    ## remove Alaska, Hawaii and territories from CONUS boundary
    conus <- terra::vect(subset(states, !(NAME %in% territories)))

    ## set CONUS crs to match MERRA data
    conus <- terra::project(conus, "epsg:4326")

    terra::crs(data_final) <- "epsg:4326"

    ## crop to CONUS boundary
    data_conus <- terra::crop(data_final, conus, mask = TRUE)

    return(
      ## return the cropped raster stack/brick to the global environment
      data_conus
    )

  } else {

    ## if specific state is selected, crop to the state boundary
    state_select <- terra::vect(subset(states, subset=NAME==state))

    ## set crs to match MERRA data
    state_project <- terra::project(state_select, "epsg:4326")

    terra::crs(data_final) <- "epsg:4326"

    ## crop to state boundary
    data_state <- terra::crop(data_final, state_project, mask = TRUE)

    return(
      ## return the cropped raster stack/brick to the global environment
      data_state
    )

  }

}


## run test function
data_test <- merra_temperature(start = "06-10-2023",
                               end = "06-15-2023",
                               state = "",
                               variable = "mean temperature",
                               unit = "Fahrenheit",
                               username = "mitchell_manware",
                               password = "Chssoccer6797?")

## write raster to view output
terra::writeRaster(data_test)




















