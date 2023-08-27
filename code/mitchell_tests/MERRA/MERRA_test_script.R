###############################################################################
## Test script for reading and manipulating MERRA2 satellite data

## 0. set working directory and import libraries
wd <- "/Volumes/SET/Projects/NRT-AP-Model/code/mitchell_tests/MERRA"
setwd(wd)

library(terra); library(tigris); library(sf)


## 1. download MERRA data
## system("touch .wgetrc | chmod og-rw .wgetrc")
## system("echo http-user=mitchell_manware >> .wgetrc | echo http-password=Chssoccer6797? >> .wgetrc")

## system("wget --user=mitchell_manware --password=Chssoccer6797? https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/M2SDNXSLV.5.12.4/2023/06/MERRA2_400.statD_2d_slv_Nx.20230630.nc4)


## 2. read data from file
## NOTE: terra::rast() function can read .nc4 files
## NOTE: ncdf4::nc_open() is not needed
data <- terra::rast("MERRA2_400.statD_2d_slv_Nx.20230630.nc4")

terra::crs(data)



terra::image(data$T2MMEAN)
terra::plot(data$T2MMEAN)


## 3. subset to 2-meter air temperature data and convert Kelvin to Celsius
air <- data$T2MMEAN-273.15
plot(air)


## 4. inspect data
air; summary(air); ext(air); crs(air)


## 5. reproject air data
## crs(air) <- "ESRI:102008"
## air <- terra::project(air, "ESRI:102008")


## 5. load US boundary data and subset to continental United States
states <- tigris::states()

territories <- c("Guam",
                 "Puerto Rico",
                 "American Samoa",
                 "Commonwealth of the Northern Mariana Islands",
                 "United States Virgin Islands",
                 "Alaska",
                 "Hawaii")

conus_states <- terra::vect(subset(states, !(NAME %in% territories)))


## 6. merge individual states boundaries to one border for conus
conus <- terra::aggregate(conus_states)
crs(conus) <- crs(air)
plot(conus, add=TRUE)


## 7. crop temperature data to conus border
air_conus <- terra::project(crop(air, conus, mask=TRUE), "ESRI:102008")
plot(air_conus)

## 8. test running custom function
test <- merra_temperature(start = "06-05-2023",
                          end = "06-07-2023",
                          state = "",
                          variable = "mean temperature",
                          unit = "Kelvin")

for (i in 1:3){
  terra::plot(test, i)
}
###############################################################################


###############################################################################
## set working directory
wd <- "/Volumes/manwareme/NRT-AP-Model/code/mitchell_tests/MERRA"
setwd(wd)
getwd()

## read files with '.nc4' file type
files <- list.files(paste0(wd, "/add_to_ignore"),
                    pattern = "*.nc4",
                    full.names = TRUE)

## for loop to read '.nc4' files and plot
for(f in 1:length(files)){
  
  ## read data
  data <- terra::rast(files[f])
  
  ## plot data
  plot(data$T2MMEAN)
  
}







