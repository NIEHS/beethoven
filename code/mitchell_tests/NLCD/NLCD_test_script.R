###############################################################################
## Script to test downloading and manipulating NLCD data 
## using terra and sf packages


###############################################################################
## 0.1 download and unzip NLCD 2021 data in terminal
## wget https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2021_land_cover_l48_20230630.zip
## unzip nlcd_2021_land_cover_l48_20230630.zip

## 0.2 download and unzip NLCD 2019 data in terminal
## wget https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2019_land_cover_l48_20210604.zip
## unzip nlcd_2019_land_cover_l48_20210604.zip


## 1. set working directory variable
setwd("/Volumes/SET/Projects/NRT-AP-Model/code/mitchell_tests/NLCD")
getwd()
wd <- ("/Volumes/SET/Projects/NRT-AP-Model/code/mitchell_tests/NLCD")


## 2. load packages
library(terra); library(sf)

## note: for script that will be in the package, adding dependancy or
## install.packages("terra", "sf") will be required


## 3. read NLCD data file as raster
nlcd_2021 <- rast("nlcd_2021_land_cover_l48_20230630.img")
nlcd_2019 <- rast("nlcd_2019_land_cover_l48_20210604.img")


## 4. inspect nlcd_2021 data
nlcd_2021
summary(nlcd_2021)
ext(nlcd_2021)


## 5. plot data
terra::plot(nlcd_2019, main="2019")
terra::plot(nlcd_2021, main="2021")


## 6. import and manipulate Connecticut border polygon using terra SpatVector
library(tigris)
## import county boundaries
ct <- terra::vect(counties(state="CT", year = 2021))
plot(ct)
## merge county 
ct <- terra::aggregate(ct)
plot(ct)
## inspect polygon
crs(ct, describe=TRUE)
ext(ct)


## 7. clip NLCD data to Connecticut boundary
## copy nlcd data to be cropped
nlcd_2021_ct <- nlcd_2021
## set projection of Connecticut boundary
ct_proj <- project(ct, nlcd_2021_ct)
## crop the NLCD data to the Connecituct boundary
nlcd_2021_ct <- crop(nlcd_2021_ct, ct_proj, mask=TRUE)
## inspect plot
plot(nlcd_2021_ct)
plot(ct_proj, add=TRUE, border="black")


## 8. reclassify NLCD data to binary green space variable
## NOTE: in terra package, subst function can substitute categorical
## NOTE: raster data with numeric data
## NOTE: for NLCD data, subst function must be used before classify

## list all values of NLCD data
nlcd_values <- c("Unclassified", "Open Water", "Perennial Snow/Ice",
                 "Developed, Open Space", "Developed, Low Intensity",
                 "Developed, Medium Intensity",
                 "Developed, High Intensity", "Barren Land",
                 "Deciduous Forest", "Evergreen Forest", "Mixed Forest",
                 "Shrub/Scrub", "Herbaceous", "Hay/Pasture", "Cultivated Crops",
                 "Woody Wetlands","Emergent Herbaceous Wetlands")
## binary indicator of whether NLCD data type is green space (1) or not (0)
nlcd_binary <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)

## substitute 0 for "non green space" and 1 for "green space"
nlcd_2021_subst <- subst(nlcd_2021_ct, nlcd_values, nlcd_binary)

## matrix needed to rewrite 0 and 1 binary data
nlcd_rcl <- matrix(c(0, 1, 0, 1), ncol = 2, byrow = TRUE)

## reclassify data as non green space (0) or green space (1)
nlcd_2021_classify <- classify(nlcd_2021_subst, rcl = nlcd_rcl)

## plot binary green space indicator
plot(nlcd_2021_classify, col = c("grey", "forestgreen"),
     type = "classes",
     levels = c("Non Green Space", "Green Space"))


## 9. run custom NLCD functions
getwd()
## default data
nlcd_state(state = "Connecticut", year = 2019, green_space = FALSE)
nlcd_state(state = "Connecticut", year = 2021, green_space = FALSE)

## green space indicator
nlcd_state(state = "Connecticut", year = 2019, green_space = TRUE)
nlcd_state(state = "Connecticut", year = 2021, green_space = TRUE)

## data download
nlcd_state(state = "Connecticut", year = 2021, green_space = FALSE,
           download = TRUE)

getwd()


###############################################################################
## 10. read AQS monitor data using sf package
monitors <- st_read(paste0(getwd(), "/aqs_monitors.csv"))
head(monitors)
colnames(monitors)

## remove monitors with missing coordinates
monitors <- subset(monitors, subset=Longitude!="")
monitors <- subset(monitors, subset=Latitude!="")

## subset to only EPA monitors
epa_monitors <- subset(monitors, subset=Monitor.Type=="EPA")


## 11. convert data frame with lat and long data to a sf object
epa_monitors_sf <- st_as_sf(epa_monitors, coords = c("Longitude", "Latitude"),
                            crs = "EPSG:4326")
crs(epa_monitors_sf, describe = TRUE)


## 12. plot EPA monitor locations
plot(st_geometry(epa_monitors_sf), pch=16, cex=0.2)


## 13. set monitor data to Albers Equal Area crs
epa_monitors_proj <- st_transform(epa_monitors_sf, "ESRI:102008")
plot(st_geometry(epa_monitors_proj), pch=16, cex=0.2, col="blue")


## 14. run custom AQS functions
epa_monitors(state = "Arizona")
epa_monitors(state = "Arizona", parameter = "Sulfur dioxide")






## 9. run custom NLCD functions
## default data
nlcd_state(state = "Connecticut", year = 2019, green_space = FALSE)
nlcd_state(state = "Connecticut", year = 2021, green_space = FALSE)

## green space indicator
nlcd_state(state = "Connecticut", year = 2019, green_space = TRUE)
nlcd_state(state = "Connecticut", year = 2021, green_space = TRUE)


## 14. run custom AQS functions
## all parameters
epa_monitors(state = "Arizona")

## select parameter (Sulfer dioxide in Arizona or California is good exmaple)
epa_monitors(state = "Arizona", parameter = "Sulfur dioxide")















