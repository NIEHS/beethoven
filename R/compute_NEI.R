library(sf)
library(tidyverse)
library(data.table)

# Code taken from https://gist.github.com/apalbright/84cff7cb220f9db41138f7fa9b72e511

# download the zip file "cb_2018_us_county_500k.zip" from US Census website here:
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# unzip the file, put it where you are working, and then call the shape file within the unzipped folder



setwd("/Users/dawr2/Desktop/Ranadeep_Daw_Projects/NEI/")




# Load NEI data
onroad2017_123  <- fread("./input/NEI/aadt2017/onroad_123.csv")
onroad2017_4    <- fread("./input/NEI/aadt2017/onroad_4.csv")
onroad2017_5    <- fread("./input/NEI/aadt2017/onroad_5.csv")
onroad2017_67   <- fread("./input/NEI/aadt2017/onroad_67.csv")
onroad2017_8910 <- fread("./input/NEI/aadt2017/onroad_8910.csv")

onroad2017 <- do.call("rbind", list(onroad2017_123, onroad2017_4, onroad2017_5, onroad2017_67, onroad2017_8910))
onroad2017[, total_emission:= ifelse(`emissions uom` =="TON", `total emissions`, `total emissions`/2000)]
onroad2017 <- onroad2017[order(`fips code`), ]
onroad2017 <- onroad2017[, list(`fips code`, total_emission)]
#onroad2017$`fips code` %>% unique() %>% length()
onroad2017 <- onroad2017[, .(total_emission=sum(total_emission)), by=`fips code`]
onroad2017 = onroad2017[, `fips code` := as.character(`fips code`)]
onroad2017[ ,pseudo_Year := 2017]

onroad2020_123  <- fread("./input/NEI/aadt2020/onroad123.csv")
onroad2020_4    <- fread("./input/NEI/aadt2020/onroad_4.csv")
onroad2020_5    <- fread("./input/NEI/aadt2020/onroad_5.csv")
onroad2020_67   <- fread("./input/NEI/aadt2020/onroad_67.csv")
onroad2020_8910 <- fread("./input/NEI/aadt2020/onroad_8910.csv")

onroad2020 <- do.call("rbind", list(onroad2020_123, onroad2020_4, onroad2020_5, onroad2020_67, onroad2020_8910))
onroad2020 <- onroad2020[order(`fips code`), ]
onroad2020[, total_emission:= ifelse(`emissions uom` =="TON", `total emissions`, `total emissions`/2000)]
onroad2020 <- onroad2020[, list(`fips code`, total_emission)]
#onroad2020$`fips code` %>% unique() %>% length()
onroad2020 <- onroad2020[, .(total_emission=sum(total_emission)), by=`fips code`]
onroad2020 = onroad2020[, `fips code` := as.character(`fips code`)]
onroad2020[ ,pseudo_Year := 2020]

onroad <- rbind(onroad2017, onroad2020)


# Read county data
counties<-st_read("./input/NEI/cb_2018_us_county_500k/cb_2018_us_county_500k.shp", quiet=T)



# Mock lat-lon data. Input locations with columns "Longitude", "Latitude"
input_dat = data.table("Latitude"= 35.7596, "Longitude"= -79.0193, "Year" = 2018)





######## Now compute NEI #########
compute_NEI <- function(input_dat, onroad){
  
  input_dat[, pseudo_Year:= ifelse(Year < 2020, 2017, 2020)]
  
  input_dat_sf = input_dat
  input_dat_sf[, Lon:= Longitude]
  input_dat_sf[, Lat:= Latitude]
  input_dat_sf = st_as_sf(input_dat, coords = c("Longitude", "Latitude"), 
                          crs=st_crs(counties), agr = "constant")
  
  
  intersected <- st_intersects(input_dat_sf, counties)
  input_dat_sf <- input_dat_sf %>%
    mutate(intersection = as.integer(intersected),
           `fips code` = if_else(is.na(intersection), "",
                                 counties$GEOID[intersection]))
  
  
  input_dat_sf_dt <- as.data.table(input_dat_sf)
  output_dat_sf <- input_dat_sf_dt[onroad, on = .(`fips code`, pseudo_Year), nomatch = NULL]
  
  return(output_dat_sf)
}


# Mock lat-lon data. Input locations with columns "Longitude", "Latitude"
input_dat = data.table("Latitude"= 35.7596, "Longitude"= -79.0193, "Year" = 2022)

output = compute_NEI(input_dat, onroad)
output
