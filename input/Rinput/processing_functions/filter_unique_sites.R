## Date: 2023-11-24
## Description: Get the unique site locations and the WGS84 coordinates of those
check_installed_load <- function(lib) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

pkgs <- c("data.table", "sf", "terra")
invisible(suppressMessages(sapply(pkgs, check_installed_load)))
options(sf_use_s2 = FALSE)

# data manipulation
source("./R/manipulate_spacetime_data.R")

## processing
sites <- readRDS("./tests/testdata/daily_88101_2018-2022.rds")

## get unique sites
##
sites$Site.ID <-
  sprintf("%02d%03d%04d%05d",
          sites$State.Code,
          sites$County.Code,
          sites$Site.Num,
          sites$Parameter.Code)

# select relevant fields only
sites_v <- unique(sites[, c("Site.ID", "Longitude", "Latitude", "Datum")])
names(sites_v)[2:3] <- c("lon", "lat")
sites_v <- as.data.table(sites_v)

# subset mainland
sites_v <- sites_v[!grepl("^(02|15|72|78|6)", Site.ID), ]

# NAD83 to WGS84
sites_v_nad <- project_dt(sites_v[Datum == "NAD83"], "EPSG:4269", "EPSG:4326")
sites_v_nad <- sites_v_nad[, c(3, 6, 5)]
sites_v_wgs <- sites_v[Datum == "WGS84"][, -4]
final_sites <- rbind(sites_v_wgs, sites_v_nad)

rm(list = grep("^(?!final)", ls(), perl = TRUE, value = TRUE))

# final_sites (data.table object) only remains in memory
# File ends