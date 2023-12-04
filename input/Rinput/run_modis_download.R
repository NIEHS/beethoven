# assuming that the working directory is the parent directory of the repository
rootdir <- "/ddn/gs1"
basedir <- "group/set/Projects/NRT-AP-model/"
userdir <- "home/songi2/projects/NRTAPModel"
inputdir <- "input/data"
modisdir <- "modis/raw"
rlibdir <- "home/songi2/r-libs/"
download_dir <- file.path(rootdir, basedir, inputdir, modisdir)
download_dir_user <- file.path(rootdir, userdir, inputdir, modisdir)
lib_dir <- file.path(rootdir, rlibdir)


# sourcing modis data download function
source("./input/Rinput/download_functions/download_modis_data.R")


products <-
  c(
    "MOD09GA",
    "MOD11A1",
    "MOD06_L2",
    "MCD19A2",
    "MOD13A2",
    "VNP46A2"
  )
# MOD06_L2 are not tiled;
# VNP46A2 is based on h5 formats


# Earthdata token (My Profile > Generate Token)
# Valid for 60 days; should reauthorize
edtoken <- readLines("~/.edtoken")[1]


args <- commandArgs(trailingOnly = TRUE)
product_query <- as.character(args[1])
dstart <- as.character(args[2])
dend <- as.character(args[3])

# 2023-11-27: The shell argument should describe
# the full dates of start and end in an appropriate format

#
# 1826 days
# MOD09GA 2.0-2.5 GB/day (4.2 TB)
# MOD11A1 80 MB/day
# MCD19A2 160 MB/day
# MOD13A2 300 MB/day
# VNP46A2 110 MB/day
# 1 TB for other four;
download_modis_data(
  date_start = dstart,
  date_end = dend,
  product = product_query,
  version = "61",
  horizontal_tiles = c(7, 13),
  vertical_tiles = c(3, 6),
  nasa_earth_data_token = edtoken,
  directory_to_save = download_dir_user,
  data_download_acknowledgement = TRUE
)
