# description: process modis data with
# buffer radii and the list of target covariates
# Insang Song
# Updated: 11/22/2023

# set base directory to ddn (provided that users ssh at triton)
rootdir <- "/ddn/gs1"
basedir <- "group/set/Projects/NRT-AP-model/"
inputdir <- "input/"
modisdir <- "modis/raw"
rlibdir <- "home/songi2/r-libs/"
download_dir <- file.path(rootdir, basedir, inputdir, modisdir)
lib_dir <- file.path(rootdir, rlibdir)

if (!dir.exists(download_dir)) {
  dir.create(download_dir)
}

# register custom library path for debugging on Triton
.libPaths(lib_dir)

# assuming that the working directory is the parent directory of the repository
getwd()

# sourcing modis data download function
source("./input/Rinput/download_functions/download_modis_data.R")
args(download_modis_data)

# check if remotes package is available
if (!require(remotes)) {
  install.packages("remotes")
  library(remotes)
}

# install scalable_gis
if (!require(scomps)) {
  remotes::install_github("Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS")
}

pkgs <- c("terra", "sf", "future.apply", "scomps", "exactextractr")
suppressMessages(invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE)))
options(sf_use_s2 = FALSE)

products <- c(
    "MOD09GA",
    "MOD11A1",
    "MOD06_L2",
    "MCD19A2",
    "MOD13A2",
    "VNP46A2"
)

# Earthdata token (My Profile > Generate Token)
# Valid for 60 days; should reauthorize
edtoken <- readLines("~/.edtoken")[1]

download_modis_data(
  date_start = "2018-01-01",
  date_end = "2018-01-31",
  product = products[1],
  version = "61",
  horizontal_tiles = c(7, 13),
  vertical_tiles = c(3, 6),
  nasa_earth_data_token = edtoken,
  directory_to_save = "/Users/songi2/Documents/input",
  data_download_acknowledgement = TRUE
)
