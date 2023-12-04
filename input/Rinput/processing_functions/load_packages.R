# set base directory to ddn (provided that users ssh at triton)
username <- "songi2"
rootdir <- "/ddn/gs1"
basedir <- "group/set/Projects/NRT-AP-model/"
userdir <- sprintf("home/%s/projects/NRTAPModel", username)
inputdir <- "input/data"
modisdir <- "modis/raw"
rlibdir <- sprintf("home/%s/r-libs/", username)
download_dir <- file.path(rootdir, basedir, inputdir, modisdir)
download_dir_user <- file.path(rootdir, userdir, inputdir, modisdir)
lib_dir <- file.path(rootdir, rlibdir)


# register custom library path for debugging on Triton
.libPaths(lib_dir)

# check if remotes package is available
if (!require(pak)) {
  install.packages("pak")
  library(pak)
}

# install scalable_gis
if (!require(scomps)) {
  pak::pak("Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS")
}

pkgs <- c("terra", "sf", "doParallel", "parallel", "parallelly",
  "future.apply", "scomps", "exactextractr", "foreach",
  "data.table", "tigris", "doRNG")
suppressMessages(
  invisible(
    sapply(pkgs,
      library,
      character.only = TRUE,
      quietly = TRUE)))
tigris_cache_dir("~/tigris_cache")
options(sf_use_s2 = FALSE, tigris_use_cache = TRUE)



# filter unique sites
source(
  file.path(rootdir, userdir, "input/Rinput/processing_functions/filter_unique_sites.R"))

unique_sites <- filter_unique_sites(
  file.path(rootdir, userdir, "tests/testdata/daily_88101_2018-2022.rds"),
  file.path(rootdir, userdir, "R/manipulate_spacetime_data.R"))
# unique_sites_t <- filter_unique_sites(include_time = TRUE)
sites <- sf::st_as_sf(unique_sites, coords = 2:3, crs = "EPSG:4326")
