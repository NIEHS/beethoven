# description: process modis data with
# buffer radii and the list of target covariates
# Insang Song
# Updated: 11/24/2023
# set base directory to ddn (provided that users ssh at triton)
rootdir <- "/ddn/gs1"
basedir <- "group/set/Projects/NRT-AP-model/"
userdir <- "home/songi2/projects/NRTAPModel"
inputdir <- "input/data"
modisdir <- "modis/raw"
rlibdir <- "home/songi2/r-libs/"
download_dir <- file.path(rootdir, basedir, inputdir, modisdir)
download_dir_user <- file.path(rootdir, userdir, inputdir, modisdir)
lib_dir <- file.path(rootdir, rlibdir)

if (!dir.exists(download_dir)) {
  dir.create(download_dir)
}
if (!dir.exists(download_dir_user)) {
  dir.create(download_dir_user)
}

# register custom library path for debugging on Triton
.libPaths(lib_dir)

# filter unique sites
source("./input/Rinput/processing_functions/filter_unique_sites.R")


# check if remotes package is available
if (!require(remotes)) {
  install.packages("remotes")
  library(remotes)
}

# install scalable_gis
if (!require(scomps)) {
  remotes::install_github("Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS")
}

pkgs <- c("terra", "sf", "future.apply", "scomps", "exactextractr", "foreach", "data.table", "tigris")
suppressMessages(invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE)))
tigris_cache_dir("~/tigris_cache")
options(sf_use_s2 = FALSE, tigris_use_cache = TRUE)
# rr <- terra::rast("...hdf")
# summary(rr["1 km 16 days NDVI"])

# https://opendap.cr.usgs.gov/opendap/hyrax/MOD09GA.061/h10v07.ncml.dap.nc4?dap4.ce=/MODIS_Grid_1km_2D_eos_cf_projection;/Latitude_1[0:1:2399][0:1:2399];/Longitude_1[0:1:2399][0:1:2399];/MODIS_Grid_500m_2D_eos_cf_projection;/sur_refl_b03_1[6574:1:6574][0:1:2399][0:1:2399];/sur_refl_b07_1[6574:1:6574][0:1:2399][0:1:2399];/sur_refl_b02_1[6574:1:6574][0:1:2399][0:1:2399];/sur_refl_b05_1[6574:1:6574][0:1:2399][0:1:2399];/sur_refl_b04_1[6574:1:6574][0:1:2399][0:1:2399];/sur_refl_b06_1[6574:1:6574][0:1:2399][0:1:2399];/sur_refl_b01_1[6574:1:6574][0:1:2399][0:1:2399];/time[6574:1:6574]
# https://opendap.cr.usgs.gov/opendap/hyrax/MOD09GA.061/h10v07.ncml.dap.nc?dap4.ce=/MODIS_Grid_1km_2D_eos_cf_projection
# https://opendap.cr.usgs.gov/opendap/hyrax/MOD09GA.061/h10v07.ncml.dap.nc4?dap4.ce=/Latitude_1%5B0:1:2399%5D%5B0:1:2399%5D;/Longitude_1%5B0:1:2399%5D%5B0:1:2399%5D;/MODIS_Grid_500m_2D_eos_cf_projection;/sur_refl_b03_1%5B6574:1:6574%5D%5B0:1:2399%5D%5B0:1:2399%5D;/sur_refl_b07_1%5B6574:1:6574%5D%5B0:1:2399%5D%5B0:1:2399%5D;/sur_refl_b02_1%5B6574:1:6574%5D%5B0:1:2399%5D%5B0:1:2399%5D;/sur_refl_b05_1%5B6574:1:6574%5D%5B0:1:2399%5D%5B0:1:2399%5D;/sur_refl_b04_1%5B6574:1:6574%5D%5B0:1:2399%5D%5B0:1:2399%5D;/sur_refl_b06_1%5B6574:1:6574%5D%5B0:1:2399%5D%5B0:1:2399%5D;/sur_refl_b01_1%5B6574:1:6574%5D%5B0:1:2399%5D%5B0:1:2399%5D;/time%5B6574:1:6574%5D
## main part
## hierarchy (arbitrary subsets of census regions)
states <- tigris::states(year = 2020)
states <- vect(states)
states_main <- states[!states$GEOID %in% c("02", "15", "60", "66", "68", "69", "72", "78"), ]
mod_cenreg <- read.csv("./input/data/regional_divisions.csv")
mod_cenreg <- mod_cenreg |>
  tidyr::separate_wider_delim(
    col = "States",
    delim = "-",
    names = paste0("state_", sprintf("%02d", 1:6)),
    too_few = "align_start") |>
  tidyr::pivot_longer(cols = 2:7) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::select(-name)
states_m <- merge(states_main, mod_cenreg, by.x = "STUSPS", by.y = "value")
mod_cr <- terra::aggregate(states_m, by = "Region") |>
  terra::project("EPSG:5070")


## path setting
modis_mod13 <-
  list.files(path = "~/Documents/input_test/61/MOD13A2/2018/001/",
             pattern = "*.hdf$",
             full.names = TRUE)

# virtual raster on the same date
# should run terra::describe(..., sds = TRUE)
# when you do not know about the exact layer to use
terra::describe(modis_mod13[1], sds = TRUE)
# $var
# get names
names(terra::rast(modis_mod13[1], lyrs = 1))

# mosaic all in daily data
modis_mod13vrt <- lapply(
  modis_mod13, \(x) terra::rast(x, lyrs = 1)) |>
  do.call(what = terra::mosaic, args = _)



plan(multicore, workers = 6L)
parallel::makeCluster(6L)


sites_v <- final_sites |>
  terra::vect(crs = "EPSG:4326") |>
  terra::project("EPSG:5070")

# mod13_10k <- scomps::distribute_process_hierarchy(
#       regions = mod_cr,
#       split_level = mod_cr$Region,
#       fun_dist = scomps::extract_with_buffer,
#       points = sites_v,
#       surf = modis_mod13vrt,
#       radius = 1e4L,
#       id = "Site.ID",
#       func = "mean"
#     )


# the data values in MODIS HDF files seem
# to have 0.00000001 scale, not like 0.00001 in the documentation.
# strange thing is that the value scale is restored after
# running gdal_translate or h4toh5.
# should consult NASA for details.
scomps::extract_with_buffer(
  points = sites_v,
  surf = modis_mod13vrt,
  radius = 1e4L,
  id = "Site.ID"
)

mod13_10k <- 
  foreach(image = modis_mod13,
          .combine = dplyr::bind_rows,
          .export = c("mod_cr", "sites_v"),
          .packages = c("terra", "scomps", "exactextractr", "foreach", "future", "dplyr")) %do%
    {
    modis_in <- terra::rast(image)
    scomps::distribute_process_hierarchy(
      regions = mod_cr,
      split_level = mod_cr$region,
      fun_dist = scomps::extract_with_buffer,
      points = final_sites,
      surf = modis_in,
      radius = 1e4L,
      id = "Site.ID",
      func = "mean"
    )
    }
