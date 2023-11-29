# description: process modis data with
# buffer radii and the list of target covariates
# Insang Song
# Updated: 11/29/2023
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

# if (!dir.exists(download_dir)) {
#   dir.create(download_dir)
# }
# if (!dir.exists(download_dir_user)) {
#   dir.create(download_dir_user)
# }

# register custom library path for debugging on Triton
.libPaths(lib_dir)

# filter unique sites
source("./input/Rinput/processing_functions/filter_unique_sites.R")

unique_sites <- filter_unique_sites()
# unique_sites_t <- filter_unique_sites(include_time = TRUE)


# check if remotes package is available
if (!require(remotes)) {
  install.packages("remotes")
  library(remotes)
}

# install scalable_gis
if (!require(scomps)) {
  remotes::install_github("Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS")
}

pkgs <- c("terra", "sf", "future", "future.apply", "scomps", "exactextractr", "foreach", "data.table", "tigris")
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
# states <- tigris::states(year = 2020)
# states <- vect(states)
# states_main <- states[!states$GEOID %in% c("02", "15", "60", "66", "68", "69", "72", "78"), ]
# mod_cenreg <- read.csv("./input/data/regional_divisions.csv")
# mod_cenreg <- mod_cenreg |>
#   tidyr::separate_wider_delim(
#     col = "States",
#     delim = "-",
#     names = paste0("state_", sprintf("%02d", 1:6)),
#     too_few = "align_start") |>
#   tidyr::pivot_longer(cols = 2:7) |>
#   dplyr::filter(!is.na(value)) |>
#   dplyr::select(-name)
# states_m <- merge(states_main, mod_cenreg, by.x = "STUSPS", by.y = "value")
# mod_cr <- terra::aggregate(states_m, by = "Region") |>
#   terra::project("EPSG:5070")


## path setting
modis_mod13 <-
  list.files(path = "~/projects/NRTAPModel/input/data/modis/raw/61/MOD13A2",
             pattern = "*.hdf$",
             recursive = TRUE,
             full.names = TRUE)

# virtual raster on the same date
# should run terra::describe(..., sds = TRUE)
# when you do not know about the exact layer to use
# terra::describe(modis_mod13[1], sds = TRUE)

# $var
# get names
# names(terra::rast(modis_mod13[1], lyrs = 1))

# mosaic all in daily data
# modis_mod13vrt <- lapply(
#   modis_mod13, \(x) terra::rast(x, lyrs = 1)) |>
#   do.call(what = terra::vrt, args = _)

# modis_mod13vrt <- terra::vrt(modis_mod13, options = c("-sd", 1), set_names = TRUE)



modis_worker <- function(
  paths,
  date,
  name_extracted = NULL,
  subdataset = NULL,
  foo = scomps::extract_with_buffer,
  ...
) {
  if (is.null(name_extracted)) {
    stop("No name of target raster is specified.\n")
  }
  today <- as.character(date)
  year_today <- strftime(today, "%Y")
  jul_today <- strftime(today, "%j")
  dayjul <- paste0(year_today, "/", jul_today)
  files_today <- grep(paste0(dayjul), paths, value = TRUE)
  # files_today <- list.files(path = dir_today, pattern = "*.hdf$", full.names = TRUE)
  if (is.null(subdataset)) {
    terra::describe(files_today[1], sds = TRUE)
    stop("Please put relevant index for subdataset. The full list of subdatasets is above.\n")
  }
  vrt_today <- terra::vrt(files_today, options = c("-sd", subdataset), set_names = TRUE)
  extracted <- foo(..., surf = vrt_today)
  # assuming that extracted is a data.frame
  extracted$date <- date
  names(extracted)[ncol(extracted)-1] <- name_extracted
  return(extracted)
}


sites <- sf::st_as_sf(unique_sites, coords = 2:3, crs = "EPSG:4326")


plan(multicore, workers = 23L)
doFuture::registerDoFuture()
set.seed(202311)
# mod13dates <- seq(1, 366, 16)
# mod13_2018 <- sprintf("%d%03d", 2018, mod13dates)
# mod13_2018 <- as.Date(mod13_2018, format = "%Y%j")
# mod13_2018

resdf <-
foreach(
  datei = seq_along(mod13dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mod13", "mod13dates", "modis_worker", "radius"),
  .combine = "rbind"
) %dopar% {
  foreach(
    year = seq(2018, 2022),
    .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
    .export = c("sites", "modis_mod13", "mod13dates", "modis_worker", "datei", "radius"),
    .combine = "rbind"
  ) %do% {
    options(sf_use_s2 = FALSE)
    radius = c(1e3L, 1e4L, 5e4L)
    
    mod13_thisyear <- sprintf("%d%03d", year, mod13dates)
    mod13_thisyear <- as.Date(mod13_thisyear, format = "%Y%j")

    #dateindex <- seq_along(mod13dates)
    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)

    res0 <-
    lapply(radiuslist,
      function(k) {
        extracted <- modis_worker(
          paths = modis_mod13,
          date = mod13_thisyear[datei],
          subdataset = 1,
          name_extracted = sprintf("MOD_NDVIV_0_%05d", radius[k]),
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
      })
    res <- Reduce(dplyr::left_join, res0)
    return(res)
  }
}




### MOD11A1
modis_mod11 <-
  list.files(path = "~/projects/NRTAPModel/input/data/modis/raw/61/MOD11A1",
             pattern = "*.hdf$",
             recursive = TRUE,
             full.names = TRUE)
# stringi::stri_extract_first(modis_mod11, regex = "\\d{4,4}/\\d{3,3}")
# TODO (11302023): identify subdataset and run extract operation
# in HPC


foreach(
  datei <- seq_along(mod13dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps"),
  .export = c("sites", "modis_mod13", "mod13dates", "modis_worker"),
  .combine = c
) %dopar% {
  foreach(
    year = seq(2018, 2022),
    .packages = c("sf", "terra", "exactextractr", "foreach", "scomps"),
    .export = c("sites", "modis_mod13", "mod13dates", "modis_worker", "datei"),
    .combine = c
  ) %do% {
    options(sf_use_s2 = FALSE)
    radius = c(1e3, 1e4, 5e4)
    
    mod13_thisyear <- sprintf("%d%03d", year, mod13dates)
    mod13_thisyear <- as.Date(mod13_thisyear, format = "%Y%j")

    #dateindex <- seq_along(mod13dates)
    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)

    res0 <-
    lapply(radiuslist,
      function(k) {
        extracted <- modis_worker(
          paths = modis_mod13,
          date = mod13dates[datei],
          subdataset = 1,
          name_extracted = sprintf("MOD_NDVIV_0_%05d", radius[k]),
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
      })
    res <- Reduce(rbind, res0)
    return(res)
  }
}

# test <- modis_worker(
#   paths = modis_mod13,
#   date = mod13_2018[1],
#   subdataset = 1,
#   name_extracted = "MOD_NDVIV_0_10000",
#   points = terra::vect(sites),
#   id = "site_id",
#   radius = 1e4L
# )

# grep("2018/001", modis_mod13, value = TRUE)


plan(multicore, workers = 6L)
parallel::makeCluster(6L)




sites_v <- unique_sites |>
  terra::vect(crs = "EPSG:4326")

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
  id = "site_id"
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
