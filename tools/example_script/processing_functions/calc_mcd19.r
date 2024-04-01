#   tigris
year <- commandArgs(TRUE)[1]
threads <- commandArgs(TRUE)[2]

apptainer <- FALSE
# set base directory to ddn (provided that users ssh at triton)

if (!apptainer) {
  rootdir <- "/ddn/gs1"
  basedir <- "group/set/Projects/NRT-AP-model/"
  userhome <- "home/songi2"
  userdir <- "home/songi2/projects/NRTAPModel"
  inputdir <- "input"
  modisdir <- "modis/raw"
  rlibdir <- "home/songi2/r-libs/"
  download_dir <- file.path(rootdir, basedir, inputdir, modisdir)
  download_dir_user <- file.path(rootdir, userdir, inputdir, modisdir)
  lib_dir <- file.path(rootdir, rlibdir)
  .libPaths(lib_dir)
} else {
  rootdir <- "/opt"
  basedir <- ""
  userhome <- ""
  userdir <- ""
  inputdir <- "input"
  modisdir <- "modis/raw"

}

# register custom library path for debugging on Triton
pkgs <-
  c("terra", "sf", "doParallel", "parallelly", "future",
    "future.apply", "exactextractr", "foreach", "data.table", "doRNG")
suppressMessages(invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE)))
options(sf_use_s2 = FALSE)

# filter unique sites
# source(
#   file.path(rootdir, userdir, "input/Rinput/processing_functions/filter_unique_sites.R"))

# unique_sites <- filter_unique_sites(
#   file.path(rootdir, userdir, "tests/testdata/daily_88101_2018-2022.rds"),
#   file.path(rootdir, userdir, "R/manipulate_spacetime_data.R"))
# sites_sf <- sf::st_as_sf(unique_sites, coords = 2:3, crs = "EPSG:4326")
sites_sf <- readRDS(file.path(rootdir, "home/songi2", "sites_unique.rds"))

radius <- c(0L, 1e3L, 1e4L, 5e4L)

# test <- FALSE
# if (test) year <- 2018; threads <- 2L

modis_mcd19 <- readLines(file.path(rootdir, "home/songi2", "rtest/mcd19filelist.txt"))
modis_mcd19y <-
  grep(sprintf("/%d/", as.integer(year)), modis_mcd19, value = TRUE)

source(file.path(rootdir, userdir, "R/calculate_covariates.R"))
source(file.path(rootdir, userdir, "R/calculate_covariates_support.R"))
source(file.path(rootdir, userdir, "R/manipulate_spacetime_data.R"))
list_foo <- c("modis_get_vrt", "modis_preprocess_vnp46", "modis_worker", "modis_mosaic_mod06", "is_stdt")
# HDF4_EOS:EOS_GRID:"/ddn/gs1/home/songi2/projects/NRTAPModel/input/modis/raw/61/MOD09GA/2018/001/MOD09GA.A2018001.h07v05.061.2021295010454.hdf":MODIS_Grid_500m_2D:sur_refl_b01_1


mcd19cn <-
  c("MOD_AD4TA_0_",
    "MOD_AD5TA_0_",
    "MOD_CSZAN_0_",
    "MOD_CVZAN_0_",
    "MOD_RAZAN_0_",
    "MOD_SCTAN_0_",
    "MOD_GLNAN_0_"
  )

mcd19_vars_1km <-
  calc_modis(
    modis_mcd19y,
    product = "MCD19A2",
    sites = sites_sf,
    subdataset = "(Optical_Depth)",
    name_covariates = mcd19cn[1:2],
    nthreads = as.integer(threads),
    export_list_add = list_foo
  )
# 11:20 started
Sys.time()

saveRDS(
  mcd19_vars_1km,
  file.path(
    rootdir,
    "home/songi2",
    sprintf("NRTAP_Covar_MCD19A2_AOD_%d.rds", as.integer(year))
  ),
  compress = "xz"
)

mcd19_vars_5km <-
  calc_modis(
    modis_mcd19y,
    product = "MCD19A2",
    sites = sites_sf,
    subdataset = "(cos|RelAZ|Angle)",
    name_covariates = mcd19cn[-1:-2],
    nthreads = as.integer(threads),
    export_list_add = list_foo
  )
saveRDS(
  mcd19_vars_5km,
  file.path(
    rootdir,
    "home/songi2",
    sprintf("NRTAP_Covar_MCD19A2_Angles_%d.rds", as.integer(year))
  ),
  compress = "xz"
)

Sys.time()
