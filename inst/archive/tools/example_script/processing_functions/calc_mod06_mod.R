#   tigris

year <- commandArgs(TRUE)[[1]]
threads <- commandArgs(TRUE)[[2]]


apptainer <- TRUE
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
  userdir <- "home/songi2/projects/NRTAPModel"
  inputdir <- "input"
  modisdir <- "modis/raw"
  rlibdir <- "home/songi2/r-libs/"
  download_dir <- file.path(rootdir, basedir, inputdir, modisdir)
  download_dir_user <- file.path(rootdir, userdir, inputdir, modisdir)
  lib_dir <- file.path(rootdir, rlibdir)

}

# register custom library path for debugging on Triton
pkgs <- c("terra", "sf", "doParallel", "parallelly", "future.apply", "exactextractr", "foreach", "data.table", "doRNG")
suppressMessages(invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE)))
# tigris_cache_dir("~/tigris_cache")
options(sf_use_s2 = FALSE)

# filter unique sites
source(
  file.path(rootdir, userdir, "input/Rinput/processing_functions/filter_unique_sites.R"))

unique_sites <- filter_unique_sites(
  file.path(rootdir, userdir, "tests/testdata/daily_88101_2018-2022.rds"),
  file.path(rootdir, userdir, "R/manipulate_spacetime_data.R"))
sites_sf <- sf::st_as_sf(unique_sites, coords = 2:3, crs = "EPSG:4326")
# sites_sf <- readRDS(file.path(rootdir, "home/songi2", "sites_unique.rds"))

# check if remotes package is available
if (!require(pak)) {
  install.packages("pak")
  library(pak)
}

# install scalable_gis
# if (!require(scomps)) {
#   pak::pak("Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS")
# }


radius <- c(0L, 1e3L, 1e4L, 5e4L)

# mod06_dir <- file.path(rootdir, userdir, inputdir, "modis", "raw", "61", "MOD06_L2")

# files_mod06 <-
#   list.files(path = mod06_dir,
#              pattern = "*.hdf$",
#              full.names = TRUE,
#              recursive = TRUE)
# writeLines(files_mod06, "~/rtest/mod06l2filelist.txt")


# stringi::stri_extract_first(modis_mod11, regex = "\\d{4,4}/\\d{3,3}")
# terra::describe(modis_mod09[1], sds = T)
# SDS 2:8 /12:18/ (Band 1 - 7)
#  1 MOD_SFCRF_1_00000
#  2 MOD_SFCRF_2_00000
#  3 MOD_SFCRF_3_00000
#  4 MOD_SFCRF_4_00000
#  5 MOD_SFCRF_5_00000
#  6 MOD_SFCRF_6_00000
#  7 MOD_SFCRF_7_00000
# terra::rast(modis_mod09[1], lyrs = 2:8) |> names()
# terra::rast(modis_mod09[41272], subds = 4) |> summary()


test <- TRUE
if (test) year <- 2022; threads <- 41L

modis_mod06 <- readLines(file.path(rootdir, "home/songi2", "rtest/mod06l2filelist.txt"))
modis_mod06y <-
  grep(sprintf("/%d/", as.integer(year)), modis_mod06, value = TRUE)

source(file.path(rootdir, userdir, "R/calculate_covariates.R"))
source(file.path(rootdir, userdir, "R/manipulate_spacetime_data.R"))
list_foo <- c("modis_get_vrt", "modis_preprocess_vnp46", "modis_worker", "modis_mosaic_mod06", "is_stdt")
# HDF4_EOS:EOS_GRID:"/ddn/gs1/home/songi2/projects/NRTAPModel/input/modis/raw/61/MOD09GA/2018/001/MOD09GA.A2018001.h07v05.061.2021295010454.hdf":MODIS_Grid_500m_2D:sur_refl_b01_1
# layertemplate <- "HDF4_EOS:EOS_GRID:%s:MODIS_Grid_500m_2D:sur_refl_b%02d_1"


mod06_vars <-
  calc_modis(
    modis_mod06y,
    product = "MOD06_L2",
    sites = sites_sf,
    name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
    nthreads = as.integer(threads),
    export_list_add = list_foo
  )


# modis_mosaic_mod06(modis_mod06y[1:26], "2020-01-01")
# mod09_vars_df <-
#   Reduce(
#     function(x, y) left_join(x, y, by = c("site_id", "time")),
#     mod09_vars
#   )

saveRDS(mod06_vars, file.path(rootdir, "home/songi2", sprintf("NRTAP_Covar_MOD06L2_%d.rds", as.integer(year))), compress = "xz")
