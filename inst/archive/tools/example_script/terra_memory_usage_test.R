
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
pkgs <- c("terra", "sf", "doParallel", "parallelly", "future.apply", "exactextractr", "foreach", "data.table", "doRNG")
suppressMessages(invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE)))
# tigris_cache_dir("~/tigris_cache")
options(sf_use_s2 = FALSE)

modis_vnp46 <- readLines(file.path(rootdir, "home/songi2", "rtest/vnp46filelist.txt"))
modis_vnp46y <-
  grep(sprintf("/%d/", as.integer(year)), modis_vnp46, value = TRUE)
modis_mod09 <- readLines(file.path(rootdir, "home/songi2", "rtest/vnp46filelist.txt"))
modis_mod09 <-
  grep(sprintf("/%d/", as.integer(year)), modis_vnp46, value = TRUE)


source(file.path(rootdir, userdir, "R/calculate_covariates.R"))
source(file.path(rootdir, userdir, "R/manipulate_spacetime_data.R"))
list_foo <- c("modis_get_vrt", "modis_preprocess_vnp46", "modis_worker", "modis_mosaic_mod06", "is_stdt")

library(profvis)
sites_sf <- readRDS(file.path(rootdir, "home/songi2", "sites_unique.rds"))

profvis::profvis(
    vnp46k <- modis_preprocess_vnp46(modis_vnp46y[1:27], "2018-01-01")
)

profvis::profvis(
    vnp46ke <- modis_worker(vnp46k, "2018-01-01", sites_sf, "VNP46A2", radius = 50000L)
)

system.time(
profvis::profvis(
    vnp46ke1 <- calc_modis(
        modis_vnp46y[1:54],
        "VNP46A2",
        sites_sf,
        name_covariates = "Nightlight_0_",
        # radius = 50000L,
        nthreads = 1L,
        export_list_add = list_foo
    ),
    torture = FALSE
)
)


kk <-
modis_get_vrt(
  modis_mcd19y[1:23], "MCD19A2", "2018-01-01", regex_sds = "(Optical_Depth|cos|RelAZ|Angle)"
)

profvis::profvis(
  mcd19_vars1 <-
  calc_modis(
    modis_mcd19y[1:46],
    product = "MCD19A2",
    sites = sites_sf,
    subdataset = "(cos|RelAZ|Angle)",
    name_covariates = mcd19cn,
    nthreads = as.integer(threads),
    export_list_add = list_foo
  )
)
