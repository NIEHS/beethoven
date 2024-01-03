args <- commandArgs(trailingOnly = TRUE)
year_target <- args[[1]]

ddnhome <- "/ddn/gs1/home/songi2"
usrlib <- file.path(ddnhome, "r-libs")
prjhome <- file.path(ddnhome, "projects", "NRTAPModel")
inputdir <- file.path(prjhome, "input")
.libPaths(usrlib)

if (!require(NRTAPmodel)) {
  pak::pak("Spatiotemporal-Exposures-and-Toxicology/NRTAPmodel@isong-calc-covars")
  library(NRTAPmodel)
}

pkgs <- c("terra", "sf", "foreach", "parallelly", "doParallel", "data.table", "doRNG", "exactextractr")
invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE))
sf::sf_use_s2(FALSE)

setwd(prjhome)



##
# source(file.path(prjhome, "R", "calculate_covariates.R"))
source(file.path(inputdir, "Rinput", "processing_functions", "filter_unique_sites.R"))
sites <- filter_unique_sites()
sites_sf <-
  sf::st_as_sf(as.data.frame(sites), coords = c("lon", "lat"),
               crs = "EPSG:4326")

mod06_dir <- file.path(inputdir, "modis", "raw", "61", "MOD06_L2")
files_mod06 <-
  list.files(path = mod06_dir,
             pattern = "*.hdf$",
             full.names = TRUE,
             recursive = TRUE)

# Cloud fraction night, day (37, 39, respectively)
# indices <- c(37, 39)

# header <- "HDF4_EOS:EOS_SWATH:"
# suffix <- ":mod06:"
# parsing <- c("Cloud_Fraction_Day", "Cloud_Fraction_Night")
# filename <- files_mod06[1:24]
# parsinga <- sprintf("%s%s%s%s", header, filename, suffix, parsing[2])
# parsinga

# rectify_ref_stars <- function(ras) {
#   ras <- stars::read_stars(ras)
#   # ref_ext <- sf::st_bbox(ras)
#   # ref_ext <- ref_ext + c(-5L, 5L, -5L, 5L)
#   #ref_aoi <- terra::rast(ref_ext, resolution = 0.05)
#   rtd <- stars::st_warp(ras, crs = 4326, cellsize = 0.025)
#   return(rtd)
# }

# kk <- rectify_ref_stars(parsinga[1])
# pps <- split(parsinga, parsinga) |>
#   lapply(rectify_ref_stars) |>
#   lapply(terra::rast) |>
#   Reduce(f = terra::mosaic, x = _)
# plot(pps)


# rectify_ref <- function(ras) {
#   ras <- terra::rast(ras)
#   ref_ext <- terra::ext(ras)
#   ref_ext <- ref_ext + 5L
#   ref_aoi <- terra::rast(ref_ext, resolution = 0.05)
#   rtd <- terra::rectify(ras, method = "near", aoi = ref_aoi)
#   return(rtd)
# }

# pps <- split(parsinga, parsinga) |>
#   lapply(rectify_ref) |>
#   Reduce(f = terra::mosaic, x = _)

# cf1r <- terra::rectify(cf1, method = "near", aoi = ref)
# cf2r <- terra::rectify(cf2, method = "near")
# cf9r <- terra::rectify(cf9, method = "near")

mod06_vars <-
  calc_modis(
    files_mod06[1:48],
    product = "MOD06_L2",
    sites = sites,
    name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
    nthreads = 48L
  )
