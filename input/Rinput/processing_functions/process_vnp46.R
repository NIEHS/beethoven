
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
source(file.path(prjhome, "R", "calculate_covariates.R"))
source(file.path(inputdir, "Rinput", "processing_functions", "filter_unique_sites.R"))
sites <- filter_unique_sites()
sites_sf <- sf::st_as_sf(as.data.frame(sites), coords = c("lon", "lat"),
        crs = "EPSG:4326")

###
# year_target <- 2018L
files_vnp46 <-
  list.files(file.path(inputdir, "modis", "raw", "5000", "VNP46A2", as.character(year_target)),
             "*.h5$", recursive = TRUE, full.names = TRUE)
# range: h05v03--h11v06
# -130, -120, 50, 60
# -70, -60, 20, 30
tile_def <-
  expand.grid(
    vaddr = sprintf("v%02d", 3:6),
    haddr = sprintf("h%02d", 5:11)
  )
tile_def$tile <- paste0(tile_def$haddr, tile_def$vaddr)
tile_def <- data.frame(tile = tile_def$tile)
tile_def$xmin <- rep(seq(-130, -70, 10), each = 4)
tile_def$xmax <- tile_def$xmin + 10
tile_def$ymin <- rep(seq(50, 20, -10), 7)
tile_def$ymax <- tile_def$ymin + 10

vnpnames <- sprintf("MOD_LGHTN_0_%05d", c(0, 1e3, 1e4, 5e4))
vnp_mean <- function(x) mean(x[x != 65535L])

vnp46_res <-
  calc_modis(
    paths = files_vnp46[1:200],
    product = "VNP46A2",
    sites_input = sites_sf,
    name_covariates = "MOD_LGHTN_0_",
    subdataset = 3L,
    fun_summary = vnp_mean,
    nthreads = 12L,
    tilelist = tile_def,#file.path(prjhome, "inst", "extdata", "modis_vnp46_tiles.csv"),
    export_list_add = c("tile_def", "fun_summary")
  )

targ_filename <- sprintf("NRTAP_Covars_MODIS_Nightlight_%s.rds", as.character(year_target))
saveRDS(vnp46_res, file.path(ddnhome, targ_filename))



##----
# OK
# xx = preprocess_modis_vnp46(files_vnp46, "2018-02-02", tile_def)
# modis_worker(files_vnp46[1:27], "2018-01-01", sites_sf, "N_01000", "VNP46A2", subdataset = 3L, tile_def_vnp46 = tile_def,
#   fun_summary_raster = function(x) mean(x[x!=65535L]),
#   foo = scomps::extract_with_buffer,
#   points = terra::vect(sites_sf),
#   id = "site_id",
#   radius = 5e4
# )

##----