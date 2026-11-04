
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
    paths = files_vnp46,
    product = "VNP46A2",
    sites_input = sites_sf,
    name_covariates = "MOD_LGHTN_0_",
    subdataset = 3L,
    fun_summary = "mean",
    nthreads = 64L,
    tilelist = tile_def
  )

targ_filename <- sprintf("NRTAP_Covars_MODIS_Nightlight_%s.rds", as.character(year_target))
saveRDS(vnp46_res, file.path(ddnhome, targ_filename))



# ##----
# # OK
# xx = preprocess_modis_vnp46(files_vnp46, "2018-02-02", tile_def)
# # xx2 <- xx
# # xx2[xx2 == 65535L] <- NA
# modis_worker(xx2, date = "2018-02-02", sites_sf, "N_VNP_01000", "VNP46A2", #subdataset = 3L, tile_def_vnp46 = tile_def,
#   fun_summary_raster = "mean",
#   foo = scomps::extract_with_buffer,
#   points = terra::vect(sites_sf),
#   id = "site_id",
#   radius = 5e4
# )


# # ok
# scomps::extract_with_buffer(points = sites_sf, surf = xx2, radius = 30000L, id = "site_id")

#   calc_modis(
#     paths = files_vnp46[1:200],
#     product = "VNP46A2",
#     sites_input = sites_sf,
#     radius = c(1000L, 10000L),
#     name_covariates = "MOD_LGHTN_0_",
#     subdataset = 3L,
#     fun_summary = "mean",
#     nthreads = 12L,
#     tilelist = tile_def
#   ) |> data.table::as.data.table()


#         res0 <-
#           lapply(1:3,
#                  function(k) {
#                    name_radius <-
#                      sprintf("%s%05d",
#                              "FAUX_VNP46_",
#                              c(1000, 10000, 50000)[k])

#                    tryCatch({
#                      extracted <-
#                        modis_worker(
#                          raster = xx2,
#                          date = "2018-02-02",
#                          sites_in = sites_sf,
#                          product = "VNP46A2",
#                          fun_summary_raster = "mean",
#                          name_extracted = name_radius,
#                          foo = scomps::extract_with_buffer,
#                          points = terra::vect(sites_sf),
#                          id = "site_id",
#                          radius = c(1000, 10000, 50000)[k]
#                        )
#                      return(extracted)
#                    }, error = function(e) {
#                      print(e)
#                      error_df <- sf::st_drop_geometry(sites_sf)
#                      error_df$date <- "2018-02-02"
#                      error_df$remarks <- -99999
#                      names(error_df)[which(names(error_df) == "remarks")] <-
#                        name_radius
#                     #  error_df$message <- e
#                     #  names(error_df)[which(names(error_df) == "remarks")] <-
#                     #    paste0(name_radius, "_error")
#                      return(error_df)
#                    })
#                  })
#         res <-
#           Reduce(\(x, y) {
#                           dplyr::left_join(x, y,
#                                            by = c("site_id", "date"))},
#           res0)


# 2021215, 216
# 2021-08-03 and 08-04
# download_modis_data(
#   "2021-08-03", "2021-08-04", product = "VNP46A2", horizontal_tiles = c(5, 12), vertical_tiles = c(3, 6), nasa_earth_data_token = readLines("~/.edtoken")[1], data_download_acknowledgement = TRUE, download = TRUE
# )





##----