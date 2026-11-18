#   tigris

year <- commandArgs(TRUE)[[1]]
threads <- commandArgs(TRUE)[[2]]


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

# if (!dir.exists(download_dir)) {
#   dir.create(download_dir)
# }
# if (!dir.exists(download_dir_user)) {
#   dir.create(download_dir_user)
# }

# register custom library path for debugging on Triton
pkgs <- c("terra", "sf", "doParallel", "parallelly", "future.apply", "exactextractr", "foreach", "data.table", "doRNG")
suppressMessages(invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE)))
# tigris_cache_dir("~/tigris_cache")
options(sf_use_s2 = FALSE)

# filter unique sites
# source(
#   file.path(rootdir, userdir, "input/Rinput/processing_functions/filter_unique_sites.R"))

# unique_sites <- filter_unique_sites(
#   file.path(rootdir, userdir, "tests/testdata/daily_88101_2018-2022.rds"),
#   file.path(rootdir, userdir, "R/manipulate_spacetime_data.R"))
# sites_sf <- sf::st_as_sf(unique_sites, coords = 2:3, crs = "EPSG:4326")
sites_sf <- readRDS(file.path(rootdir, "home/songi2", "sites_unique.rds"))

# install scalable_gis
# if (!require(scomps)) {
#   pak::pak("Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS")
# }

radius <- c(0L, 1e3L, 1e4L, 5e4L)



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




# cl <-
#   parallelly::makeClusterPSOCK(
#     30L, rscript_libs = .libPaths())
# registerDoParallel(cl)


# mod09get <- function(
#   flist, date_in, layers = 2:8
# ) {
#   today <- as.character(date_in)
#   year_today <- strftime(today, "%Y")
#   jul_today <- strftime(today, "%j")
#   dayjul <- paste0(year_today, "/", jul_today)
#   ftarget <- grep(paste0(dayjul), flist, value = TRUE)
#   layer_target <- lapply(ftarget, \(x) terra::rast(x, lyrs = layers))
#   #terra::vrt(ftarget, options = opt_list)
#   do.call(terra::merge, layer_target)
# }

# resdf_mod09_surfref <-
# foreach(
#   datei = seq_along(mod09dates),
#   .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr", "parallelly"),
#   .export = c("sites", "modis_mod09", "mod09dates", "modis_worker", "mod09get", "get_vrt", "radius"),
#   .combine = dplyr::bind_rows,
#   .errorhandling = "pass"
# ) %dorng% {
#     options(sf_use_s2 = FALSE)
#     #radius <- c(0L, 1e3L, 1e4L, 5e4L)
    
#     mod09_thisyear <- mod09dates[datei]
#     mod09_thisyear <- as.Date(mod09_thisyear, format = "%Y%j")

#     radiusindex <- seq_along(radius)
#     radiuslist <- split(radiusindex, radiusindex)
#     nameflag <- sprintf("MOD_SFCRF_%d_", seq(1, 7))
#     res0 <-
#     lapply(radiuslist,
#       function(k) {
#         name_radius <- sprintf("%s%05d", nameflag, radius[k])
#         tryCatch({
#         extracted <- modis_worker(
#           paths = modis_mod09,
#           date = mod09_thisyear,
#           subdataset = NULL,
#           name_extracted = name_radius,
#           points = terra::vect(sites),
#           id = "site_id",
#           layers = seq(2, 8),
#           radius = radius[k],
#           ismod09 = TRUE
#           )
#         return(extracted)
#         }, error = function(e) {
#           error_df <- sf::st_drop_geometry(sites)
#           error_df$date <- mod09_thisyear
#           error_df$remarks <- -99999
#           names(error_df)[which(names(error_df) == "remarks")] <- name_radius
#           return(error_df)
#         })
#       })
#     res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
#     return(res)
#   }
test <- FALSE
if (test) year <- 2018; threads <- 32L

# mod09_dir <- file.path(rootdir, userdir, inputdir, "modis", "raw", "61", "MOD09GA")

# files_mod09 <-
#   list.files(path = mod09_dir,
#              pattern = "*.hdf$",
#              full.names = TRUE,
#              recursive = TRUE)
# writeLines(files_mod09, "~/rtest/mod09filelist.txt")


modis_mod09 <- readLines(file.path(rootdir, "home/songi2", "rtest/mod09filelist.txt"))
modis_mod09y <-
  grep(sprintf("/%d/", as.integer(year)), modis_mod09, value = TRUE)

source(file.path(rootdir, userdir, "R/calculate_covariates.R"))
source(file.path(rootdir, userdir, "R/manipulate_spacetime_data.R"))
list_foo <- c("modis_get_vrt", "modis_preprocess_vnp46", "modis_worker", "modis_mosaic_mod06", "is_stdt")
# HDF4_EOS:EOS_GRID:"/ddn/gs1/home/songi2/projects/NRTAPModel/input/modis/raw/61/MOD09GA/2018/001/MOD09GA.A2018001.h07v05.061.2021295010454.hdf":MODIS_Grid_500m_2D:sur_refl_b01_1
layertemplate <- "HDF4_EOS:EOS_GRID:%s:MODIS_Grid_500m_2D:sur_refl_b%02d_1"


llist <- seq(1, 7)
llist <- split(llist, llist)


# system.time(
#   out <-
#   modis_worker(
#     modis_get_vrt(
#       sprintf(layertemplate, modis_mod09[1:23], 1),
#       "MOD09GA",
#       "2018-01-01"
#     ),
#     "2018-01-01",
#     sites_sf,
#     "MOD09_L1_50000",
#     radius = 50000)
#  )


mod09_vars <-
  lapply(
    llist,
    function(x) {
      mod09path <- sprintf(layertemplate, modis_mod09y, x)
      calced <- calc_modis(
        mod09path,
        product = "MOD09GA",
        sites = sites_sf,
        name_covariates = sprintf("MOD_SFRF%d_0_", x),
        nthreads = as.integer(threads),
        export_list_add = list_foo
      )
      return(calced)
    }
  )
# 11:20 started
Sys.time()

mod09_vars_df <-
  Reduce(
    function(x, y) dplyr::left_join(x, y, by = c("site_id", "time")),
    mod09_vars
  )

saveRDS(
  mod09_vars_df,
  file.path(
    rootdir,
    "home/songi2",
    sprintf("NRTAP_Covar_MOD09GA_B1_B7_%d.rds", as.integer(year))
  ),
  compress = "xz"
)
# mod09_vars1 <-
#   calc_modis(
#     sprintf(layertemplate, modis_mod09y[1:23], 1),
#     product = "MOD09GA",
#     sites = sites_sf,
#     name_covariates = sprintf("MOD_SFCRF_%d_", 1),
#     nthreads = 1L,
#     export_list_add = list_foo
#   )


# mod09_2020001 <-
#     modis_get_vrt(
#     paths = sprintf(layertemplate, modis_mod09y[1:23], 1),
#     "MOD09GA",
#     date_in = "2020-01-01",
#     foo = "mean"
#     )

# mod09_2020002 <-
#     modis_get_vrt(
#     paths = sprintf(layertemplate, modis_mod09y[1:23 + 23], 1),
#     "MOD09GA",
#     date_in = "2020-01-02",
#     foo = "mean"
#     )


# sites_sft <- sites_sf |> dplyr::mutate(time = "2020-01-01")
# mod09_ex1 <- modis_worker(mod09_2020001, "2020-01-01", sites_in = sites_sf, name_extracted = sprintf("MOD09_Extr_%d_%05d", 1, 10000), product = "MOD09GA", radius = 10000L)
# mod09_ex2 <- modis_worker(mod09_2020002, "2020-01-02", sites_in = sites_sf, name_extracted = sprintf("MOD09_Extr_%d_%05d", 1, 10000), product = "MOD09GA", radius = 10000L)



# radiuslist <- list(1, 2)
# radius <- c(1000, 5000)
# name_covariates <- "mod_comp_1_"
#         res0 <-
#           lapply(radiuslist,
#             function(k) {
#               name_radius <-
#                 sprintf("%s%05d",
#                         name_covariates,
#                         radius[k])

#               tryCatch({
#                 extracted <-
#                   modis_worker(
#                     raster = mod09_2020001,
#                     date = as.character("2020-01-01"),
#                     sites_in = sites_sf,
#                     product = "MOD09GA",
#                     fun_summary_raster = "mean",
#                     name_extracted = name_radius,
#                     id_col = "site_id",
#                     radius = radius[k]
#                   )
#                 return(extracted)
#               }, error = function(e) {
#                 return(e)
#                 name_radius <-
#                   sprintf("%s%05d",
#                           name_covariates,
#                           radius[k])
#                 error_df <- sf::st_drop_geometry(sites_sf)
#                 if (!"time" %in% names(error_df)) {
#                   error_df$time <- "2020-01-01"
#                 }
#                 # coerce to avoid errors
#                 error_df <- as.data.frame(error_df)
#                 error_df <- error_df[, c(id_col, "time")]
#                 error_df[, name_radius] <- -99999
#                 return(error_df)
#               }
#               )
#             }
#           )

# res000 <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "time")), res0)
