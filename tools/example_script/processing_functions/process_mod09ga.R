# description: process modis data with
# buffer radii and the list of target covariates
# Insang Song
# Updated: 12/01/2023
# packages: parallelly, doParallel, scomps, terra, sf, exactextractr, foreach, data.table.
#   tigris
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
source(
  file.path(rootdir, userdir, "input/Rinput/processing_functions/filter_unique_sites.R"))

unique_sites <- filter_unique_sites(
  file.path(rootdir, userdir, "tests/testdata/daily_88101_2018-2022.rds"),
  file.path(rootdir, userdir, "R/manipulate_spacetime_data.R"))
# unique_sites_t <- filter_unique_sites(include_time = TRUE)
sites_sf <- sf::st_as_sf(unique_sites, coords = 2:3, crs = "EPSG:4326")


# check if remotes package is available
if (!require(pak)) {
  install.packages("pak")
  library(pak)
}

# install scalable_gis
if (!require(scomps)) {
  pak::pak("Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS")
}

pkgs <- c("terra", "sf", "doParallel", "parallel", "parallelly", "future.apply", "scomps", "exactextractr", "foreach", "data.table", "tigris", "doRNG")
suppressMessages(invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE)))
tigris_cache_dir("~/tigris_cache")
options(sf_use_s2 = FALSE, tigris_use_cache = TRUE)

radius <- c(0L, 1e3L, 1e4L, 5e4L)



## hierarchy (arbitrary subsets of census regions)
#
## path setting
# modis_mod13 <-
#   list.files(path = "~/projects/NRTAPModel/input/data/modis/raw/61/MOD13A2",
#              pattern = "*.hdf$",
#              recursive = TRUE,
#              full.names = TRUE)

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


#' @param flist character. All full hdf paths from list.files
#' @param index_sds integer(1). Index of subdataset
#' @param date_in Date(1). target date.
get_vrt_old <- function(
  flist, index_sds, date_in) {
  today <- as.character(date_in)
  year_today <- strftime(today, "%Y")
  jul_today <- strftime(today, "%j")
  dayjul <- paste0(year_today, "/", jul_today)
  ftarget <- grep(paste0(dayjul), flist, value = TRUE)
  terra::vrt(ftarget, options = c("-sd", index_sds), set_names = FALSE)
  }

#' Get virtual raster or mosaicked raster from multiple MODIS hdf files
#' @param flist character. Full list of hdf file paths.
#'  preferably a recursive search result from \code{list.files}.
#' @param index_sds integer(1). The index of subdataset to draw.
#' @param date_in Date(1). date to query.
#' @param foo closure. A function compatible with \code{SpatRaster}.
#' @return A SpatRaster object.
get_vrt <- function(
  flist, index_sds = NULL, date_in, foo = mean) {
  today <- as.character(date_in)
  year_today <- strftime(today, "%Y")
  jul_today <- strftime(today, "%j")
  dayjul <- paste0(year_today, "/", jul_today)
  ftarget <- grep(paste0(dayjul), flist, value = TRUE)
  layer_descr <- lapply(ftarget, \(x) terra::describe(x, sds = TRUE)[index_sds, c("name", "nlyr")])
  layer_target <- sapply(layer_descr, \(x) x[[1]])
  layer_number <- sapply(layer_descr, \(x) as.integer(x[[2]]))
  if (any(layer_number > 1)) {
    layer_target <- lapply(layer_target, \(x) terra::rast(x))
    layer_target <- lapply(layer_target, foo, na.rm = TRUE)
    # 12012023: terra::merge instead of terra::vrt due to C++ error
    do.call(terra::merge, layer_target)
  } else {
    terra::vrt(layer_target)
  }
}



modis_worker <- function(
  paths,
  date,
  sites_in = sites,
  name_extracted = NULL,
  subdataset = NULL,
  layers = NULL,
  foo = scomps::extract_with_buffer,
  ismod09 = FALSE,
  ...
) {
  # if (is.null(name_extracted)) {
  #   stop("No name of target raster is specified.\n")
  # }

  if (ismod09) {
    vrt_today <- mod09get(flist = paths, date_in = date, layers = 2:8)
  } else {
    if (is.null(subdataset)) {
      terra::describe(paths[1], sds = TRUE)
      stop("Please put relevant index for subdataset. The full list of subdatasets is above.\n")
    }
    vrt_today <- get_vrt(flist = paths, index_sds = subdataset, date_in = date, layers = layers)
  }

  if (any(grepl("00000", name_extracted))) {
    sites_in <- terra::vect(sites_in)
    sites_in <- terra::project(sites_in, terra::crs(vrt_today))
    extracted <- terra::extract(x = vrt_today, y = sites_in)
    sites_blank <- sf::st_drop_geometry(sites)
    extracted <- cbind(sites_blank, extracted)
  } else {
    extracted <- foo(..., surf = vrt_today)
  }

  # assuming that extracted is a data.frame
  extracted$date <- date
  name_offset <- terra::nlyr(vrt_today)
  name_range <- seq(ncol(extracted) - name_offset, ncol(extracted) - 1, 1)
  names(extracted)[name_range] <- name_extracted
  return(extracted)
}

## MOD09GA elements ####
modis_mod09 <-
  list.files(path = "~/projects/NRTAPModel/input/modis/raw/61/MOD09GA",
             pattern = "*.hdf$",
             recursive = TRUE,
             full.names = TRUE)
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

mod09dates <- stringi::stri_extract_first(modis_mod09, regex = "\\d{4,4}/\\d{3,3}")
mod09dates <- unique(mod09dates)
mod09dates <- stringi::stri_replace_all(mod09dates, fixed = "/", "")


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
# based on older version of calc_modis
source("R/calculate_covariates.R")
mod09_vars <-
  calc_modis(
    modis_mod09,
    product = "MOD09GA",
    sites = sites_sf,
    name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
    nthreads = 2L
  )


mod09_2018001 <-
modis_get_vrt(
  paths = modis_mod09[1:23],
  "MOD09GA",
  date_in = "2018-01-01",
  foo = "mean"
)

mod09_pnts <- terra::spatSample(mod09_2018001, 10000L, as.points = TRUE)
mod09_pnts <- terra::buffer(mod09_pnts, 10000L)
mod09_buf10_ext <- exactextractr::exact_extract(mod09_2018001, sf::st_as_sf(mod09_pnts), fun = "mean")

# CPU hang error check
terra::describe(modis_mod09[1], sds = TRUE)
sds_selector("MOD09GA", NULL)
regex_sds <- NULL
d1 <- sds_aggregate(modis_mod09[1], "MOD09GA", fun_agg = "mean")
d2 <- sds_aggregate(modis_mod09[2], "MOD09GA", fun_agg = "mean")
d3 <- sds_aggregate(modis_mod09[3], "MOD09GA", fun_agg = "mean")
object.size(values(d1))
d123 <- terra::vrt(d1, d2, d3)
object.size(d123)

d123l <- do.call(terra::merge, list(d1, d2, d3))

saveRDS(resdf_mod09_surfref,
    file = "/ddn/gs1/home/songi2/NRTAP_Covar_MOD09GA_B1_B7.rds")



for (i in 24:46) {
  assign(sprintf("d%02d", i),
         sds_aggregate(modis_mod09[i], "MOD09GA", fun_agg = "mean"),
         envir = .GlobalEnv)
}

d24461 <-
  terra::vrt(modis_mod09[24:46], options = c("-sd", "12"))
d24462 <-
  terra::vrt(modis_mod09[24:46], options = c("-sd", "13"))
d2446
d1 <- sds_aggregate(modis_mod09[24], "MOD09GA", fun_agg = "mean")
d2 <- sds_aggregate(modis_mod09[25], "MOD09GA", fun_agg = "mean")
d3 <- sds_aggregate(modis_mod09[26], "MOD09GA", fun_agg = "mean")
d4 <- sds_aggregate(modis_mod09[27], "MOD09GA", fun_agg = "mean")
d5 <- sds_aggregate(modis_mod09[28], "MOD09GA", fun_agg = "mean")
d15 <- terra::merge(d1, d2, d3, d4, d5)


system("find /tmp/Rtmp*  -maxdepth 2 -mmin -60")
