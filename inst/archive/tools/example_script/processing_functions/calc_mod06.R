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

mod06_dir <- file.path(inputdir, "modis", "raw", "61", "MOD06_L2", year_target)
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



# sites_sf <- sf::st_as_sf(sites, coords= 2:3, crs = 4326)
# mod06_vars <-
#   calc_modis(
#     files_mod06[1:48],
#     product = "MOD06_L2",
#     sites = sites_sf,
#     name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
#     nthreads = 2L
#   )

# modecatch <- c("2018001", "2018002")
# radius <- c(0, 1e3L, 1e4L, 5e4L)
# product <- "MOD06_L2"
# doParallel::registerDoParallel(2L)

# resdf_mod06_surfref <-
# foreach(
#   datei = seq_along(modecatch),
#   .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr", "parallelly", "doParallel", "NRTAPmodel", "doRNG"),
#   .export = c("sites_sf", "files_mod06", "radius", "modecatch", "product"),
#   .combine = dplyr::bind_rows,
#   .errorhandling = "pass"
# ) %dorng% {
#     options(sf_use_s2 = FALSE)
#     #radius <- c(0L, 1e3L, 1e4L, 5e4L)
    
#     mod09_thisyear <- modecatch[datei]
#     mod09_thisyear <- as.Date(mod09_thisyear, format = "%Y%j")

#     radiusindex <- seq_along(radius)
#     radiuslist <- split(radiusindex, radiusindex)
#     nameflag <- c("MOD_06DYT_", "MOD_06NGT_")
#     res0 <-
#     lapply(radiuslist,
#       function(k) {
#         name_radius <- sprintf("%s%05d", nameflag, radius[k])
#         tryCatch({
#         inras <- modis_mosaic_mod06(files_mod06, mod09_thisyear)
        
#         extracted <- modis_worker(
#           raster = inras,
#           date = mod09_thisyear,
#           sites_in = sites_sf,
#           product = "MOD06_L2",
#           name_extracted = name_radius,
#           points = terra::vect(sites_sf),
#           id = "site_id",
#           radius = radius[k]
#           )
#         return(extracted)
#         }, error = function(e) {
#           error_df <- sf::st_drop_geometry(sites_sf)
#           error_df$date <- mod09_thisyear
#           error_df$remarks <- -99999
#           names(error_df)[which(names(error_df) == "remarks")] <- name_radius
#           return(error_df)
#         })
#       })
#     res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
#     return(res)
#   }

# testras <- modis_mosaic_mod06(files_mod06, as.Date("2018-01-02"))

# kk <- modis_mosaic_mod06(files_mod06, "2018-01-01")
# kx <-
#   modis_worker(testras,
#                as.Date("2018-01-01"),
#                sites_in = sites_sf,
#                name_extracted = c("MOD_CLCVD_0_10000", "MOD_CLCVN_0_10000"),
#                product = "MOD06_L2",
#                points = terra::vect(sites_sf),
#                radius = c(50000),
#                id = "site_id")
# kx2 <-
#   modis_worker(kk,
#                "2018-01-01",
#                sites_in = sites_sf,
#                name_extracted = c("MOD_CLCVD_0_10000", "MOD_CLCVN_0_10000"),
#                product = "MOD06_L2",
#                points = terra::vect(sites_sf),
#                radius = c(50000),
#                id = "site_id")
# kx3 <-
#   modis_worker(kk,
#                "2018-01-01",
#                sites_in = sites_sf,
#                name_extracted = c("MOD_CLCVD_0_10000", "MOD_CLCVN_0_10000"),
#                product = "MOD06_L2",
#                points = terra::vect(sites_sf),
#                radius = c(50000),
#                id = "site_id")
# kxl <- list(kx, kx2, kx3)
# kxldf <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), kxl)

# kxx <- terra::extract(kk, vect(sites_sf))


  calc_modis0 <-
  function(
    path,
    product = c("MOD11A1", "MOD13A2", "MOD06_L2",
                "VNP46A2", "MOD09GA", "MCD19A2"),
    sites,
    id_col = "site_id",
    name_covariates,
    radius = c(0L, 1e3L, 1e4L, 5e4L),
    subdataset = NULL,
    layers = NULL,
    fun_summary = "mean",
    nthreads = floor(parallelly::availableCores() / 2),
    tilelist = NULL,
    package_list_add = NULL,
    export_list_add = NULL
  ) {
    product <- match.arg(product)
    dates_available <-
      regmatches(path, regexpr("20\\d{2,2}/[0-3]\\d{2,2}", path))
    dates_available <- unique(dates_available)
    dates_available <- sub("/", "", dates_available)

    sites_input <- sites
    # default export list to minimize memory consumption per thread
    export_list <-
      c("path", "product", "sites_input", "name_covariates",
        "id_col", "fun_summary", "tilelist",
        "radius", "subdataset", "layers")
    # "modis_worker", "modis_get_vrt", "modis_preprocess_vnp46")
    package_list <-
      c("sf", "terra", "exactextractr", "foreach", "data.table",
        "NRTAPmodel", "scomps", "dplyr", "doRNG", "parallelly", "doParallel")
    if (!is.null(export_list_add)) {
      export_list <- c(export_list, export_list_add)
    }
    if (!is.null(package_list_add)) {
      package_list <- c(package_list, package_list_add)
    }

    cl <-
      parallelly::makeClusterPSOCK(
                                   nthreads,
                                   rscript_libs = .libPaths())
    doParallel::registerDoParallel(cl)

    calc_results <-
      foreach::foreach(
        datei = seq_along(dates_available),
        .packages = package_list,
        .export = export_list,
        .combine = dplyr::bind_rows,
        .errorhandling = "pass",
        .verbose = FALSE
      ) %dorng% {
        options(sf_use_s2 = FALSE)

        day_to_pick <- dates_available[datei]
        day_to_pick <- as.Date(day_to_pick, format = "%Y%j")

        radiusindex <- seq_along(radius)
        radiuslist <- split(radiusindex, radiusindex)

        # VNP46 corner assignment
        if (product == "VNP46A2") {
          vrt_today <-
            modis_preprocess_vnp46(
              paths = path,
              date_in = day_to_pick,
              tile_df = tilelist,
              subdataset = subdataset,
              crs_ref = "EPSG:4326"
            )
        } else if (product == "MOD06_L2") {
          vrt_today <-
            modis_mosaic_mod06(
                               paths = path,
                               date_in = day_to_pick)
        } else {
          vrt_today <-
            modis_get_vrt(
                          paths = path,
                          index_sds = subdataset,
                          product = product,
                          date_in = day_to_pick,
                          layers = layers)
        }
        if (terra::nlyr(vrt_today) != length(name_covariates)) {
          warning("The number of layers in the input raster do not match
                  the length of name_covariates.\n")
        }

        res0 <-
          lapply(radiuslist,
                 function(k) {
                   name_radius <-
                     sprintf("%s%05d",
                             name_covariates,
                             radius[k])

                   tryCatch({
                     extracted <-
                       modis_worker(
                         raster = vrt_today,
                         date = as.character(day_to_pick),
                         sites_in = sites_input,
                         product = product,
                         fun_summary_raster = fun_summary,
                         name_extracted = name_radius,
                         foo = scomps::extract_with_buffer,
                         points = terra::vect(sites_input),
                         id = id_col,
                         radius = radius[k]
                       )
                     return(extracted)
                   }, error = function(e) {
                     print(e)
                     error_df <- sf::st_drop_geometry(sites_input)
                     error_df$date <- day_to_pick
                     error_df$remarks <- -99999
                     names(error_df)[which(names(error_df) == "remarks")] <-
                       name_radius

                     return(error_df)
                   })
                 })
        res <-
          Reduce(\(x, y) {
                          dplyr::left_join(x, y,
                                           by = c("site_id", "date"))},
          res0)
        return(res)
      }
    Sys.sleep(3L)
    parallelly::killNode(cl)

    return(calc_results)
  }
mod06_vars <-
  calc_modis0(
    files_mod06,
    product = "MOD06_L2",
    sites = sites_sf,
    name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
    nthreads = 50L
  )

outfilename <- file.path(ddnhome, sprintf("NRTAP_Covars_MOD06_L2_%s.rds", year_target))
saveRDS(mod06_vars, outfilename, compress = "xz")


### consolidate
library(data.table)
tdir <- "~"
rdss <- list.files(tdir, "NRTAP_Covars_MOD06*.*.rds$", full.names = TRUE)
rdsd <- lapply(rdss, readRDS)
rdsdf <- data.table::rbindlist(rdsd)
saveRDS(rdsdf, file.path(tdir, "NRTAP_Covars_MOD06_2018_2022.rds"), compress = "xz")


mod09s <- list.files("input/modis/raw/61/MOD09GA/2018/241", "*.hdf", full.names = TRUE)
.libPaths("~/r-libs")
library(terra)
describe(mod09s[1], sds = TRUE)$var

terra::rast(mod09s[1], subds = 12:18)
