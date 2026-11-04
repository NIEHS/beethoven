# description: process modis data with
# buffer radii and the list of target covariates
# Insang Song
# Updated: 12/01/2023
# packages: parallelly, doParallel, scomps, terra, sf, exactextractr, foreach, data.table.
#   tigris

# to look at the path and package settings,
# consult "load_packages.r"
source("./input/Rinput/processing_functions/load_packages.R")

# if (!dir.exists(download_dir)) {
#   dir.create(download_dir)
# }
# if (!dir.exists(download_dir_user)) {
#   dir.create(download_dir_user)
# }

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



#####
mod13dates <- seq(1, 366, 16)
mod13_2018 <- sprintf("%d%03d", 2018, mod13dates)
mod13_2018 <- as.Date(mod13_2018, format = "%Y%j")
mod13_2018

resdf_ndvi <-
foreach(
  datei = seq_along(mod13dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mod13", "mod13dates", "modis_worker", "get_vrt", "radius"),
  .combine = "rbind"
) %dorng% {
  foreach(
    year = seq(2018, 2022),
    .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
    .export = c("sites", "modis_mod13", "mod13dates", "modis_worker", "datei", "get_vrt"),
    .combine = "rbind"
  ) %do% {
    options(sf_use_s2 = FALSE)
    radius = c(0L, 1e3L, 1e4L, 5e4L)
    
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
resdf_ndvi_c <- resdf_ndvi
resdf_ndvi_c[, c(3,5,6,7)] <- resdf_ndvi_c[, c(3,5,6,7)] / 1e8

saveRDS(resdf_ndvi,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MOD13A2_NDVI.rds")




### MOD11A1
modis_mod11 <-
  list.files(path = "~/projects/NRTAPModel/input/data/modis/raw/61/MOD11A1",
             pattern = "*.hdf$",
             recursive = TRUE,
             full.names = TRUE)
# stringi::stri_extract_first(modis_mod11, regex = "\\d{4,4}/\\d{3,3}")
# terra::describe(modis_mod11[1], sds = T)

mod11dates <- stringi::stri_extract_first(modis_mod11, regex = "\\d{4,4}/\\d{3,3}")
mod11dates <- unique(mod11dates)
mod11dates <- stringi::stri_replace_all(mod11dates, fixed = "/", "")

resdf_mod11day <-
foreach(
  datei = seq_along(mod11dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mod11", "mod11dates", "modis_worker", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)
    radius = c(0L, 1e3L, 1e4L, 5e4L)

    mod11_thisyear <- mod11dates[datei]
    mod11_thisyear <- as.Date(mod11_thisyear, format = "%Y%j")

    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)

    res0 <-
    lapply(radiuslist,
      function(k) {
        tryCatch({
          extracted <- modis_worker(
            paths = modis_mod11,
            date = mod11_thisyear,
            subdataset = 1,
            name_extracted = sprintf("MOD_SFCTD_0_%05d", radius[k]),
            points = terra::vect(sites),
            id = "site_id",
            radius = radius[k]
            )
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mod11_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- sprintf("MOD_SFCTD_0_%05d", radius[k])
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
  }

saveRDS(resdf_mod11day,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MOD11A2_Day.rds")
gc()




resdf_mod11night <-
foreach(
  datei = seq_along(mod11dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mod11", "mod11dates", "modis_worker", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)
    radius = c(0L, 1e3L, 1e4L, 5e4L)

    mod11_thisyear <- mod11dates[datei]
    mod11_thisyear <- as.Date(mod11_thisyear, format = "%Y%j")

    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)

    res0 <-
    lapply(radiuslist,
      function(k) {
        tryCatch({
        extracted <- modis_worker(
          paths = modis_mod11,
          date = mod11_thisyear,
          subdataset = 5,
          name_extracted = sprintf("MOD_SFCTN_0_%05d", radius[k]),
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mod11_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- sprintf("MOD_SFCTN_0_%05d", radius[k])
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
  }
saveRDS(resdf_mod11night,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MOD11A2_Night.rds")


## MCD19A2 elements ####
modis_mcd19 <-
  list.files(path = "~/projects/NRTAPModel/input/data/modis/raw/61/MCD19A2",
             pattern = "*.hdf$",
             recursive = TRUE,
             full.names = TRUE)
# stringi::stri_extract_first(modis_mod11, regex = "\\d{4,4}/\\d{3,3}")
# terra::describe(modis_mcd19[1], sds = T)
# 1, 2, 9:13
#  1 MOD_AD4TA_0_00000
#  2 MOD_AD5TA_0_00000
#  9 MOD_CSZAN_0_00000
# 10 MOD_CVZAN_0_00000
# 11 MOD_RAZAN_0_00000
# 12 MOD_SCTAN_0_00000
# 13 MOD_GLNAN_0_00000

mcd19dates <- stringi::stri_extract_first(modis_mcd19, regex = "\\d{4,4}/\\d{3,3}")
mcd19dates <- unique(mcd19dates)
mcd19dates <- stringi::stri_replace_all(mcd19dates, fixed = "/", "")


resdf_mcd19_aod047 <-
foreach(
  datei = seq_along(mcd19dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mcd19", "mcd19dates", "modis_worker", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)
    radius = c(0L, 1e3L, 1e4L, 5e4L)
    
    mcd19_thisyear <- mcd19dates[datei]
    mcd19_thisyear <- as.Date(mcd19_thisyear, format = "%Y%j")

    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)
    nameflag <- "MOD_AD4TA_0_"
    res0 <-
    lapply(radiuslist,
      function(k) {
        name_radius <- sprintf("%s%05d", nameflag, radius[k])
        tryCatch({
        extracted <- modis_worker(
          paths = modis_mcd19,
          date = mcd19_thisyear,
          subdataset = 1,
          name_extracted = name_radius,
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
        return(extracted)
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mcd19_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- name_radius
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
  }

saveRDS(resdf_mcd19_aod047,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MCD19A2_AOD047.rds")


##
resdf_mcd19_aod055 <-
foreach(
  datei = seq_along(mcd19dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mcd19", "mcd19dates", "modis_worker", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)

    mcd19_thisyear <- mcd19dates[datei]
    mcd19_thisyear <- as.Date(mcd19_thisyear, format = "%Y%j")


    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)
    nameflag <- "MOD_AD5TA_0_"

    res0 <-
    lapply(radiuslist,
      function(k) {
        name_radius <- sprintf("%s%05d", nameflag, radius[k])
        tryCatch({
        extracted <- modis_worker(
          paths = modis_mcd19,
          date = mcd19_thisyear,
          subdataset = 2,
          name_extracted = name_radius,
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
         return(extracted)
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mcd19_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- name_radius
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
    
  }

saveRDS(resdf_mcd19_aod055,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MCD19A2_AOD055.rds")

###
resdf_mcd19_csz <-
foreach(
  datei = seq_along(mcd19dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr", "parallelly"),
  .export = c("sites", "modis_mcd19", "mcd19dates", "modis_worker", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)
    
    mcd19_thisyear <- mcd19dates[datei]
    mcd19_thisyear <- as.Date(mcd19_thisyear, format = "%Y%j")

    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)
    nameflag <- "MOD_CSZAN_0_"

    res0 <-
    lapply(radiuslist,
      function(k) {
        name_radius <- sprintf("%s%05d", nameflag, radius[k])
        tryCatch({
        extracted <- modis_worker(
          paths = modis_mcd19,
          date = mcd19_thisyear,
          subdataset = 9,
          name_extracted = name_radius,
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
        return(extracted)
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mcd19_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- name_radius
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
  }

saveRDS(resdf_mcd19_csz,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MCD19A2_CSZ.rds")



resdf_mcd19_cvz <-
foreach(
  datei = seq_along(mcd19dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mcd19", "mcd19dates", "modis_worker", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)

    mcd19_thisyear <- mcd19dates[datei]
    mcd19_thisyear <- as.Date(mcd19_thisyear, format = "%Y%j")

    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)
    nameflag <- "MOD_CVZAN_0_"

    res0 <-
    lapply(radiuslist,
      function(k) {
        name_radius <- sprintf("%s%05d", nameflag, radius[k])
        tryCatch({
        extracted <- modis_worker(
          paths = modis_mcd19,
          date = mcd19_thisyear,
          subdataset = 10,
          name_extracted = name_radius,
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
        return(extracted)
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mcd19_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- name_radius
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
  }
saveRDS(resdf_mcd19_cvz,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MCD19A2_CVZ.rds")


resdf_mcd19_raz <-
foreach(
  datei = seq_along(mcd19dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mcd19", "mcd19dates", "modis_worker", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)

    mcd19_thisyear <- mcd19dates[datei]
    mcd19_thisyear <- as.Date(mcd19_thisyear, format = "%Y%j")

    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)
    nameflag <- "MOD_RAZAN_0_"

    res0 <-
    lapply(radiuslist,
      function(k) {
        name_radius <- sprintf("%s%05d", nameflag, radius[k])
        tryCatch({
        extracted <- modis_worker(
          paths = modis_mcd19,
          date = mcd19_thisyear,
          subdataset = 11,
          name_extracted = name_radius,
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
        return(extracted)
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mcd19_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- name_radius
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
  }
saveRDS(resdf_mcd19_raz,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MCD19A2_RAZ.rds")




resdf_mcd19_sct <-
foreach(
  datei = seq_along(mcd19dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mcd19", "mcd19dates", "modis_worker", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)

    mcd19_thisyear <- mcd19dates[datei]
    mcd19_thisyear <- as.Date(mcd19_thisyear, format = "%Y%j")

    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)
    nameflag <- "MOD_SCTAN_0_"
    
    res0 <-
    lapply(radiuslist,
      function(k) {
        name_radius <- sprintf("%s%05d", nameflag, radius[k])
        tryCatch({
        extracted <- modis_worker(
          paths = modis_mcd19,
          date = mcd19_thisyear,
          subdataset = 12,
          name_extracted = name_radius,
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
        return(extracted)
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mcd19_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- name_radius
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
  }
saveRDS(resdf_mcd19_sct,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MCD19A2_SCT.rds")




resdf_mcd19_gln <-
foreach(
  datei = seq_along(mcd19dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mcd19", "mcd19dates", "modis_worker", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)

    mcd19_thisyear <- mcd19dates[datei]
    mcd19_thisyear <- as.Date(mcd19_thisyear, format = "%Y%j")

    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)
    nameflag <- "MOD_GLNAN_0_"

    res0 <-
    lapply(radiuslist,
      function(k) {
        name_radius <- sprintf("%s%05d", nameflag, radius[k])
        tryCatch({
        extracted <- modis_worker(
          paths = modis_mcd19,
          date = mcd19_thisyear,
          subdataset = 13,
          name_extracted = name_radius,
          points = terra::vect(sites),
          id = "site_id",
          radius = radius[k]
          )
        return(extracted)
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mcd19_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- name_radius
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
  }
saveRDS(resdf_mcd19_gln,
  file = "/ddn/gs1/home/songi2/NRTAP_Covar_MCD19A2_GLN.rds")






## MOD09GA elements ####
modis_mod09 <-
  list.files(path = "~/projects/NRTAPModel/input/data/modis/raw/61/MOD09GA",
             pattern = "*.hdf$",
             recursive = TRUE,
             full.names = TRUE)
# stringi::stri_extract_first(modis_mod11, regex = "\\d{4,4}/\\d{3,3}")
terra::describe(modis_mod09[1], sds = T)
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


cl <-
  parallelly::makeClusterPSOCK(
    30L, rscript_libs = .libPaths())
registerDoParallel(cl)




resdf_mod09_surfref <-
foreach(
  datei = seq_along(mod09dates),
  .packages = c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr"),
  .export = c("sites", "modis_mod09", "mod09dates", "modis_worker", "mod09get", "get_vrt", "radius"),
  .combine = dplyr::bind_rows,
  .errorhandling = "pass"
) %dorng% {
    options(sf_use_s2 = FALSE)
    radius = c(0L, 1e3L, 1e4L, 5e4L)
    
    mod09_thisyear <- mod09dates[datei]
    mod09_thisyear <- as.Date(mod09_thisyear, format = "%Y%j")

    radiusindex <- seq_along(radius)
    radiuslist <- split(radiusindex, radiusindex)
    nameflag <- sprintf("MOD_SFCRF_%d_", seq(1, 7))
    res0 <-
    lapply(radiuslist,
      function(k) {
        name_radius <- sprintf("%s%05d", nameflag, radius[k])
        tryCatch({
        extracted <- modis_worker(
          paths = modis_mod09,
          date = mod09_thisyear,
          subdataset = NULL,
          name_extracted = name_radius,
          points = terra::vect(sites),
          id = "site_id",
          layers = seq(2, 8),
          radius = radius[k],
          ismode09 = TRUE
          )
        return(extracted)
        }, error = function(e) {
          error_df <- sf::st_drop_geometry(sites)
          error_df$date <- mod09_thisyear
          error_df$remarks <- -99999
          names(error_df)[which(names(error_df) == "remarks")] <- name_radius
          return(error_df)
        })
      })
    res <- Reduce(\(x, y) dplyr::left_join(x, y, by = c("site_id", "date")), res0)
    return(res)
  }

# mod09get(modis_mod09, "2022-01-04")
test <- modis_worker(paths = modis_mod09, 
  "2022-01-04",
  name_extracted = sprintf("%s%05d", paste0("B", 1:7), 1e4L),
  layers = 2:8,
  ismod09 = TRUE,
  id = "site_id",
  points = terra::vect(sites),
  radius = 1e3)
