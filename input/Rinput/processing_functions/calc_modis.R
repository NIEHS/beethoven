#' Get mosaicked or merged raster from multiple MODIS hdf files
#' @param paths character. Full list of hdf file paths.
#'  preferably a recursive search result from \code{list.files}.
#' @param product character(1). Product code of MODIS. Should be one of
#' \code{c('MOD11A1', 'MOD13A2', 'MOD06_L2', 'VNP46A2', 'MOD09GA', 'MCD19A2')}
#' @param index_sds integer(1). The index of subdataset to draw.
#' @param layers integer. The index (indices) of layers if there are only
#' layers inside the input file.
#' @param date_in Date(1). date to query.
#' @param foo closure. A function compatible with \code{SpatRaster}.
#' @author Insang Song
#' @returns A SpatRaster object.
#' @export
get_vrt <- function(
  paths,
  product = c('MOD11A1', 'MOD13A2', 'MOD06_L2', 'VNP46A2', 'MOD09GA', 'MCD19A2'),
  index_sds = NULL,
  layers = NULL,
  date_in,
  foo = mean) {
  if (!is.character(paths)) {
    stop("Argument flist should be a list of hdf files (character).\n")
  }
  if (!is.numeric(index_sds)) {
    stop("Argument index_sds should be an integer.\n")
  }
  if (!is.function(foo)) {
    stop("Argument foo should be a function.\n")
  }
  product <- match.arg(product)

  if (startsWith(product, "MOD09")) {
    ismod09 <- TRUE
  }

  # interpret date
  today <- as.character(date_in)
  year_today <- strftime(today, "%Y")
  jul_today <- strftime(today, "%j")
  dayjul <- paste0(year_today, "/", jul_today)
  ftarget <- grep(paste0(dayjul), paths, value = TRUE)

  # get layer information
  layer_descr <- lapply(ftarget, \(x) terra::describe(x, sds = TRUE)[index_sds, c("name", "nlyr")])
  # get relevant subdataset only
  layer_target <- sapply(layer_descr, \(x) x[[1]])
  layer_number <- sapply(layer_descr, \(x) as.integer(x[[2]]))

  # if there are multiple layers in a subdataset
  if (any(layer_number > 1)) {
    if (ismod09) {
      layer_target <- lapply(ftarget, \(x) terra::rast(x, lyrs = layers))
    } else {
      layer_target <- lapply(layer_target, \(x) terra::rast(x))
    }
    layer_target <- lapply(layer_target, foo, na.rm = TRUE)
    # Merge multiple rasters into one
    # do.call(f, l) is equivalent to f(l[[1]], ... , l[[length(l)]])
    do.call(terra::merge, layer_target)
  } else {
    terra::vrt(layer_target)
  }
}


# mod09get <- function(
#   flist, date_in, layers = 2:8
# ) {
#   today <- as.character(date_in)
#   year_today <- strftime(today, "%Y")
#   jul_today <- strftime(today, "%j")
#   dayjul <- paste0(year_today, "/", jul_today)
#   ftarget <- grep(paste0(dayjul), flist, value = TRUE)
#   layer_target <- lapply(ftarget, \(x) terra::rast(x, lyrs = layers))
#   do.call(terra::merge, layer_target)
# }



#' Assign corner coordinates to retrieve a merged raster
#' @description This function will return a SpatRaster object with
#' georeferenced h5 files of VNP46A2 product.
#' @param filepaths character. Full paths of h5 files.
#' @param date character(1). Date to query.
#' @param tile_df prespecified data.frame with "tile" and "exts" columns,
#' where the former stores tile number (h00v00 format) and the latter
#' stores terra::ext output.
#' @param crs_ref character(1). terra::crs compatible CRS.
#' Default is "EPSG:4326"
#' @author Insang Song
assign_ext_vnp46 <- function(
  filepaths,
  date,
  tile_df = tile_def,
  crs_ref = "EPSG:4326"
) {
  if (is.character(date)) {
    if (nchar(date) != 10) {
      stop("Check the date format.\n")
    }
    date <- as.Date(date)
  }
  datejul <- strftime(date, format = "%Y/%j")
  stdtile <- tile_df$tile

  filepaths_today <- grep(as.character(datejul), filepaths, value = TRUE)
  # today's filenames
  # filepaths_tiles <-
  #     regmatches(filepaths_today,
  #                regexpr("h[0-9]+{2,2}v[0-9]+{2,2}", filepaths_today))
  filepaths_today <-
    grep(paste("(", 
               paste(stdtile, collapse = "|"), ")"),
         filepaths_today, value = TRUE)
  # print(filepaths_today)
  filepaths_today_tiles <-
    regmatches(filepaths_today,
               regexpr("h[0-9]+{2,2}v[0-9]+{2,2}", filepaths_today))

  vnp46_today <- unname(split(filepaths_today, filepaths_today))
  filepaths_today_tiles_list <-
    unname(split(filepaths_today_tiles, filepaths_today_tiles))

  vnp_assigned <-
    mapply(function(vnp, tile_in) {
      vnp_ <- terra::rast(vnp, subds = 3)
      tile_ext <- tile_df[tile_df$tile == tile_in, -1]
      # print(tile_ext)
      terra::crs(vnp_) <- terra::crs(crs_ref)
      terra::ext(vnp_) <- unlist(tile_ext)
      return(vnp_)
    }, vnp46_today, filepaths_today_tiles_list, SIMPLIFY = FALSE)
  vnp_all <- do.call(terra::merge, vnp_assigned)
  vnp_all[vnp_all == 65535] <- NA
  return(vnp_all)
}


#' A single-date MODIS worker for parallelization
#' @param paths character. Full list of hdf file paths.
#'  preferably a recursive search result from \code{list.files}.
#' @param date Date(1). date to query.
#' @param sites_in sf object. AQS sites.
#' @param name_extracted character. Names of calculated covariates.
#' @param product character(1). Product code of MODIS. Should be one of
#' \code{c('MOD11A1', 'MOD13A2', 'MOD06_L2', 'VNP46A2', 'MOD09GA', 'MCD19A2')}
#' @param subdataset integer(1). The index of subdataset to work with.
#' @param layers integer. The index (indices) of layers if there are only
#' layers inside the input file.
#' @param tile_def_vnp46 prespecified data.frame with "tile" and "exts"
#' columns, where the former stores tile number
#' (h00v00 format) and the latter stores terra::ext output.
#' @param fun_summary_raster function. Summary function for
#' multilayer rasters.
#' @param foo function. A calculation function working with
#' SpatRaster and sf.
#' @author Insang Song
#' @returns A SpatRaster object.
#' @export
modis_worker <- function(
  paths,
  date,
  sites_in = NULL,
  name_extracted = NULL,
  product = c('MOD11A1', 'MOD13A2', 'MOD06_L2', 'VNP46A2', 'MOD09GA', 'MCD19A2'),
  subdataset = NULL,
  layers = NULL,
  tile_def_vnp46 = NULL,
  fun_summary_raster = mean,
  foo = scomps::extract_with_buffer,
  ...
) {
  if (!is.function(foo)) {
    stop("Argument foo should be a function.\n")
  }
  product <- match.arg(product)
  if (is.null(subdataset)) {
    print(terra::describe(paths[1], sds = TRUE))
    stop("Please put relevant index for subdataset.
    The full list of subdatasets is above.\n")
  }

  # VNP46 corner assignment
  if (product == "VNP46A2") {
    vrt_today <-
      assign_ext_vnp46(
        filepaths = paths,
        date = date,
        tile_def = tile_def_vnp46,
        crs_ref = "EPSG:4326"
      )
  } else {
    vrt_today <-
      get_vrt(
        paths = paths,
        index_sds = subdataset,
        product = product,
        date_in = date,
        layers = layers)
  }


  if (any(grepl("00000", name_extracted))) {
    sites_tr <- terra::vect(sites_in)
    sites_tr <- terra::project(sites_tr, terra::crs(vrt_today))
    extracted <- terra::extract(x = vrt_today, y = sites_tr)
    sites_blank <- sf::st_drop_geometry(sites_in)
    extracted <- cbind(sites_blank, extracted)
  } else {
    extracted <- foo(..., surf = vrt_today)
  }

  # cleaning names
  # assuming that extracted is a data.frame
  extracted$date <- date
  name_offset <- terra::nlyr(vrt_today)
  # multiple columns will get proper names
  name_range <- seq(ncol(extracted) - name_offset, ncol(extracted) - 1, 1)
  colnames(extracted)[name_range] <- name_extracted
  return(extracted)
}



#' Calculate MODIS product covariates in multiple CPU threads
#' @param paths character. List of HDF files.
#' @param product character(1). MODIS product. Should be one of
#' \code{c('MOD11A1', 'MOD13A2', 'MOD06_L2', 'VNP46A2', 'MOD09GA', 'MCD19A2')}
#' @param sites_input sf object. Unique sites where covariates
#' will be calculated.
#' @param siteid character(1). Site identifier. Default is "site_id"
#' @param name_covariates character. Name header of covariates.
#' The calculated covariate names will have a form of
#' '{name_covariates}{zero-padded buffer radius in meters}',
#' e.g., 'MOD_NDVIF_0_50000' where 50 km radius circular buffer
#' was used to calculate mean NDVI value.
#' @param radius numeric. Radii to calculate covariates.
#' Default is c(0, 1000, 10000, 50000).
#' @param subdataset Index of subdataset.
#' @param layers Index of layers.
#' @param nthreads integer(1). Number of threads to be used
#'  to calculate covariates.
#' @param package_list_add character. A vector with package names to load
#'  these in each thread. Note that \code{sf}, \code{terra},
#'  \code{exactextractr}, \code{scomps}, and \code{dplyr}
#'  are the default packages to be loaded.
#' @param export_list_add character. A vector with object names to export
#'  to each thread. It should be minimized to spare memory.
#' @param foo function.
#' ...
#' @import foreach
#' @importFrom dplyr bind_rows
#' @importFrom doRNG `%dorng%`
#' @importFrom parallelly makeClusterPSOCK
#' @importFrom parallelly availableCores
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel stopCluster
calc_modis <-
  function(
    paths,
    product = c("MOD11A1", "MOD13A2", "MOD06_L2",
      "VNP46A2", "MOD09GA", "MCD19A2"),
    sites_input,
    siteid = "site_id",
    name_covariates,
    radius = c(0L, 1e3L, 1e4L, 5e4L),
    subdataset,
    layers = NULL,
    nthreads = floor(parallelly::availableCores() / 2),
    package_list_add = NULL,
    export_list_add = NULL
  ) {
    product <- match.arg(product)
    dates_available <-
      regmatches(paths, regexpr("20\\d{2,2}[0-3]\\d{2,2}", paths))
    dates_available <- unique(dates_available)

    # default export list to minimize memory consumption per thread
    export_list <- c("paths", "product", "sites_input", "name_covariates",
      "siteid",
      "radius", "subdataset", "layers", "modis_worker", "get_vrt",
      "dates_available")
    package_list <-
      c("sf", "terra", "exactextractr", "foreach", "scomps", "dplyr", "doRNG")
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
                         paths = paths,
                         date = day_to_pick,
                         sites_in = sites_input,
                         product = product,
                         subdataset = subdataset,
                         name_extracted = name_radius,
                         points = terra::vect(sites_input),
                         id = siteid,
                         radius = radius[k]
                       )
                     return(extracted)
                   }, error = function(e) {
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
    parallel::stopCluster(cl)
    rm(cl)

    return(calc_results)
  }

modis_mod13 <- list.files(
  path = "/ddn/gs1/home/songi2/projects/NRTAPModel/input/data/modis/raw/61/MOD13A2",
  pattern = "*.hdf$",
  full.names = TRUE,
  recursive = TRUE
  )


## Put these codes to tests
mod13vrt <- get_vrt(modis_mod13[1:23], product = "MOD13A2", index_sds = 1, date_in = as.Date("2018-01-01"))

expect_s4_class(mod13vrt, "SpatRaster")
expect_equal(terra::nlyr(mod13vrt), 1L)



expect_no_error(modextr <-
  modis_worker(
    paths = modis_mod13[23 + 1:23],
    date = "2018-01-17",
    sites_in = terra::vect(sites),
    product = "MOD13A2",
    name_extracted = "MOD_NDVIV_0_10000",
    subdataset = 1,
    points = terra::vect(sites),
    radius = radius[3],
    id = "site_id"
))

expect_error(
  modis_worker(
  paths = modis_mod13[23 + 1:23],
  date = "2018-01-17",
  sites_in = terra::vect(sites),
  product = "MOD13A2",
  name_extracted = "MOD_NDVIV_0_10000",
  subdataset = 1,
  foo = "tomahawk",
  points = terra::vect(sites),
  radius = radius[3],
  id = "site_id"
)
)

expect_s3_class(modextr, "data.frame")
expect_true(all(c("date", "MOD_NDVIV_0_10000", "site_id") %in% colnames(modextr)))



expect_no_error(
  resdf_ndvi0 <-
    calc_modis(
      paths = modis_mod13[1:46],
      product = "MOD13A2",
      sites_input = sites,
      name_covariates = "MOD_NDVIV_0_",
      radius = 1e4,
      subdataset = 1,
      layers = NULL,
      nthreads = 2L
    )
)

expect_no_error(
  resdf_ndvi0_half <-
    calc_modis(
      paths = modis_mod13[c(1:15, 32:46)],
      product = "MOD13A2",
      sites_input = sites,
      name_covariates = "MOD_NDVIV_0_",
      radius = 1e4,
      subdataset = 1,
      layers = NULL,
      nthreads = 2L
    )
)


expect_s3_class(resdf_ndvi0, "data.frame")
expect_true(all(c("date", "MOD_NDVIV_0_10000", "site_id") %in% colnames(resdf_ndvi0)))
expect_true(any(resdf_ndvi0_half$MOD_NDVIV_0_10000 == -99999, ))
