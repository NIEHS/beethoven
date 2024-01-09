#' Calculate covariates
#' @param covariate character(1). Covariate type.
#' @param path character. Single or multiple path strings.
#' @param sites sf/SpatVector. Unique sites. Should include
#'  a unique identifier field named \code{id_col}
#' @param id_col character(1). Name of unique identifier.
#'  Default is \code{'site_id'}.
#' @param ... Arguments passed to each covariate calculation
#'  function.
#' @seealso
#' - \link{calc_modis}: "modis", "MODIS"
#' - \link{calc_koppen_geiger}: "koppen-geiger", "koeppen-geiger", "koppen",
#' - \link{calc_ecoregion}: "ecoregion", "ecoregions"
#' - \link{calc_temporal_dummies}: "dummies"
#' @returns Calculated covariates. Mainly data.frame object.
#' @author Insang Song
#' @export
calc_covariates <-
  function(
      covariate = c("modis", "koppen-geiger",
                    "koeppen-geiger", "koppen", "koeppen",
                    "geos", "dummies", "gmted", "roads",
                    "sedac_groads", "nlcd", "tri", "ncep", "aadt",
                    "ecoregions", "ecoregion"),
      path = "./input/koppen_geiger/raw/Beck_KG_V1_present_0p0083.tif",
      sites,
      id_col = "site_id",
      ...) {

    covariate <- tolower(covariate)
    covariate <- match.arg(covariate)
    if (startsWith(covariate, "ko")) {
      covariate <- "koppen"
    }

    # select function to run
    what_to_run <- switch(covariate,
      modis = calc_modis,
      ecoregion = calc_ecoregion,
      ecoregions = calc_ecoregion,
      koppen = calc_koppen_geiger,
      narr_monolevel = calc_narr_monolevel,
      monolevel = calc_narr_monolevel,
      narr_p_levels = calc_narr_p_levels,
      p_levels = calc_narr_p_levels,
      plevels = calc_narr_p_levels,
      nlcd = calc_nlcd_ratio,
      noaa = calc_noaa_hms,
      smoke = calc_noaa_hms,
      hms = calc_noaa_hms,
      sedac_groads = calc_sedac_groads,
      roads = calc_sedac_groads,
      sedac_population = calc_sedac_population,
      population = calc_sedac_population,
      aadt = calc_aadt,
      tri = calc_tri,
      ncep = calc_ncep,
      geos = calc_geos,
      gmted = calc_gmted,
      dummies = calc_temporal_dummies
    )

    res_covariate <-
      tryCatch({
        what_to_run(
          path = path,
          sites = sites,
          id_col = id_col,
          ...
        )
      }, error = function(e) {
        print(e)
        print(args(what_to_run))
        message(paste0("Please refer to the argument list and
                        the error message above to rectify the error.\n"))
        return(NULL)
      })

    return(res_covariate)
  }


#' Calculate Koeppen-Geiger climate zone binary variables
#' @param path character(1). Path to Koppen-Geiger
#'  climate zone raster file
#' @param sites sf/SpatVector. Unique sites. Should include
#'  a unique identifier field named \code{id_col}
#' @param id_col character(1). Name of unique identifier.
#' @returns a data.frame object
#' @author Insang Song
#' @importFrom terra vect
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra extract
#' @importFrom terra coltab
#' @importFrom terra merge
#' @importFrom methods is
#' @export
calc_koppen_geiger <-
  function(
      path = "./input/koppen_geiger/raw/Beck_KG_V1_present_0p0083.tif",
      sites,
      id_col = "site_id") {
    ## You will get "sites" in memory after sourcing the file above
    kg_rast <- terra::rast(path)
    sites_tr <- sites

    if (!methods::is(sites, "SpatVector")) {
      sites_tr <- terra::vect(sites)
    }
    sites_kg <- terra::project(sites_tr, terra::crs(kg_rast))
    sites_kg_extract <- terra::extract(kg_rast, sites_kg)

    # The starting value is NA as the color table has 0 value in it
    kg_class <-
      c(
        NA, "Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", "Csa", "Csb",
        "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc",
        "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
        "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF"
      )
    kg_coltab <- terra::coltab(kg_rast)
    kg_coltab <- kg_coltab[[1]][seq(1, 31), ]
    kg_colclass <- data.frame(
      value = kg_coltab$value,
      class_kg = kg_class
    )

    sites_kg_extract[[id_col]] <- unlist(sites_kg[[id_col]])
    colnames(sites_kg_extract)[2] <- "value"
    sites_kg_extract_e <- merge(sites_kg_extract, kg_colclass, by = "value")

    # "Dfa": 25
    # "BSh": 6
    # "Dfb": 26
    id_search <- unlist(sites_kg_extract_e[[id_col]])
    # errorfix: how to generalize and auto-fix it?
    sites_kg_extract_e[
      which(id_search == "44009000788101"),
      "class_kg"
    ] <- "Dfa"
    sites_kg_extract_e[
      which(id_search == "48061200488101"),
      "class_kg"
    ] <- "BSh"
    sites_kg_extract_e[
      which(id_search == "33015001488101"),
      "class_kg"
    ] <- "Dfb"

    sites_kg_extract_e$class_kg <-
      as.factor(substr(sites_kg_extract_e$class_kg, 1, 1))
    # currently there are no "E" region in sites.
    # however, E is filled with all zeros at the moment.
    aelabels <- LETTERS[1:5]
    df_ae_separated <-
      split(aelabels, aelabels) |>
      lapply(function(x) {
        as.integer(sites_kg_extract_e$class_kg == x)
      }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_ae_separated) <- sprintf("DUM_CLRG%s_0_00000", aelabels)

    kg_extracted <-
      cbind(
        site_id = unlist(sites_kg_extract_e[[id_col]]),
        df_ae_separated
      )
    return(kg_extracted)
  }


#' Compute land cover classes ratio in circle buffers around points
#'
#' @param path character giving nlcd data path
#' @param sites terra::SpatVector of points geometry
#' @param radius numeric (non-negative) giving the
#' radius of buffer around points
#' @param year numeric giving the year of NLCD data used
#' @importFrom utils read.csv
#' @importFrom utils data
#' @importFrom terra rast
#' @importFrom terra project
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom terra same.crs
#' @importFrom terra buffer
#' @importFrom sf st_union
#' @importFrom sf st_geometry
#' @importFrom terra intersect
#' @importFrom exactextractr exact_extract
#' @import spData
#' @export
calc_nlcd_ratio <- function(path,
                            sites,
                            radius = 1000,
                            year = 2021) {
  # check inputs
  if (!is.numeric(radius)) {
    stop("radius is not a numeric.")
  }
  if (radius <= 0) {
    stop("radius has not a likely value.")
  }
  if (!is.numeric(year)) {
    stop("year is not a numeric.")
  }
  if (class(sites)[1] != "SpatVector") {
    stop("sites is not a terra::SpatVector.")
  }
  if (!is.character(path)) {
    stop("path is not a character.")
  }
  if (!file.exists(path)) {
    stop("path does not exist.")
  }
  # open nlcd file corresponding to the year
  nlcd_file <- list.files(path,
                          pattern = paste0("nlcd_", year, "_.*.tif$"),
                          full.names = TRUE)
  if (length(nlcd_file) == 0) {
    stop("NLCD data not available for this year.")
  }
  nlcd <- terra::rast(nlcd_file)
  # select points within mainland US and reproject on nlcd crs if necessary
  # need spData library
  utils::data("us_states", package = "spData")
  us_main <- sf::st_union(get("us_states")) |>
    terra::vect() |>
    terra::project(y = terra::crs(sites))
  data_vect_b <- sites |>
    terra::intersect(x = us_main)
  if (!terra::same.crs(data_vect_b, nlcd)) {
    data_vect_b <- terra::project(data_vect_b, terra::crs(nlcd))
  }
  # create circle buffers with buf_radius
  bufs_pol <- terra::buffer(data_vect_b, width = radius) |>
    sf::st_as_sf()
  # ratio of each nlcd class per buffer
  nlcd_at_bufs <- exactextractr::exact_extract(nlcd,
                                               sf::st_geometry(bufs_pol),
                                               fun = "frac",
                                               stack_apply = TRUE,
                                               progress = FALSE)
  # select only the columns of interest
  nlcd_at_bufs <- nlcd_at_bufs[names(nlcd_at_bufs)[grepl("frac_",
                                                         names(nlcd_at_bufs))]]
  # change column names
  fpath <- system.file("extdata", "nlcd_classes.csv", package = "NRTAPmodel")
  nlcd_classes <- utils::read.csv(fpath)
  nlcd_names <- names(nlcd_at_bufs)
  nlcd_names <- sub(pattern = "frac_", replacement = "", x = nlcd_names)
  nlcd_names <- as.numeric(nlcd_names)
  nlcd_names <- nlcd_classes[nlcd_classes$value %in% nlcd_names, c("class")]
  new_names <- sapply(
    nlcd_names,
    function(x) {
      sprintf("LDU_%s_0_%05d_%04d", x, radius, year)
    }
  )
  names(nlcd_at_bufs) <- new_names
  # merge data_vect with nlcd class fractions (and reproject)
  new_data_vect <- cbind(data_vect_b, nlcd_at_bufs)
  new_data_vect <- terra::project(new_data_vect, terra::crs(sites))
  return(new_data_vect)
}


#' Calculate EPA Ecoregions level 2/3 binary variables
#' @param path character(1). Path to Ecoregion Shapefiles
#' @param sites sf/SpatVector. Unique sites. Should include
#'  a unique identifier field named \code{id_col}
#' @param id_col character(1). Name of unique identifier.
#' @returns a data.frame object with dummy variables and attributes of:
#'   - \code{attr(., "ecoregion2_code")}: Ecoregion lv.2 code and key
#'   - \code{attr(., "ecoregion3_code")}: Ecoregion lv.3 code and key
#' @author Insang Song
#' @importFrom methods is
#' @import terra
#' @export
calc_ecoregion <-
  function(
    path = "./input/data/ecoregions/raw/us_eco_l3_state_boundaries.shp",
    sites,
    id_col = "site_id"
  ) {

    if (!methods::is(sites, "SpatVector")) {
      sites <- terra::vect(sites)
    }
    ecoreg <- terra::vect(path)
    ecoreg <- ecoreg[, grepl("^(L2_KEY|L3_KEY)", names(ecoreg))]

    sites <- terra::project(sites, terra::crs(ecoreg))

    sites_in <- terra::intersect(sites, ecoreg)
    sites_out <-
      sites[!unlist(sites[[id_col]]) %in% unlist(sites_in[[id_col]]), ]

    sites_snapped <- terra::snap(sites_out, ecoreg, tolerance = 50)
    sites_fixed <- rbind(sites_in, sites_snapped)
    extracted <- terra::extract(ecoreg, sites_fixed)

    # Generate field names from extracted ecoregion keys
    # TODO: if we keep all-zero fields, the initial reference
    # should be the ecoregion polygon, not the extracted data
    key2_sorted <- unlist(extracted[, 3])
    key2_num <-
      regmatches(key2_sorted, regexpr("\\d{1,2}\\.[1-9]", key2_sorted))
    key2_num <- as.integer(10 * as.numeric(key2_num))
    key2_num <- sprintf("DUM_E2%03d_0_00000", key2_num)
    key2_num_unique <- sort(unique(key2_num))

    key3_sorted <- unlist(extracted[, 2])
    key3_num <-
      regmatches(key3_sorted, regexpr("\\d{1,3}", key3_sorted))
    key3_num <- as.integer(as.numeric(key3_num))
    key3_num <- sprintf("DUM_E3%03d_0_00000", key3_num)
    key3_num_unique <- sort(unique(key3_num))


    df_lv2 <-
      split(key2_num_unique, key2_num_unique) |>
      lapply(function(x) { as.integer(key2_num == x) }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_lv2) <- key2_num_unique
    df_lv3 <-
      split(key3_num_unique, key3_num_unique) |>
      lapply(function(x) { as.integer(key3_num == x) }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_lv3) <- key3_num_unique

    sites_ecoreg <- cbind(sites[[id_col]], df_lv2, df_lv3)
    attr(sites_ecoreg, "ecoregion2_code") <- sort(unique(ecoreg$L2_KEY))
    attr(sites_ecoreg, "ecoregion3_code") <- sort(unique(ecoreg$L3_KEY))
    return(sites_ecoreg)
  }



#' Get mosaicked or merged raster from multiple MODIS hdf files
#' @param paths character. Full list of hdf file paths.
#'  preferably a recursive search result from \code{list.files}.
#' @param product character(1). Product code of MODIS. Should be one of
#' \code{c('MOD11A1', 'MOD13A2', 'MOD09GA', 'MCD19A2')}
#' @param date_in character(1). date to query. Should be in
#' \code{"YYYY-MM-DD"} format.
#' @param regex_sds character(1). subdataset names to extract.
#' Should conform to regular expression. See \link{regex} for details.
#' Default is NULL, which means that the subdataset names are automatically
#' selected based on \code{product} value.
#' @param foo Function name or custom function to aggregate overlapping
#' cell values. See \code{fun} description in \link[terra]{tapp} for details.
#' @author Insang Song
#' @returns A SpatRaster object.
#' @export
modis_get_vrt <- function(
    paths,
    product = c("MOD11A1", "MOD13A2",
                "MOD09GA", "MCD19A2"),
    date_in = NULL,
    regex_sds = NULL,
    foo = "mean") {

  product <- match.arg(product)

  sds_selector <- function(pname, custom_sel = regex_sds) {
    modis_sds <-
      switch(pname,
        MOD11A1 = "(LST_)",
        MOD13A2 = "(NDVI_)",
        MOD09GA = "(sur_refl_b0)",
        MCD19A2 = "(Optical_Depth)"
      )
    if (!is.null(custom_sel)) {
      modis_sds <- custom_sel
    }
    return(modis_sds)
  }

  # aggregate sublayers in subdataset
  sds_aggregate <- function(path, product, fun_agg = foo) {
    # describe provides subdataset information
    # we use var to get detailed information in subdatasets
    sds_desc <- terra::describe(path, sds = TRUE)
    flag_modis <- sds_selector(pname = product)
    index_sds <- grep(flag_modis, sds_desc$var)
    sds_desc <- sds_desc[index_sds, c("name", "var", "nlyr")]

    # if there are multiple layers in a subdataset,
    # aggregate (average)
    sds_read <- terra::rast(path, subds = index_sds)
    sds_nsds <- nrow(sds_desc)
    if (all(sds_desc$nlyr == 1L)) {
      sds_agg <- sds_read
    } else {
      sds_aggindex <- rep(seq_len(sds_nsds), times = sds_desc$nlyr)
      sds_agg <-
        terra::tapp(sds_read,
                    index = sds_aggindex,
                    fun = fun_agg,
                    na.rm = TRUE)
    }
    names(sds_agg) <- sds_desc$var
    return(sds_agg)
  }

  if (!is.character(paths)) {
    stop("Argument flist should be a list of hdf files (character).\n")
  }
  if (!(is.character(foo) || is.function(foo))) {
    stop("Argument foo should be a function or name of a function
         that is accepted in terra::tapp.\n")
  }
  # this case cannot detect malform like 2024-02-30.
  if (!grepl("[0-9]{4,4}\\-([0][1-9]|[1][0-2])\\-([0-2][0-9]|[3][0-1])",
             date_in)
  ) {
    stop("date_in does not conform to the required format
         'YYYY-MM-DD'.\n")
  }

  # if (startsWith(product, "MOD09")) {
  #   ismod09 <- TRUE
  # }

  # interpret date
  today <- as.character(date_in)
  dayjul <- strftime(today, "%Y%j")
  ftarget <- grep(sprintf("A%s", dayjul), paths, value = TRUE)

  # get layer information
  layer_target <-
    lapply(ftarget,
           function(x) {
             sds_aggregate(x, product = product, fun_agg = foo)
           })
  # get relevant subdataset only
  # layer_target <- sapply(layer_descr, function(x) x[[1]])
  # layer_number <- sapply(layer_descr, function(x) as.integer(x[[2]]))

  # if there are multiple layers in a subdataset
  # if (any(layer_number > 1)) {
  #   if (ismod09) {
  #     layers <- describe(ftarget[[1]], sds = TRUE)
  #     layers <- grep("sur_refl_b0", layers$var)
  #     layer_target <- lapply(ftarget, \(x) terra::rast(x, lyrs = layers))
  #   } else {
  #     layer_target <- lapply(layer_target, \(x) terra::rast(x))
  #   }
  #   layer_target <- lapply(layer_target, foo, na.rm = TRUE)

  # Merge multiple rasters into one
  # do.call(f, l) is equivalent to f(l[[1]], ... , l[[length(l)]])
  if (length(paths) > 1) {
    result_merged <- do.call(terra::merge, layer_target)
  } else {
    result_merged <- layer_target
  }
  # } else {
  #   result_merged <- terra::vrt(layer_target)
  # }
  return(result_merged)
}


#' Assign MODIS VNP46 corner coordinates to retrieve a merged raster
#' @description This function will return a SpatRaster object with
#' georeferenced h5 files of VNP46A2 product.
#' @param paths character. Full paths of h5 files.
#' @param date_in character(1). Date to query.
#' @param subdataset integer(1). Subdataset number to process.
#' Default is 3L.
#' @param tile_df prespecified data.frame with "tile" and "exts" columns,
#' where the former stores tile number (h00v00 format) and the latter
#' stores terra::ext output.
#' @param crs_ref character(1). terra::crs compatible CRS.
#' Default is "EPSG:4326"
#' @author Insang Song
#' @importFrom terra rast
#' @importFrom terra ext
#' @importFrom terra crs
#' @importFrom terra merge
#' @export
modis_preprocess_vnp46 <- function(
  paths,
  date_in,
  subdataset = 3L,
  tile_df,
  crs_ref = "EPSG:4326"
) {
  # this case cannot detect malform like 2024-02-30.
  if (!grepl("[0-9]{4,4}\\-([0][1-9]|[1][0-2])\\-([0-2][0-9]|[3][0-1])",
             date_in)
  ) {
    stop("date_in does not conform to the required format
         'YYYY-MM-DD'.\n")
  }
  if (!all(c("tile", "xmin", "xmax", "ymin", "ymax") %in%
             colnames(tile_df))) {
    stop("tile_df is in invalid format. Please review tile_df.\n")
  }
  date_in <- as.Date(date_in)
  datejul <- strftime(date_in, format = "%Y%j")
  stdtile <- tile_df$tile

  filepaths_today <- grep(sprintf("A%s", datejul), paths, value = TRUE)
  # today's filenames
  filepaths_today <-
    grep(paste("(", 
               paste(stdtile, collapse = "|"), ")"),
         filepaths_today, value = TRUE)

  filepaths_today_tiles <-
    regmatches(filepaths_today,
               regexpr("h([0-2][0-9]|[3][0-6])v([0-1][0-9])", filepaths_today))

  vnp46_today <- unname(split(filepaths_today, filepaths_today))
  filepaths_today_tiles_list <-
    unname(split(filepaths_today_tiles, filepaths_today_tiles))

  # for filenames,
  # assign corner coordinates then merge
  # Subdataset 3 is BRDF-corrected nighttime light
  vnp_assigned <-
    mapply(function(vnp, tile_in) {
      vnp_ <- terra::rast(vnp, subds = subdataset)
      tile_ext <- tile_df[tile_df$tile == tile_in, -1]
      # print(tile_ext)
      terra::crs(vnp_) <- terra::crs(crs_ref)
      terra::ext(vnp_) <- unlist(tile_ext)
      return(vnp_)
    }, vnp46_today, filepaths_today_tiles_list, SIMPLIFY = FALSE)
  if (length(filepaths_today) > 1) {
    vnp_all <- do.call(terra::merge, vnp_assigned)
  } else {
    vnp_all <- vnp_assigned
  }
  return(vnp_all)
}


#' Mosaic MODIS MOD06_L2 product files
#' @description This function will return a SpatRaster object with
#' mosaicked 5-minute cloud coverage values.
#' @param paths character. Full paths of hdf files.
#' @param date_in character(1). Date to query.
#' @param get_var character. One of 'Cloud_Fraction_Day' or
#' 'Cloud_Fraction_Night'
#' @param resolution numeric(1). Resolution of output raster.
#' Unit is degree.
#' @returns SpatRaster object. CRS is "EPSG:4326".
#' @author Insang Song
#' @importFrom stars read_stars
#' @importFrom stars st_warp
#' @importFrom terra rast
#' @importFrom terra mosaic
#' @export
modis_mosaic_mod06 <-
  function(
    paths,
    date_in,
    get_var = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
    resolution = 0.025
  ) {
    rectify_ref_stars <- function(ras, cellsize = resolution) {
      ras <- stars::read_stars(ras)
      rtd <-
        stars::st_warp(ras, crs = 4326, cellsize = cellsize, threshold = 0.66)
      return(rtd)
    }
    header <- "HDF4_EOS:EOS_SWATH:"
    suffix <- ":mod06:"
    ras_mod06 <- vector("list", 2L)
    datejul <- strftime(date_in, format = "%Y%j")
    paths_today <- grep(sprintf("A%s", datejul), paths, value = TRUE)

    if (length(paths) > 1) {
      for (element in seq_along(get_var)) {
        target_text <-
          sprintf("%s%s%s%s", header, paths_today, suffix, get_var[element])
        # rectified stars objects to SpatRaster
        mod06_element <- split(target_text, target_text) |>
          lapply(rectify_ref_stars) |>
          lapply(terra::rast)
        mod06_element <- Reduce(f = terra::mosaic, x = mod06_element)
        ras_mod06[[element]] <- mod06_element
      }
      mod06_mosaic <- c(ras_mod06[[1]], ras_mod06[[2]])
      terra::varnames(mod06_mosaic) <- get_var
      mod06_mosaic <- terra::crop(mod06_mosaic,
                                  terra::ext(c(-130, -60, 20, 54)))
    } else {
      mod06_mosaic <- terra::rast(rectify_ref_stars(paths))
    }
    return(mod06_mosaic)
  }



#' A single-date MODIS worker for parallelization
#' @param raster SpatRaster.
#' @param date Date(1). date to query.
#' @param sites_in sf object. AQS sites.
#' @param name_extracted character. Names of calculated covariates.
#' @param product character(1). Product code of MODIS. Should be one of
#' \code{c('MOD11A1', 'MOD13A2', 'MOD06_L2', 'VNP46A2', 'MOD09GA', 'MCD19A2')}
#' @param fun_summary_raster function. Summary function for
#' multilayer rasters. Passed to \code{foo}. See also
#' \code{\link[exactextractr]{exact_extract}}
#' @param id_col character(1). Field name where unique site identifiers
#' are stored. Default is "site_id"
#' @param radius numeric. Radius to buffer.
#' @author Insang Song
#' @returns A data.frame object.
#' @importFrom terra extract
#' @importFrom terra project
#' @importFrom terra vect
#' @importFrom terra nlyr
#' @importFrom terra describe
#' @importFrom methods is
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @export
modis_worker <- function(
  raster,
  date,
  sites_in = NULL,
  name_extracted = NULL,
  product = c("MOD11A1", "MOD13A2", "MOD06_L2",
              "VNP46A2", "MOD09GA", "MCD19A2"),
  fun_summary_raster = "mean",
  id_col = "site_id",
  radius = 0L
) {
  if (!any(methods::is(sites_in, "SpatVector"),
           methods::is(sites_in, "sf"),
           methods::is(sites_in, "sftime"))) {
    stop("sites_in should be one of sf, sftime, or SpatVector.\n")
  }

  extract_with_buffer <- function(
    points,
    surf,
    radius,
    id,
    func = "mean"
  ) {
    # generate buffers
    bufs <- terra::buffer(points, width = radius, quadsegs = 180L)
    bufs <- terra::project(bufs, terra::crs(surf))
    # crop raster
    bufs_extent <- terra::ext(bufs)
    surf_cropped <- terra::crop(surf, bufs_extent, snap = "out")

    # extract raster values
    surf_at_bufs <-
      exactextractr::exact_extract(
        surf_cropped,
        sf::st_as_sf(bufs),
        fun = func,
        force_df = TRUE,
        append_cols = id,
        progress = FALSE,
        max_cells_in_memory = 5e07
      )
    surf_at_bufs_summary <-
      surf_at_bufs

    return(surf_at_bufs_summary)
  }
  # if (!is.function(foo)) {
  #   stop("Argument foo should be a function.\n")
  # }
  product <- match.arg(product)

  if (methods::is(sites_in, "SpatVector")) {
    sites_in <- sf::st_as_sf(sites_in)
  }

  # # VNP46 corner assignment
  if (product == "VNP46A2") {
    raster[raster == 65535L] <- NA
  }

  # raster used to be vrt_today
  if (any(grepl("00000", name_extracted))) {
    sites_tr <- terra::vect(sites_in)
    sites_tr <- terra::project(sites_tr, terra::crs(raster))
    extracted <- terra::extract(x = raster, y = sites_tr)
    sites_blank <- sf::st_drop_geometry(sites_in)
    extracted <- cbind(sites_blank, extracted)
  } else {
    extracted <-
      extract_with_buffer(
        points = sites_in,
        surf = raster,
        id = id_col,
        radius = radius,
        func = fun_summary_raster
      )
  }

  # cleaning names
  # assuming that extracted is a data.frame
  extracted$date <- date
  name_offset <- terra::nlyr(raster)
  # multiple columns will get proper names
  name_range <- seq(ncol(extracted) - name_offset, ncol(extracted) - 1, 1)
  colnames(extracted)[name_range] <- name_extracted
  return(extracted)
}



#' Calculate MODIS product covariates in multiple CPU threads
#' @param path character. List of HDF files.
#' @param product character(1). MODIS product. Should be one of
#' \code{c('MOD11A1', 'MOD13A2', 'MOD06_L2', 'VNP46A2', 'MOD09GA', 'MCD19A2')}
#' @param sites sf object. Unique sites where covariates
#' will be calculated.
#' @param id_col character(1). Site identifier. Default is "site_id"
#' @param name_covariates character. Name header of covariates.
#' The calculated covariate names will have a form of
#' '{name_covariates}{zero-padded buffer radius in meters}',
#' e.g., 'MOD_NDVIF_0_50000' where 50 km radius circular buffer
#' was used to calculate mean NDVI value.
#' @param radius numeric. Radii to calculate covariates.
#' Default is c(0, 1000, 10000, 50000).
#' @param subdataset Index or search pattern of subdataset.
#'  Pattern is not accepted when product is \code{"VNP46A2"} or
#'  \code{"MOD06_L2"}
#' @param fun_summary character or function. Function to summarize
#'  extracted raster values.
#' @param nthreads integer(1). Number of threads to be used
#'  to calculate covariates.
#' @param tilelist character(1). data.frame with
#'  VNP46A2 tile corner coordinate list.
#' @param package_list_add character. A vector with package names to load
#'  these in each thread. Note that \code{sf}, \code{terra},
#'  \code{exactextractr}, \code{scomps}, and \code{dplyr}
#'  are the default packages to be loaded.
#' @param export_list_add character. A vector with object names to export
#'  to each thread. It should be minimized to spare memory.
#' @note See details for setting parallelization
#' \code{\link[foreach]{foreach}},
#' \code{\link[parallelly]{makeClusterPSOCK}},
#' \code{\link[parallelly]{availableCores}},
#' \code{\link[doParallel]{registerDoParallel}}
#' @import foreach
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom doRNG `%dorng%`
#' @importFrom parallelly makeClusterPSOCK
#' @importFrom parallelly availableCores
#' @importFrom doParallel registerDoParallel
#' @importFrom parallelly killNode
#' @export
calc_modis <-
  function(
    path,
    product = c("MOD11A1", "MOD13A2", "MOD06_L2",
                "VNP46A2", "MOD09GA", "MCD19A2"),
    sites,
    id_col = "site_id",
    name_covariates,
    radius = c(0L, 1e3L, 1e4L, 5e4L),
    subdataset = NULL,
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
        "id_col", "fun_summary", "tilelist", "radius", "subdataset")
    package_list <-
      c("sf", "terra", "exactextractr", "foreach", "data.table", "stars",
        "NRTAPmodel", "dplyr", "doRNG", "parallelly", "doParallel")
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
              subdataset = subdataset,
              tile_df = tilelist,
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
                          regex_sds = subdataset,
                          product = product,
                          date_in = day_to_pick)
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
                         id_col = id_col,
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


#' Calculate temporal dummy variables
#' @param sites data.frame with a temporal field named "date"
#'  see \link{\code{convert_stobj_to_stdt}}
#' @param id_col character(1). Unique site identifier column name.
#'  Default is "site_id".
#' @param domain_year integer. Year domain to dummify.
#'  Default is \code{seq(2018L, 2022L)}
#' @returns data.frame with year, month, and weekday indicators.
#' @author Insang Song
#' @importFrom methods is
#' @importFrom data.table year
#' @importFrom data.table month
#' @importFrom data.table as.data.table
#' @export
calc_temporal_dummies <-
  function(
    sites,
    id_col = "site_id",
    domain_year = seq(2018L, 2022L)
  ) {
    if (!methods::is(sites, "data.frame")) {
      stop("Argument sites is not a data.frame.\n")
    }
    if (!"date" %in% names(sites)) {
      stop("A mandatory field 'date' does not exist in sites.\n")
    }
    id_col <- id_col
    dummify <- function(vec, domain) {
      vec_unique <- domain#sort(unique(vec))
      vec_split <- split(vec_unique, vec_unique)
      vec_assigned <-
        lapply(vec_split,
               function(x) {
                 as.integer(vec == x)
               })
      dt_dum <- Reduce(cbind, vec_assigned)
      dt_dum <- data.table::as.data.table(dt_dum)
      return(dt_dum)
    }

    # year
    vec_year <- data.table::year(sites$date)
    dt_year_dum <- dummify(vec_year, domain_year)
    # should the last year be the present year or 2022?
    colnames(dt_year_dum) <-
      sprintf("DUM_Y%d_0_00000", domain_year)


    # month
    vec_month <- data.table::month(sites$date)
    dt_month_dum <- dummify(vec_month, seq(1L, 12L))
    shortmn <-
      c("JANUA", "FEBRU", "MARCH", "APRIL",
        "MAYMA", "JUNEJ", "JULYJ", "AUGUS",
        "SEPTE", "OCTOB", "NOVEM", "DECEM")
    colnames(dt_month_dum) <-
      sprintf("DUM_%s_0_00000", shortmn)

    # weekday (starts from 1-Monday)
    vec_wday <- as.POSIXlt(sites$date)$wday
    dt_wday_dum <- dummify(vec_wday, seq(1L, 7L))
    colnames(dt_wday_dum) <-
      sprintf("DUM_WKDY%d_0_00000", seq(1L, 7L))


    # column binding
    sites_dums <-
      cbind(
        sites,
        dt_year_dum,
        dt_month_dum,
        dt_wday_dum
      )

    return(sites_dums)
  }

#' Calculate TRI covariates
#' @param path character(1). Path to the directory with TRI CSV files
#' @param sites stdt/sf/SpatVector/data.frame. Unique sites
#' see \link{\code{convert_stobj_to_stdt}}
#' @param id_col character(1). Unique site identifier column name.
#'  Default is "site_id".
#' @param domain_year integer. Year domain to dummify.
#'  Default is \code{seq(2018L, 2022L)}
#' @author Insang Song
#' @returns A data.frame object.
#' @importFrom terra vect
#' @importFrom terra crs
#' @importFrom methods is
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @export
calculate_tri <- function(
  path = "./input/tri/",
  sites,
  id_col = "site_id",
  domain_year = seq(2018L, 2022L),
  radius = c(1e3L, 1e4L, 5e4L)
) {
  if (is_stdt(sites)) {
    sites <- sites$stdt
    sites_epsg <- sites$crs_dt
  } else {
    if (!all(c("lon", "lat", "date") %in% colnames(sites))) {
      stop("sites should be stdt or have 'lon', 'lat', and 'date' fields.\n")
    }
    sites_epsg <- terra::crs(sites)
    if (!methods::is(sites, "SpatVector")) {
      if (methods::is(sites, "sf")) {
        sites <- terra::vect(sites)
      }
      if (is.data.frame(sites)) {
        sites <-
          terra::vect(sites,
                      geom = c("lon", "lat"),
                      epsg = sites_epsg)
      }
    }
  }

  radius <- match.arg(radius)

  csvs_tri <- list.files(path = path, pattern = "*.csv$", full.names = TRUE)
  col_sel <- c(1, 13, 12, 34, 41, 42, 43, 45, 46, 48, 47, 104)
  csvs_tri <- lapply(csvs_tri, read.csv)
  csvs_tri <- lapply(csvs_tri, function(x) x[, col_sel])
  csvs_tri <- data.table::rbindlist(csvs_tri)
  # column name readjustment

  # depending on the way the chemicals are summarized
  # ... csvs are aggregated...
  csvs_tri_x <-
    data.table::dcast(csvs_tri, YEAR + LONGITUDE + LATITUDE ~ .)
  spvect_tri <-
    terra::vect(csvs_tri_x,
                geom = c("LONGITUDE", "LATITUDE"),
                crs = "EPSG:4326",
                keepgeom = TRUE)
  sites_re <- terra::project(sites, terra::crs(spvect_tri))

  list_buffer <- split(radius, radius)
  list_buffer <-
    lapply(list_buffer,
           function(x) {
             xx <- terra::nearby(sites_re, spvect_tri, distance = x)
             xx$buffer <- x
             xx[, lapply(.SD, sum, na.rm = TRUE),
                by = c("YEAR", "LONGITUDE", "LATITUDE", "buffer")]
           })
  df_tri <- data.table::rbindlist(list_buffer)
  return(df_tri)

}


