################################################################################
##### Calculate covariates at 1km x 1km grid and daily temporal resolution.
################################################################################
##### TODO: prospective or retrospective, or both?
##### Strategy: no all-merged data.frame (data.table) in-branch processing
##### (using the branched target then pass it to targets_predict)
##### Each branched target should have the same degree of
##### interaction (i.e., cross())
################################################################################
target_calculate_predict <-
  list(
    targets::tar_target(
      sf_us_contig,
      command = sf::st_read("inst/extdata/us_contiguous.gpkg"),
      description = "US | outline | prediction"
    ),
    targets::tar_target(
      chr_hex_res8_index,
      command = polyfill(sf_us_contig, res = 8),
      description = "H3 | grid | prediction"
    ),
    targets::tar_target(
      chr_hex_res3_index,
      command = map_chr(chr_hex_res8_index, h3_to_parent, res = 3),
      description = "H3 | grid | prediction"
    ),
    targets::tar_target(
      list_h3_res8_index,
      command = split(chr_hex_res8_index, chr_hex_res3_index),
      description = "H3 | split | grid | prediction"
    ),
    targets::tar_target(
      chr_hex_res2_index,
      command = map_chr(chr_hex_res8_index, h3_to_parent, res = 2),
      description = "H3 | grid | prediction"
    ),
    targets::tar_target(
      list_h3_res8_index2,
      command = split(chr_hex_res8_index, chr_hex_res2_index),
      description = "H3 | split | grid | prediction"
    ),

    ############################################################################
    ############################################################################
    ############################################################################

    ###########################         HMS          ###########################
    targets::tar_target(
      list_pred_calc_hms,
      command = {
        h3_locs <- h3_to_geo_sf(list_h3_res8_index[[1]])
        h3_locs$site_id <- h3_locs$h3_index
        beethoven::inject_calculate(
          covariate = "hms",
          locs = h3_locs,
          injection = list(
            path = file.path(chr_input_dir, "hms", "data_files"),
            date = beethoven::fl_dates(unlist(list_dates)),
            covariate = "hms"
          )
        )[[1]] |>
          dplyr::select(
            -dplyr::any_of(c("h3_index", "geometry", "hms_year"))
          )
      },
      pattern = cross(list_h3_res8_index, list_dates),
      iteration = "list",
      description = "Calculate HMS features | prediction",
      format = "parquet",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_40"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      )
    ),
    ##############################   NLCD    #############################
    targets::tar_target(
      name = list_pred_calc_nlcd,
      command = {
        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        unique_radii <- chr_iter_radii
        nlcd_years <- chr_iter_calc_nlcd
        # threefold lists
        # First level: years (rowbind)
        lapply(
          nlcd_years,
          function(yearj) {
            # Second level: radii (full join)
            lapply(
              unique_radii,
              function(radi) {
                inject_nlcd(
                  year = yearj,
                  radius = radi,
                  from = amadeus::process_nlcd(
                    path = file.path(chr_input_dir, "nlcd", "data_files"),
                    year = yearj,
                  ),
                  locs = h3_locs, # full dataset (no chunking by rows)
                  locs_id = arglist_common$char_siteid,
                  mode = "exact",
                  max_cells = 3e7
                )
              }
            ) %>%
              Reduce(
                f = function(d, e) {
                  dplyr::full_join(d, e, by = c("site_id", "time"))
                }
              )
          }
        ) %>%
          collapse::rowbind(., fill = TRUE) %>%
          as.data.frame()
      },
      pattern = map(list_h3_res8_index2),
      iteration = "list",
      format = "parquet",
      description = "Calculate NLCD features | prediction grid | H3",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_40"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      )
    ),
    ##############################   GMTED    #############################
    targets::tar_target(
      name = list_pred_calc_gmted,
      command = {
        h3_locs <- h3_to_geo_sf(list_h3_res8_index[[1]])
        h3_locs$site_id <- h3_locs$h3_index
        beethoven::calc_gmted_direct(
          variable = c(chr_iter_calc_gmted_vars, "7.5 arc-seconds"),
          path = file.path(chr_input_dir, "gmted", "data_files"),
          locs = h3_locs,
          locs_id = "site_id",
          radius = chr_iter_calc_gmted_radii
        )
      },
      iteration = "list",
      pattern = cross(
        list_h3_res8_index,
        chr_iter_calc_gmted_vars,
        chr_iter_calc_gmted_radii
      ),
      format = "parquet",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      description = "Calculate GMTED features | prediction | H3"
    ),
    ############################################################################
    ############################################################################
    ############################################################################

    ###########################         GEOS         ###########################
    targets::tar_target(
      list_pred_calc_geos_aqc,
      command = {
        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index
        download_geos_buffer <- TRUE
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[1]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = h3_locs,
          locs_id = "site_id"
        )
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      format = "parquet",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | aqc | H3 | prediction"
    ),
    targets::tar_target(
      list_pred_calc_geos_chm,
      command = {
        download_geos_buffer
        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[2]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = h3_locs,
          locs_id = "site_id"
        )
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      format = "parquet",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | chm | H3 | prediction"
    ),
    ###########################         NARR         ###########################
    targets::tar_target(
      list_pred_calc_narr,
      command = {
        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index
        # download_narr_buffer <- TRUE
        lapply(
          chr_iter_calc_narr,
          function(name) {
            dt_iter_calc_narr <- amadeus::calculate_narr(
              from = amadeus::process_narr(
                path = file.path(chr_input_dir, "narr", name),
                variable = name,
                date = beethoven::fl_dates(unlist(list_dates))
              ),
              locs = h3_locs,
              locs_id = "site_id",
              radius = 0,
              fun = "mean",
              geom = FALSE
            )
            if (length(grep("level", names(dt_iter_calc_narr))) == 1) {
              dt_iter_calc_narr <-
                dt_iter_calc_narr[dt_iter_calc_narr$level == 1000, ]
              dt_iter_calc_narr <-
                dt_iter_calc_narr[, -grep("level", names(dt_iter_calc_narr))]
            }
            dt_iter_calc_narr
          }
        ) %>%
          Reduce(
            f = function(d, e) {
              dplyr::full_join(d, e, by = c("site_id", "time"))
            },
            .
          ) %>%
          as.data.frame()
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      iteration = "list",
      format = "parquet",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_big_grid"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      description = "Calculate NARR features | prediction | H3"
    ),
    ###########################       MODIS - MOD11       ######################
    targets::tar_target(
      list_pred_calc_mod11,
      command = {
        search_dir <- file.path(chr_input_dir, "modis_preprocessed", "MOD11A1")
        date_find <- list_dates

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        lapply(chr_iter_radii, function(r) {
          Map(
            f = function(date_i) {
              date_in <- paste("(", date_i, ")", sep = "")
              target_file <- list.files(
                path = search_dir,
                pattern = paste0(date_in, "\\.tif$"),
                full.names = TRUE
              )
              res <- tryCatch(
                {
                  beethoven::calculate_modis_direct(
                    file = target_file, #chr_list_calc_mod11_files,
                    site = h3_locs,
                    site_id = arglist_common[["char_siteid"]],
                    radius = r,
                    colheader = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
                    mark = TRUE
                  )
                },
                error = function(e) {
                  res <- expand.grid(
                    site_id = h3_locs[["site_id"]],
                    time = date_i,
                    MOD_SFCTD_0_ = NA_real_,
                    MOD_SFCTN_0_ = NA_real_
                  )
                  names(res)[3:4] <- paste0(names(res)[3:4], sprintf("%05d", r))
                  return(res)
                }
              )
              res
            },
            date_find
          ) %>%
            collapse::rowbind(., fill = TRUE)
        }) %>%
          Reduce(
            f = function(d, e) {
              dplyr::full_join(d, e, by = c("site_id", "time"))
            },
            .
          ) %>%
          as.data.frame()
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      iteration = "list",
      format = "parquet",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      description = "Calculate MODIS - MOD11 features | prediction"
    ),
    ##########################       MODIS - MOD06       ######################
    targets::tar_target(
      list_pred_calc_mod06,
      command = {
        search_dir <- file.path(chr_input_dir, "modis_preprocessed", "MOD06_L2")
        date_find <- list_dates

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        lapply(chr_iter_radii, function(r) {
          Map(
            f = function(date_i) {
              date_in <- paste("(", date_i, ")", sep = "")
              target_file <- list.files(
                path = search_dir,
                pattern = paste0(date_in, "\\.tif$"),
                full.names = TRUE
              )
              res <- tryCatch(
                {
                  beethoven::calculate_modis_direct(
                    file = target_file,
                    site = h3_locs,
                    site_id = arglist_common[["char_siteid"]],
                    radius = r,
                    colheader = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
                    mark = TRUE
                  )
                },
                error = function(e) {
                  res <- expand.grid(
                    site_id = h3_locs[["site_id"]],
                    time = date_i,
                    MOD_CLCVD_0_ = NA_real_,
                    MOD_CLCVN_0_ = NA_real_
                  )
                  names(res)[3:4] <- paste0(names(res)[3:4], sprintf("%05d", r))
                  return(res)
                }
              )
              res
            },
            date_find
          ) %>%
            collapse::rowbind(., fill = TRUE)
        }) %>%
          Reduce(
            f = function(d, e) {
              dplyr::full_join(d, e, by = c("site_id", "time"))
            },
            .
          ) %>%
          as.data.frame()
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      iteration = "list",
      format = "parquet",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      description = "Calculate MODIS - MOD06 features | prediction | H3"
    ),
    ###########################       MODIS - MOD13       ######################
    targets::tar_target(
      list_pred_calc_mod13,
      command = {
        search_dir <- file.path(chr_input_dir, "modis_preprocessed", "MOD13A1")
        date_find <- list_dates

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        lapply(chr_iter_radii, function(r) {
          Map(
            f = function(date_i) {
              date_in <- paste("(", date_i, ")", sep = "")
              target_file <- list.files(
                path = search_dir,
                pattern = paste0(date_in, "\\.tif$"),
                full.names = TRUE
              )
              res <- tryCatch(
                {
                  beethoven::calculate_modis_direct(
                    file = target_file,
                    site = h3_locs,
                    site_id = arglist_common[["char_siteid"]],
                    radius = r,
                    colheader = c("MOD_NDVIV_0_"),
                    mark = TRUE
                  )
                },
                error = function(e) {
                  res <- expand.grid(
                    site_id = h3_locs[["site_id"]],
                    time = date_i,
                    MOD_NDVIV_0_ = NA_real_
                  )
                  names(res)[3] <- paste0(names(res)[3], sprintf("%05d", r))
                  return(res)
                }
              )
              res
            },
            date_find
          ) %>%
            collapse::rowbind(., fill = TRUE)
        }) %>%
          Reduce(
            f = function(d, e) {
              dplyr::full_join(d, e, by = c("site_id", "time"))
            },
            .
          ) %>%
          as.data.frame()
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "Calculate MODIS - MOD13 features | prediction grid | H3"
    ),
    ###########################     MODIS - MCD19_1km     ######################
    targets::tar_target(
      list_pred_calc_mcd19_1km,
      command = {
        search_dir <- file.path(
          chr_input_dir,
          "modis_preprocessed",
          "MCD19A2_1km"
        )
        date_find <- list_dates

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        lapply(chr_iter_radii, function(r) {
          Map(
            f = function(date_i) {
              date_in <- paste("(", date_i, ")", sep = "")
              target_file <- list.files(
                path = search_dir,
                pattern = paste0(date_in, "\\.tif$"),
                full.names = TRUE
              )
              res <- tryCatch(
                {
                  beethoven::calculate_modis_direct(
                    file = target_file,
                    site = h3_locs,
                    site_id = arglist_common[["char_siteid"]],
                    radius = r,
                    colheader = c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
                    mark = TRUE
                  )
                },
                error = function(e) {
                  res <- expand.grid(
                    site_id = h3_locs[["site_id"]],
                    time = date_i,
                    MOD_AD4TA_0_ = NA_real_,
                    MOD_AD5TA_0_ = NA_real_
                  )
                  names(res)[3:4] <- paste0(names(res)[3:4], sprintf("%05d", r))
                  return(res)
                }
              )
              res
            },
            date_find
          ) %>%
            collapse::rowbind(., fill = TRUE)
        }) %>%
          Reduce(
            f = function(d, e) {
              dplyr::full_join(d, e, by = c("site_id", "time"))
            },
            .
          ) %>%
          as.data.frame()
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "Calculate MODIS - MCD19_1km features | prediction grid | H3"
    ),
    ###########################     MODIS - MCD19_5km     ######################
    targets::tar_target(
      list_pred_calc_mcd19_5km,
      command = {
        search_dir <- file.path(
          chr_input_dir,
          "modis_preprocessed",
          "MCD19A2_5km"
        )
        date_find <- list_dates

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        lapply(chr_iter_radii, function(r) {
          Map(
            f = function(date_i) {
              date_in <- paste("(", date_i, ")", sep = "")
              target_file <- list.files(
                path = search_dir,
                pattern = paste0(date_in, "\\.tif$"),
                full.names = TRUE
              )
              res <- tryCatch(
                {
                  beethoven::calculate_modis_direct(
                    file = target_file,
                    site = h3_locs,
                    site_id = arglist_common[["char_siteid"]],
                    radius = r,
                    colheader = c(
                      "MOD_CSZAN_0_",
                      "MOD_CVZAN_0_",
                      "MOD_RAZAN_0_",
                      "MOD_SCTAN_0_",
                      "MOD_GLNAN_0_"
                    ),
                    mark = TRUE
                  )
                },
                error = function(e) {
                  res <- expand.grid(
                    site_id = h3_locs[["site_id"]],
                    time = date_i,
                    MOD_CSZAN_0_ = NA_real_,
                    MOD_CVZAN_0_ = NA_real_,
                    MOD_RAZAN_0_ = NA_real_,
                    MOD_SCTAN_0_ = NA_real_,
                    MOD_GLNAN_0_ = NA_real_
                  )
                  names(res)[3:7] <- paste0(names(res)[3:7], sprintf("%05d", r))
                  return(res)
                }
              )
              res
            },
            date_find
          ) %>%
            collapse::rowbind(., fill = TRUE)
        }) %>%
          Reduce(
            f = function(d, e) {
              dplyr::full_join(d, e, by = c("site_id", "time"))
            },
            .
          ) %>%
          as.data.frame()
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "Calculate MODIS - MCD19_5km features | prediction grid | H3"
    ),
    ###########################       MODIS - MOD09       ######################
    targets::tar_target(
      list_pred_calc_mod09,
      command = {
        search_dir <- file.path(chr_input_dir, "modis_preprocessed", "MOD09GA")
        date_find <- list_dates

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        lapply(chr_iter_radii, function(r) {
          Map(
            f = function(date_i) {
              date_in <- paste("(", date_i, ")", sep = "")
              target_file <- list.files(
                path = search_dir,
                pattern = paste0(date_in, "\\.tif$"),
                full.names = TRUE
              )
              res <- tryCatch(
                {
                  beethoven::calculate_modis_direct(
                    file = target_file,
                    site = h3_locs,
                    site_id = arglist_common[["char_siteid"]],
                    radius = r,
                    colheader = c(
                      "MOD_SFCRF_1_",
                      "MOD_SFCRF_2_",
                      "MOD_SFCRF_3_",
                      "MOD_SFCRF_4_",
                      "MOD_SFCRF_5_",
                      "MOD_SFCRF_6_",
                      "MOD_SFCRF_7_"
                    ),
                    mark = TRUE
                  )
                },
                error = function(e) {
                  res <- expand.grid(
                    site_id = h3_locs[["site_id"]],
                    time = date_i,
                    MOD_SFCRF_1_ = NA_real_,
                    MOD_SFCRF_2_ = NA_real_,
                    MOD_SFCRF_3_ = NA_real_,
                    MOD_SFCRF_4_ = NA_real_,
                    MOD_SFCRF_5_ = NA_real_,
                    MOD_SFCRF_6_ = NA_real_,
                    MOD_SFCRF_7_ = NA_real_
                  )
                  names(res)[3:9] <- paste0(names(res)[3:9], sprintf("%05d", r))
                  return(res)
                }
              )
              res
            },
            date_find
          ) %>%
            collapse::rowbind(., fill = TRUE)
        }) %>%
          Reduce(
            f = function(d, e) {
              dplyr::full_join(d, e, by = c("site_id", "time"))
            },
            .
          ) %>%
          as.data.frame()
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "Calculate MODIS - MOD09GA features | prediction grid | H3"
    ),
    ###########################       MODIS - VIIRS       ######################
    targets::tar_target(
      list_pred_calc_viirs,
      command = {
        search_dir <- file.path(chr_input_dir, "modis_preprocessed", "VIIRS")
        date_find <- list_dates

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        lapply(chr_iter_radii, function(r) {
          Map(
            f = function(date_i) {
              date_in <- paste("(", date_i, ")", sep = "")
              target_file <- list.files(
                path = search_dir,
                pattern = paste0(date_in, "\\.tif$"),
                full.names = TRUE
              )
              res <- tryCatch(
                {
                  beethoven::calculate_modis_direct(
                    file = target_file,
                    site = h3_locs,
                    site_id = arglist_common[["char_siteid"]],
                    radius = r,
                    colheader = c(
                      "MOD_LGHTN_0_"
                    ),
                    mark = TRUE
                  )
                },
                error = function(e) {
                  res <- expand.grid(
                    site_id = h3_locs[["site_id"]],
                    time = date_i,
                    MOD_LGHTN_0_ = NA_real_
                  )
                  names(res)[3] <- paste0(names(res)[3], sprintf("%05d", r))
                  return(res)
                }
              )
              res
            },
            date_find
          ) %>%
            collapse::rowbind(., fill = TRUE)
        }) %>%
          Reduce(
            f = function(d, e) {
              dplyr::full_join(d, e, by = c("site_id", "time"))
            },
            .
          ) %>%
          as.data.frame()
      },
      pattern = cross(list_h3_res8_index2, list_dates),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "Calculate MODIS - VIIRS features | prediction grid | H3"
    ),
    ###########################        KOPPEN        ###########################
    # should be revised
    targets::tar_target(
      list_pred_calc_koppen,
      command = {
        # download_koppen
        h3_locs <- h3_to_geo_sf(list_h3_res8_index[[1]])
        h3_locs$site_id <- h3_locs$h3_index
        amadeus::calculate_koppen_geiger(
          from = amadeus::process_koppen_geiger(
            path = file.path(
              chr_input_dir,
              "koppen_geiger",
              "data_files",
              "Beck_KG_V1_present_0p0083.tif"
            )
          ),
          locs = h3_locs,
          locs_id = "site_id",
          geom = FALSE
        ) %>%
          as.data.frame()
      },
      iteration = "list",
      pattern = map(list_h3_res8_index),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "data.table of Koppen Geiger features | prediction grid | H3"
    ),
    ###########################      POPULATION      ###########################
    targets::tar_target(
      list_pred_calc_pop,
      command = {
        download_population
        h3_locs <- h3_to_geo_sf(list_h3_res8_index[[1]])
        h3_locs$site_id <- h3_locs$h3_index
        lapply(
          chr_iter_radii,
          function(r) {
            amadeus::calculate_population(
              from = amadeus::process_population(
                path = file.path(
                  chr_input_dir,
                  "population",
                  "data_files",
                  paste0(
                    "gpw_v4_population_density_adjusted_to_",
                    "2015_unwpp_country_totals_rev11_2020_30_sec.tif"
                  )
                )
              ),
              locs = h3_locs,
              locs_id = "site_id",
              geom = FALSE,
              radius = r
            )
          }
        ) %>%
          Reduce(
            f = function(d, e) dplyr::full_join(d, e, by = c("site_id")),
            .
          ) %>%
          as.data.frame()
      },
      pattern = map(list_h3_res8_index),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "Calculate population features | prediction grid | H3"
    ),
    ###########################         TRI          ###########################
    # df_feat_calc_tri_params
    targets::tar_target(
      list_pred_calc_tri,
      command = {
        unique_radii <- chr_iter_radii

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        grid_rowids <- seq_len(nrow(h3_locs))
        grid_rowidx <- split(
          grid_rowids,
          ceiling(grid_rowids / 10000)
        )

        # Threefold list comprehension
        # First is for years (should be rowbinded)
        lapply(
          chr_years,
          function(year) {
            # The second one is for radii: full joined
            lapply(
              unique_radii,
              function(radi) {
                # The last one is for grid rows: rowbinded
                lapply(
                  grid_rowidx,
                  function(rows) {
                    grid_sub <- h3_locs[rows, ]
                    res_tri <- tryCatch(
                      {
                        base_tri <-
                          amadeus::process_tri(
                            year = year,
                            path = file.path(chr_input_dir, "tri"),
                            # variables parameter should be changed properly depending on the vintage
                            # as of Dec 2024, newly downloaded TRI data has different layout
                            # compared to the one used in 2023
                            # add 3 after 20
                            variables = c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49)
                          )
                        res_tri <-
                          beethoven::calc_tri_mod(
                            from = base_tri,
                            locs = h3_locs[rows, ],
                            radius = radi
                          )
                        as.data.frame(res_tri)
                      },
                      error = function(e) {
                        res_tri <- data.frame(
                          site_id = grid_sub[["site_id"]],
                          time = year
                        )
                        return(res_tri)
                      }
                    )
                    res_tri
                  }
                ) %>%
                  collapse::rowbind(., fill = TRUE)
              }
            ) %>%
              Reduce(
                f = function(d, e) {
                  dplyr::full_join(d, e, by = c("site_id", "time"))
                },
                .
              )
          }
        ) %>%
          collapse::rowbind(., fill = TRUE) %>%
          as.data.frame()
      },
      pattern = map(list_h3_res8_index2),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "Calculate TRI features | prediction grid | H3"
    ),
    ###########################         NEI          ###########################
    targets::tar_target(
      list_pred_calc_nei,
      command = {
        download_nei <- TRUE

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        grid_rowids <- seq_len(nrow(h3_locs))
        grid_rowidx <- split(
          grid_rowids,
          ceiling(grid_rowids / 100000)
        )

        # Twofold list comprehension
        # First is for years (should be rowbinded)
        lapply(
          chr_iter_calc_nei,
          function(year) {
            # The last one is for grid rows: rowbinded
            lapply(
              grid_rowidx,
              function(rows) {
                beethoven::inject_calculate(
                  covariate = "nei",
                  locs = h3_locs[rows, ],
                  injection = list(
                    domain = year,
                    domain_name = "year",
                    path = file.path(chr_input_dir, "nei", "data_files"),
                    covariate = "nei"
                  )
                ) %>%
                  collapse::rowbind(fill = TRUE)
              }
            ) %>%
              collapse::rowbind(fill = TRUE)
          }
        ) %>%
          collapse::rowbind(fill = TRUE) %>%
          as.data.frame()
      },
      iteration = "list",
      pattern = map(list_h3_res8_index2),
      format = "parquet",
      description = "Calculate NEI features | prediction grid | H3",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      )
    ),
    ###########################      ECOREGIONS      ###########################
    targets::tar_target(
      list_pred_calc_ecoregions,
      command = {
        # download_ecoregions
        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        amadeus::calculate_ecoregion(
          from = amadeus::process_ecoregion(
            path = file.path(
              chr_input_dir,
              "ecoregions",
              "data_files",
              "us_eco_l3_state_boundaries.shp"
            )
          ),
          locs = h3_locs,
          # NOTE: locs are all AQS sites for computational efficiency
          locs_id = "site_id"
        )
      },
      iteration = "list",
      pattern = map(list_h3_res8_index2),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "List of Ecoregions features | prediction grid | H3"
    ),
    ###########################        GROADS        ###########################
    targets::tar_target(
      list_pred_calc_groads,
      command = {
        download_groads <- TRUE

        h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
        h3_locs$site_id <- h3_locs$h3_index

        grid_rowids <- seq_len(nrow(h3_locs))
        grid_rowidx <- split(
          grid_rowids,
          ceiling(grid_rowids / 10000)
        )

        # Twofold list comprehension
        # First is for radii (should be full-joined)
        lapply(
          chr_iter_radii,
          function(radi) {
            # The last one is for grid rows: rowbinded
            lapply(
              grid_rowidx,
              function(rows) {
                tryCatch(
                  {
                    amadeus::calculate_groads(
                      from = amadeus::process_groads(
                        path = file.path(
                          chr_input_dir,
                          "groads",
                          "data_files",
                          "gROADS-v1-americas.gdb"
                        )
                      ),
                      locs = h3_locs[rows, ],
                      locs_id = "site_id",
                      radius = radi
                    )
                  },
                  error = function(e) {
                    res <- expand.grid(
                      site_id = h3_locs[["site_id"]],
                      description = "1980 - 2010",
                      GRD_TOTAL_0_ = NA_real_,
                      GRD_DENKM_0_ = NA_real_
                    )
                    names(res)[3:4] <- paste0(
                      names(res)[3:4],
                      sprintf("%05d", radi)
                    )
                    return(res)
                  }
                )
              }
            ) %>%
              collapse::rowbind(fill = TRUE)
          }
        ) %>%
          Reduce(
            function(x, y) dplyr::full_join(x, y, by = c("site_id")),
            .
          ) %>%
          as.data.frame()
      },
      iteration = "list",
      pattern = map(list_h3_res8_index2),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_30"),
        parquet = targets::tar_resources_parquet(compression = "lz4")
      ),
      format = "parquet",
      description = "Calculate gRoads features | prediction grid | H3"
    )
    ###########################        SPATIAL BRANCHES FOR PREDICTION       ######################
    # targets::tar_target(
    #   list_pred_calc_spatial,
    #   command = {
    #     Reduce(
    #       f = function(d, e) dplyr::full_join(d, e, by = c("site_id")),
    #       list(
    #         list_pred_calc_nlcd,
    #         list_pred_calc_gmted,
    #         list_pred_calc_pop,
    #         list_pred_calc_koppen,
    #         list_pred_calc_ecoregions,
    #         list_pred_calc_groads
    #       )
    #     ) %>%
    #       as.data.frame()
    #   },
    #   pattern = map(
    #     list_pred_calc_nlcd,
    #     list_pred_calc_gmted,
    #     list_pred_calc_pop,
    #     list_pred_calc_koppen,
    #     list_pred_calc_ecoregions,
    #     list_pred_calc_groads
    #   ),
    #   iteration = "list",
    #   format = "parquet",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_10"),
    #     parquet = targets::tar_resources_parquet(compression = "lz4")
    #   ),
    #   description = "data.frame of spatial features | prediction"
    # ),
    ###########################        SPATIOTEMPORAL BRANCHES FOR PREDICTION       ###########################
    # targets::tar_target(
    #   list_pred_calc_spt,
    #   command = {
    #     Reduce(
    #       f = function(d, e) dplyr::full_join(d, e, by = c("site_id", "time")),
    #       list_pred_calc_hms,
    #       list_pred_calc_mod11,
    #       list_pred_calc_mod06,
    #       list_pred_calc_mod13,
    #       list_pred_calc_mcd19_1km,
    #       list_pred_calc_mcd19_5km,
    #       list_pred_calc_mod09,
    #       list_pred_calc_viirs,
    #       list_pred_calc_narr,
    #       list_pred_calc_geos_aqc,
    #       list_pred_calc_geos_chm
    #     ) %>%
    #       as.data.frame()
    #   },
    #   pattern = map(
    #     list_pred_calc_hms,
    #     list_pred_calc_mod11,
    #     list_pred_calc_mod06,
    #     list_pred_calc_mod13,
    #     list_pred_calc_mcd19_1km,
    #     list_pred_calc_mcd19_5km,
    #     list_pred_calc_mod09,
    #     list_pred_calc_viirs,
    #     list_pred_calc_narr,
    #     list_pred_calc_geos_aqc,
    #     list_pred_calc_geos_chm
    #   ),
    #   iteration = "list",
    #   format = "parquet",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_10"),
    #     parquet = targets::tar_resources_parquet(compression = "lz4")
    #   ),
    #   description = "data.frame of spatiotemporal features | prediction"
    # ),
    # #### Reference table for branches to match
    # targets::tar_target(
    #   int_pred_calc_lookup_branches,
    #   command = {
    #     df_branches <-
    #       targets::tar_branches(
    #         list_pred_calc_geos_aqc
    #       )
    #     branch_sp <- df_branches[["list_pred_calc_grid"]]

    #     branch_spindx <- unique(branch_sp)
    #     branch_spindx2 <- match(branch_sp, branch_spindx)
    #     branch_spindx2
    #   },
    #   iteration = "list"
    # ),
    ####################### IRREGULAR INTERVAL BRANCHES FOR PREDICTION ###########################
    # targets::tar_target(
    #   list_pred_feat_final,
    #   command = {
    #     join_spec <- dplyr::join_by("site_id", time_temp >= time)
    #     list_pred_calc_spt %>%
    #       mutate(time_temp = lubridate::year(time)) %>%
    #       left_join(
    #         list_pred_calc_tri[[int_pred_calc_lookup_branches]],
    #         by = join_spec
    #       ) %>%
    #       left_join(
    #         list_pred_calc_nei[[int_pred_calc_lookup_branches]],
    #         by = join_spec
    #       ) %>%
    #       dplyr::select(-time_temp) %>%
    #       left_join(
    #         list_pred_calc_spatial,
    #         by = "site_id"
    #       )
    #   },
    #   pattern = map(list_pred_calc_spt, int_pred_calc_lookup_branches),
    #   iteration = "list",
    #   # this target is managed as qs to reduce type conversion time
    #   format = "qs",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_10")
    #   ),
    #   description = "data.frame of irregular interval features | prediction"
    # )
  )
