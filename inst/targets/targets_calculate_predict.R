################################################################################
##### Calculate covariates at prediction grid
target_calculate_predict <-
  list(
    ############################################################################
    targets::tar_target(
      list_feat_pred_sites,
      command = {
        df_pred_grid <- file.path(
          chr_input_dir, "prediction", "df_pred_grid.rds"
        )
        df_pred_grid$site_id <- sprintf("P%08d", df_pred_grid$site_id)
        sf_pred_grid <- sf::st_as_sf(
          df_pred_grid, coords = c("lon", "lat"), crs = 5070
        )
        num_size_subgrid <- 1000
        num_pred_list_length <- nrow(sf_pred_grid) / num_size_subgrid
        sf_pred_grid$group <- rep(
          1:num_pred_list_length, length.out = nrow(sf_pred_grid)
        )
        list_pred_grid <- split(sf_pred_grid, sf_pred_grid$group)
        list_pred_grid <- lapply(
          list_pred_grid, function(x) x[1:10, "site_id"]
        )
        list_pred_grid[1:2]
      },
      description = "Prediction locations | pred | DEV (2 list objects; 10 sites each)"
    ),
    targets::tar_target(
      dt_feat_pred_sites,
      command = sf_feat_pred_sites %>%
        sf::st_drop_geometry() %>%
        tidyr::crossing(chr_dates) %>%
        dplyr::rename(time = chr_dates) %>%
        data.table::data.table(),
      description = "Prediction locations with time | pred"
    ),
    ############################################################################
    ###########################         GEOS         ###########################
    targets::tar_target(
      list_feat_pred_geos_aqc,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[1]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = list_feat_pred_sites[[1]],
          locs_id = "site_id"
        )
      },
      pattern = cross(list_feat_pred_sites, list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | aqc | pred"
    ),
    targets::tar_target(
      list_feat_pred_geos_chm,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[2]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = list_feat_pred_sites[[1]],
          locs_id = "site_id"
        )
      },
      pattern = cross(list_feat_pred_sites, list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | chm | pred"
    ),
    # targets::tar_target(
    #   dt_feat_pred_geos,
    #   command = beethoven::reduce_merge(
    #     c(
    #       beethoven::reduce_list(list_feat_pred_geos_aqc),
    #       beethoven::reduce_list(list_feat_pred_geos_chm)
    #     ),
    #     by = c("site_id", "time", "CO", "NO2", "SO2")
    #   ),
    #   description = "data.table of GEOS-CF features | pred"
    # ),
    ###########################         NARR         ###########################
    targets::tar_target(
      list_feat_pred_narr,
      command = {
        download_narr_buffer
        dt_iter_pred_narr <- amadeus::calculate_narr(
          from = amadeus::process_narr(
            path = file.path(chr_input_dir, "narr", chr_iter_calc_narr),
            variable = chr_iter_calc_narr,
            date = beethoven::fl_dates(unlist(list_dates))
          ),
          locs = list_feat_pred_sites[[1]],
          locs_id = "site_id",
          radius = 0,
          fun = "mean",
          geom = FALSE
        )
        if (length(grep("level", names(dt_iter_pred_narr))) == 1) {
          dt_iter_pred_narr <-
            dt_iter_pred_narr[dt_iter_pred_narr$level == 1000, ]
          dt_iter_pred_narr <-
            dt_iter_pred_narr[, -grep("level", names(dt_iter_pred_narr))]
        }
        beethoven::post_calc_cols(
          dt_iter_pred_narr,
          prefix = "NARR_0_"
        )
      },
      pattern = cross(list_feat_pred_sites, list_dates, chr_iter_calc_narr),
      iteration = "list",
      description = "Calculate NARR features | nolag | pred"
    ),
    # targets::tar_target(
    #   dt_feat_pred_narr_nolag,
    #   command = beethoven::reduce_merge(
    #     beethoven::reduce_list(list_feat_pred_narr),
    #     by = c("site_id", "time")
    #   ),
    #   description = "data.table of NARR features | nolag | pred"
    # ),
    targets::tar_target(
      list_feat_pred_narr_lag,
      command = {
        download_narr_buffer
        dt_lag_pred_narr <- amadeus::calculate_narr(
          from = amadeus::process_narr(
            path = file.path(chr_input_dir, "narr", chr_iter_calc_narr_lag),
            variable = chr_iter_calc_narr_lag,
            date = as.character(chr_dates[1] - 1)
          ),
          locs = dplyr::bind_rows(list_feat_pred_sites),
          locs_id = "site_id",
          radius = 0,
          fun = "mean",
          geom = FALSE
        )
        if (length(grep("level", names(dt_lag_pred_narr))) == 1) {
          dt_lag_pred_narr <-
            dt_lag_pred_narr[dt_lag_pred_narr$level == 1000, ]
          dt_lag_pred_narr <-
            dt_lag_pred_narr[, -grep("level", names(dt_lag_pred_narr))]
        }
        dt_lag_pred_narr <- beethoven::post_calc_cols(
          dt_lag_pred_narr,
          prefix = "NARR_0_"
        )
        int_narr_cols <- which(
          names(dt_feat_pred_narr_nolag) %in% names(dt_lag_pred_narr)
        )
        dt_iter_pred_narr_bind <- rbind(
          dt_lag_pred_narr,
          dt_feat_pred_narr_nolag[, int_narr_cols, with = FALSE]
        )
        amadeus::calculate_lagged(
          from = dt_iter_pred_narr_bind,
          date = chr_daterange,
          lag = 1,
          locs_id = "site_id"
        )
      },
      pattern = map(chr_iter_calc_narr_lag),
      iteration = "list",
      description = "Calculate NARR features | lag | pred"
    ),
    # targets::tar_target(
    #   dt_feat_calc_narr,
    #   command = beethoven::reduce_merge(
    #     beethoven::reduce_list(
    #       c(list(dt_feat_calc_narr_nolag), list_feat_calc_narr_lag)
    #     ),
    #     by = c("site_id", "time")
    #   ),
    #   description = "data.table of NARR features | pred"
    # ),
    ###########################         HMS          ###########################
    targets::tar_target(
      list_feat_pred_hms,
      command = {
        download_hms_buffer
        dt_iter_pred_hms <- beethoven::inject_calculate(
          covariate = "hms",
          locs = list_feat_pred_sites[[1]],
          injection = list(
            path = file.path(chr_input_dir, "hms", "data_files"),
            date = beethoven::fl_dates(unlist(list_dates)),
            covariate = "hms"
          )
        )[[1]] |>
          dplyr::select(-dplyr::any_of(c("lon", "lat", "geometry", "hms_year")))
        beethoven::post_calc_cols(
          dt_iter_pred_hms,
          prefix = "HMS_"
        )
      },
      pattern = cross(list_feat_pred_sites, list_dates),
      iteration = "list",
      description = "Calculate HMS features | pred"
    ),
    # targets::tar_target(
    #   dt_feat_pred_hms,
    #   command = beethoven::reduce_list(list_feat_pred_hms)[[1]],
    #   description = "data.table of HMS features | pred"
    # ),
    ###########################       MODIS - MOD11       ######################
    targets::tar_target(
      list_feat_pred_mod11,
      command = beethoven::inject_modis(
        locs = list_feat_pred_sites[[1]],
        injection = list_args_calc_mod11
      ),
      pattern = cross(list_feat_pred_sites, list_args_calc_mod11),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD11 features | pred"
    ),
    ###########################       MODIS - MOD06       ######################
    targets::tar_target(
      list_feat_pred_mod06,
      command = beethoven::inject_modis(
        locs = list_feat_pred_sites[[1]],
        injection = list_args_calc_mod06
      ),
      pattern = cross(list_feat_pred_sites, list_args_calc_mod06),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD06 features | pred"
    ),
    ###########################       MODIS - MOD13       ######################
    targets::tar_target(
      list_feat_pred_mod13,
      command = beethoven::inject_modis(
        locs = list_feat_pred_sites[[1]],
        injection = list_args_calc_mod13
      ),
      pattern = cross(list_feat_pred_sites, list_args_calc_mod13),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "Calculate MODIS - MOD13 features | pred"
    ),
    ###########################     MODIS - MCD19_1km     ######################
    targets::tar_target(
      list_feat_pred_mcd19_1km,
      command = beethoven::inject_modis(
        locs = list_feat_pred_sites[[1]],
        injection = list_args_calc_mcd19_1km
      ),
      pattern = cross(list_feat_pred_sites, list_args_calc_mcd19_1km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_250")
      ),
      description = "Calculate MODIS - MCD19_1km features | pred"
    ),
    ###########################     MODIS - MCD19_5km     ######################
    targets::tar_target(
      list_feat_pred_mcd19_5km,
      command = beethoven::inject_modis(
        locs = list_feat_pred_sites[[1]],
        injection = list_args_calc_mcd19_5km
      ),
      pattern = cross(list_feat_pred_sites, list_args_calc_mcd19_5km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_250")
      ),
      description = "Calculate MODIS - MCD19_5km features | pred"
    ),
    ###########################       MODIS - MOD09       ######################
    targets::tar_target(
      list_feat_pred_mod09,
      command = beethoven::inject_modis(
        locs = list_feat_pred_sites[[1]],
        injection = list_args_calc_mod09
      ),
      pattern = cross(list_feat_pred_sites, list_args_calc_mod09),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_25")
      ),
      description = "Calculate MODIS - MOD09 features | pred"
    ),
    ###########################       MODIS - VIIRS       ######################
    targets::tar_target(
      list_feat_pred_viirs,
      command = beethoven::inject_modis(
        locs = list_feat_pred_sites[[1]],
        injection = list_args_calc_viirs
      ),
      pattern = cross(list_feat_pred_sites, list_args_calc_viirs),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "Calculate MODIS - VIIRS features | pred"
    ),
    ###########################        MODIS/VIIRS        ######################
    # targets::tar_target(
    #   dt_feat_pred_nasa,
    #   command = beethoven::reduce_merge(
    #     lapply(
    #       list(
    #         list_feat_pred_mod11,
    #         list_feat_pred_mod06,
    #         list_feat_pred_mod13,
    #         list_feat_pred_mcd19_1km,
    #         list_feat_pred_mcd19_5km,
    #         list_feat_pred_mod09,
    #         list_feat_pred_viirs
    #       ),
    #       function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
    #     ),
    #     by = NULL
    #   ),
    #   description = "data.table of MODIS/VIIRS features | pred"
    # ),
    ###########################         GMTED        ###########################
    targets::tar_target(
      list_feat_pred_gmted,
      command = {
        download_gmted
        beethoven::calc_gmted_direct(
          variable = c(chr_iter_calc_gmted_vars, "7.5 arc-seconds"),
          path = file.path(chr_input_dir, "gmted", "data_files"),
          locs = list_feat_pred_sites[[1]],
          locs_id = "site_id",
          radius = chr_iter_calc_gmted_radii
        )
      },
      iteration = "list",
      pattern = cross(
        list_feat_pred_sites,
        chr_iter_calc_gmted_vars,
        chr_iter_calc_gmted_radii
      ),
      description = "Calculate GMTED features | pred"
    ),
    # targets::tar_target(
    #   dt_feat_pred_gmted,
    #   command = beethoven::reduce_merge(
    #     beethoven::reduce_list(list_feat_pred_gmted),
    #     by = "site_id"
    #   ),
    #   description = "data.table of GMTED features | pred"
    # ),
    ###########################         NLCD         ###########################
    targets::tar_target(
      list_feat_pred_nlcd,
      command = {
        download_nlcd
        beethoven::inject_nlcd(
          locs = dplyr::bind_rows(list_feat_pred_sites),
          # NOTE: locs are all AQS sites for computational efficiency
          locs_id = "site_id",
          year = df_feat_calc_nlcd_params$year,
          radius = df_feat_calc_nlcd_params$radius,
          from = amadeus::process_nlcd(
            path = file.path(chr_input_dir, "nlcd", "data_files"),
            year = df_feat_calc_nlcd_params$year
          ),
          nthreads = 1,
          mode = "exact",
          max_cells = 3e7
        )
      },
      iteration = "list",
      pattern = map(df_feat_calc_nlcd_params),
      description = "Calculate NLCD features | pred"
    ),
    # targets::tar_target(
    #   name = dt_feat_pred_nlcd,
    #   command = list_feat_pred_nlcd %>%
    #     collapse::rowbind(fill = TRUE) %>%
    #     collapse::funique() %>%
    #     collapse::pivot(
    #       ids = c("site_id", "time"),
    #       values = names(.)[
    #         !names(.) %in%
    #           c(
    #             "site_id",
    #             "time"
    #           )
    #       ]
    #     ) %>%
    #     .[!is.na(.[["value"]]), ] %>%
    #     collapse::pivot(
    #       ids = c("site_id", "time"),
    #       values = c("value"),
    #       how = "wider"
    #     ),
    #   description = "NLCD feature list (all dt) | pred"
    # ),
    ###########################        KOPPEN        ###########################
    targets::tar_target(
      dt_feat_pred_koppen,
      command = {
        download_koppen
        data.table::data.table(
          amadeus::calculate_koppen_geiger(
            from = amadeus::process_koppen_geiger(
              path = file.path(
                chr_input_dir,
                "koppen_geiger",
                "data_files",
                "Beck_KG_V1_present_0p0083.tif"
              )
            ),
            locs = dplyr::bind_rows(list_feat_pred_sites),
            # NOTE: locs are all AQS sites for computational efficiency
            locs_id = "site_id",
            geom = FALSE
          )
        )
      },
      description = "data.table of Koppen Geiger features | pred"
    ),
    ###########################      POPULATION      ###########################
    targets::tar_target(
      list_feat_pred_pop,
      command = {
        download_population
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
          locs = list_feat_pred_sites[[1]],
          locs_id = "site_id",
          geom = FALSE,
          radius = chr_iter_radii
        )
      },
      pattern = cross(list_feat_pred_sites, chr_iter_radii),
      iteration = "list",
      description = "Calculate population features | pred"
    ),
    # targets::tar_target(
    #   dt_feat_pred_pop,
    #   command = beethoven::reduce_merge(
    #     beethoven::reduce_list(list_feat_pred_pop)
    #   ),
    #   description = "data.table of population features | pred"
    # ),
    ###########################         TRI          ###########################
    targets::tar_target(
      list_feat_pred_tri,
      command = {
        download_tri
        beethoven::inject_calculate(
          covariate = "tri",
          locs = dplyr::bind_rows(list_feat_pred_sites),
          # NOTE: locs are all AQS sites for computational efficiency
          injection = list(
            domain = df_feat_calc_tri_params$year,
            domain_name = "year",
            path = file.path(chr_input_dir, "tri"),
            variables = c(1, 13, 12, 14, 20, 34, 36, 47, 48, 49),
            radius = df_feat_calc_tri_params$radius,
            nthreads = 1,
            covariate = "tri"
          )
        )
      },
      iteration = "list",
      pattern = map(df_feat_calc_tri_params),
      description = "Calculate TRI features | pred"
    ),
    targets::tar_target(
      list_feat_pred_reduce_tri,
      command = {
        list_feat_pred_tri_unnest <- lapply(
          list_feat_pred_tri,
          function(x) x[[1]]
        )
        chr_tri_radii_index <- sapply(
          list_feat_pred_tri_unnest,
          function(x) {
            any(grepl(sprintf("_%05d", chr_iter_radii), names(x)))
          }
        )
        beethoven::reduce_merge(
          list_feat_pred_tri_unnest[chr_tri_radii_index],
          by = NULL,
          all.x = TRUE,
          all.y = TRUE
        )
      },
      iteration = "list",
      pattern = map(chr_iter_radii),
      description = "Reduce TRI features based on radii | pred"
    ),
    # targets::tar_target(
    #   dt_feat_pred_tri,
    #   command = {
    #     dt_feat_merge_tri <- beethoven::reduce_merge(
    #       list_feat_pred_reduce_tri,
    #       by = c("site_id", "time", "lon", "lat", "tri_year"),
    #       all.x = TRUE,
    #       all.y = TRUE
    #     )
    #     dt_feat_merge_tri[is.na(dt_feat_merge_tri)] <- 0
    #     dt_feat_pca_tri <- beethoven::post_calc_pca(
    #       data = dt_feat_merge_tri,
    #       yvar = NULL,
    #       num_comp = 5,
    #       pattern = "FUGITIVE|STACK",
    #       groups = sprintf("%05d", chr_iter_radii),
    #       prefix = "TRI",
    #       kernel = TRUE
    #     )
    #   },
    #   description = "data.table of TRI PCA-reduced features | pred"
    # ),
    ###########################         NEI          ###########################
    targets::tar_target(
      list_feat_pred_nei,
      command = {
        download_nei
        beethoven::inject_calculate(
          covariate = "nei",
          locs = list_feat_pred_sites[[1]],
          injection = list(
            domain = chr_iter_calc_nei,
            domain_name = "year",
            path = file.path(chr_input_dir, "nei", "data_files"),
            covariate = "nei"
          )
        )
      },
      iteration = "list",
      pattern = cross(list_feat_pred_sites, chr_iter_calc_nei),
      description = "Calculate NEI features | pred"
    ),
    # targets::tar_target(
    #   dt_feat_pred_nei,
    #   command = beethoven::reduce_list(
    #     lapply(
    #       list_feat_pred_nei,
    #       function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
    #     )
    #   )[[1]],
    #   description = "data.table of NEI features | pred"
    # ),
    ###########################      ECOREGIONS      ###########################
    targets::tar_target(
      dt_feat_pred_ecoregions,
      command = {
        download_ecoregions
        data.table::data.table(
          amadeus::calculate_ecoregion(
            from = amadeus::process_ecoregion(
              path = file.path(
                chr_input_dir,
                "ecoregions",
                "data_files",
                "us_eco_l3_state_boundaries.shp"
              )
            ),
            locs = dplyr::bind_rows(list_feat_pred_sites),
            # NOTE: locs are all AQS sites for computational efficiency
            locs_id = "site_id"
          )
        )
      },
      description = "data.table of Ecoregions features | pred"
    ),
    ###########################        GROADS        ###########################
    targets::tar_target(
      list_feat_pred_groads,
      command = {
        download_groads
        amadeus::calculate_groads(
          from = amadeus::process_groads(
            path = file.path(
              chr_input_dir,
              "groads",
              "data_files",
              "gROADS-v1-americas.gdb"
            )
          ),
          locs = dplyr::bind_rows(list_feat_pred_sites),
          # NOTE: locs are all AQS sites for computational efficiency
          locs_id = "site_id",
          radius = chr_iter_radii
        )
      },
      iteration = "list",
      pattern = map(chr_iter_radii),
      description = "Calculate gRoads features | pred"
    )
    # targets::tar_target(
    #   dt_feat_pred_groads,
    #   command = beethoven::reduce_merge(
    #     beethoven::reduce_list(list_feat_pred_groads),
    #     by = c("site_id", "description")
    #   ),
    #   description = "data.table of gRoads features | pred"
    # ),
    ########################       DATE FEATURES       #########################
    # targets::tar_target(
    #   dt_feat_pred_date,
    #   command = Reduce(
    #     beethoven::post_calc_autojoin,
    #     list(
    #       dt_feat_pred_geos,
    #       dt_feat_pred_narr,
    #       dt_feat_pred_nasa
    #     )
    #   ),
    #   description = "data.table of all features | pred"
    # ),
    ########################       BASE FEATURES       #########################
    # targets::tar_target(
    #   list_feat_pred_base_flat,
    #   command = lapply(
    #     list(
    #       list(dt_feat_pred_hms),
    #       list(dt_feat_pred_tri),
    #       list(dt_feat_pred_nei),
    #       list(dt_feat_pred_ecoregions),
    #       list(dt_feat_pred_koppen),
    #       list(dt_feat_pred_pop),
    #       list(dt_feat_pred_groads)
    #     ),
    #     function(x) {
    #       if (length(x) == 1) {
    #         x[[1]]
    #       } else if (
    #         sum(grepl("light|medium|heavy", sapply(x, \(t) names(t)))) == 3
    #       ) {
    #         xr <- lapply(x, \(dt) {
    #           dta <- data.table::copy(dt)
    #           dta <- dta[, time := as.character(time)]
    #           return(dta)
    #         })
    #         xrr <- Reduce(
    #           function(x, y) {
    #             collapse::join(x, y, on = c("site_id", "time"), how = "full")
    #           },
    #           xr
    #         )
    #         return(xrr)
    #       } else {
    #         collapse::rowbind(x, use.names = TRUE, fill = TRUE)
    #       }
    #     }
    #   ),
    #   description = "Calculated base feature list (all dt) | pred"
    # ),
    # targets::tar_target(
    #   dt_feat_pred_base,
    #   command = Reduce(
    #     beethoven::post_calc_autojoin,
    #     c(
    #       list(dt_feat_pred_sites),
    #       list_feat_pred_base_flat,
    #       list(dt_feat_pred_gmted),
    #       list(data.table::data.table(dt_feat_pred_nlcd))
    #     )
    #   ),
    #   description = "Base features with full temporal range | pred"
    # ),
    #######################     CUMULATIVE FEATURES      #######################
    # targets::tar_target(
    #   dt_feat_pred_design,
    #   command = beethoven::post_pred_autojoin(
    #     dt_feat_pred_base,
    #     dt_feat_pred_date,
    #     year_start = as.integer(substr(chr_daterange[1], 1, 4)),
    #     year_end = as.integer(substr(chr_daterange[2], 1, 4))
    #   ),
    #   description = "data.table of all features with PM2.5 | pred"
    # ),
    # targets::tar_target(
    #   dt_feat_pred_imputed,
    #   command = beethoven::impute_all(
    #     dt_feat_pred_design,
    #     period = chr_daterange,
    #     nthreads_dt = 32,
    #     nthreads_collapse = 32,
    #     nthreads_imputation = 32
    #   ),
    #   description = "Imputed features + lags | pred"
    # ),
    # targets::tar_target(
    #   name = dt_feat_pred_xyt,
    #   command = data.table::data.table(
    #     beethoven::attach_xy(
    #       dt_feat_pred_imputed,
    #       dplyr::bind_rows(list_feat_pred_sites)
    #     )
    #   ),
    #   description = "Imputed features + AQS sites (outcome and lat/lon) | pred"
    # )
  )
