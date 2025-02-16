
## Targets for prediction grid features ####
## FIXME: align with the "_fit" targets
## TODO: chopin's gridset implementation
## TODO: prospective or retrospective, or both?
# strategy: no all-merged data.frame (data.table)
#           in-branch processing (using the branched target then pass it to targets_predict)
# Each branched target should have the same degree of interaction (i.e., cross())
target_calculate_predict <-
  list(
    targets::tar_target(
      library,
      command = .Library
    ),
    targets::tar_target(
      libPaths,
      command = .libPaths()
    ),
    targets::tar_target(
      df_pred_calc_grid,
      command = qs::qread(
        list.files(
          path = file.path("inst", "extdata"),
          pattern = "prediction_grid.qs",
          full.names = TRUE
        )
      ),
      description = "Import prediction grid"
    ),
    targets::tar_target(
      sf_pred_raw_grid,
      command = sf::st_transform(
        sf::st_as_sf(
          df_pred_calc_grid,
          coords = c("lon", "lat"),
          crs = 5070
        ),
        crs = 4326
      ),
      description = "Prediction grid as sf (no coordinates)"
    ),
    targets::tar_target(
      df_pred_raw_grid,
      command = sf::st_coordinates(sf_pred_raw_grid),
      description = "Prediction grid coordinates as df"
    ),
    targets::tar_target(
      sf_pred_calc_grid,
      command = cbind(
        sf_pred_raw_grid,
        lon = df_pred_raw_grid[, 1],
        lat = df_pred_raw_grid[, 2]
      ),
      description = "Prediction grid as sf (with coordinates)"
    )
    ,
    targets::tar_target(
      df_pred_calc_gridcoords,
      command = sf::st_drop_geometry(
        sf_pred_calc_grid
      ),
      description = "Prediction grid as tibble (with coordinates)"
    )
    ,
    targets::tar_target(
      name = chr_pred_calc_grid,
      command = names(list_pred_calc_grid),
      description = "Names of prediction grid sf objects (SAMPLE)"
    )
    ,
    targets::tar_target(
      list_pred_split_dates,
      command = split_dates(arglist_common$char_period, 50),
      description = "Split period into list of dates"
    )
    ,
    targets::tar_target(
      chr_pred_split_dates,
      command = names(list_pred_split_dates),
      description = "Names of split dates lists"
    )
    ,
    ###########################################################################
    ##### SAMPLE TARGETS FOR DEVELOPMENT #####
    targets::tar_target(
      sf_pred_calc_split,
      command = {
        chopin::par_pad_grid(
          sf_pred_calc_grid,
          mode = "grid",
          nx = 40L,
          ny = 20L,
          padding = 100
        )[[1]]
      },
      iteration = "vector",
      description = "sf split grid polygons"
    )
    ,
    targets::tar_target(
      list_pred_calc_grid,
      command = {
        grid_unit <- sf::st_bbox(sf_pred_calc_split)
        sf::st_as_sf(
          df_pred_calc_gridcoords |>
            dplyr::filter((lon <= grid_unit[3] & lon >= grid_unit[1]) & (lat <= grid_unit[4] & lat >= grid_unit[2])),
          coords = c("lon", "lat"),
          crs = 4326,
          remove = FALSE
        )
      },
      iteration = "list",
      pattern = map(sf_pred_calc_split),
      description = "Split prediction grid into list by chopin grid (DEV SAMPLE)",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    )
    ,
    ###########################         HMS          ###########################
    targets::tar_target(
      list_pred_calc_hms,
      command = {
        beethoven::inject_calculate(
          covariate = "hms",
          locs = list_pred_calc_grid,
          injection = list(
            path = file.path(chr_input_dir, "hms", "data_files"),
            date = beethoven::fl_dates(unlist(list_dates)),
            covariate = "hms"
          )
        )[[1]] |>
          dplyr::select(-dplyr::any_of(c("lon", "lat", "geometry", "hms_year")))
      },
      pattern = cross(list_pred_calc_grid, list_dates),
      iteration = "list",
      description = "Calculate HMS features | prediction",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    )
    ,
    ##############################   NLCD    #############################
    targets::tar_target(
      name = list_pred_split_calc_nlcd,
      command =
      {
      mapply(
        function(yr, radi) {
          inject_nlcd(
            year = yr,
            radius = radi,
            from = amadeus::process_nlcd(
              path = file.path(chr_input_dir, "nlcd", "data_files"),
              year = df_feat_calc_nlcd_params$year
            ),
            locs = list_pred_calc_grid,
            locs_id = arglist_common$char_siteid,
            nthreads = 16L,
            mode = "exact",
            max_cells = 3e7
          )
        },
        df_feat_calc_nlcd_params$year,
        df_feat_calc_nlcd_params$radius,
        SIMPLIFY = FALSE) |>
        beethoven::post_calc_merge_all()
      }
      ,
      pattern = map(
        list_pred_calc_grid,
      ),
      iteration = "list",
      description = "Calculate NLCD features with branched sublists (pred)",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      )
    )
    ,
    ##############################   GMTED    #############################
    targets::tar_target(
      chr_pred_calc_gmted_radii,
      # command = c(0, 1e3, 1e4, 5e4),
      command = c(200),
      description = "Radii for GMTED features"
    )
    ,
    targets::tar_target(
      name = list_pred_split_calc_gmted,
      command =
      {
        mapply(
          function(var, radi) {
            beethoven::calc_gmted_direct(
              variable = c(var, "7.5 arc-seconds"),
              path = file.path(chr_input_dir, "gmted", "data_files"),
              locs = list_pred_calc_grid,
              locs_id = "site_id",
              radius = radi
            )
          },
          chr_iter_calc_gmted_vars,
          chr_pred_calc_gmted_radii,
          SIMPLIFY = FALSE
        ) |>
        beethoven::post_calc_merge_all()
      },
      iteration = "list",
      pattern = map(
        list_pred_calc_grid
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate GMTED features with branched sublists (pred)"
    )
    ,
    ###########################         GEOS         ###########################
    targets::tar_target(
      list_pred_calc_geos_aqc,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[1]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = list_pred_calc_grid,
          locs_id = "site_id"
        )
      },
      pattern = cross(list_pred_calc_grid, list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | aqc | prediction"
    )
    ,
    targets::tar_target(
      list_pred_calc_geos_chm,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[2]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = list_pred_calc_grid,
          locs_id = "site_id"
        )
      },
      pattern = cross(list_pred_calc_grid, list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | chm | prediction"
    )
    ,
    # targets::tar_target(
    #   dt_feat_calc_geos,
    #   command = beethoven::reduce_merge(
    #     c(
    #       beethoven::reduce_list(list_pred_calc_geos_aqc),
    #       beethoven::reduce_list(list_pred_calc_geos_chm)
    #     ),
    #     by = c("site_id", "time", "CO", "NO2", "SO2")
    #   ),
    #   description = "data.table of GEOS-CF features | fit"
    # )
    # ,
    ###########################         NARR         ###########################
    # ????
    targets::tar_target(
      list_pred_calc_narr,
      command = {
        download_narr_buffer
        dt_iter_calc_narr <- amadeus::calculate_narr(
          from = amadeus::process_narr(
            path = file.path(chr_input_dir, "narr", chr_iter_calc_narr),
            variable = chr_iter_calc_narr,
            date = beethoven::fl_dates(unlist(list_dates))
          ),
          locs = list_pred_calc_grid,
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
      },
      pattern = cross(list_pred_calc_grid, list_dates, chr_iter_calc_narr),
      iteration = "list",
      description = "Calculate NARR features | prediction"
    )
    ,
    # targets::tar_target(
    #   dt_feat_calc_narr,
    #   command = beethoven::reduce_merge(
    #     beethoven::reduce_list(list_pred_calc_narr),
    #     by = c("site_id", "time")
    #   ),
    #   description = "data.table of NARR features | fit"
    # )
    # ,

    ###########################       MODIS - MOD11       ######################
    # targets::tar_target(
    #   chr_args_calc_mod11_files,
    #   command = {
    #     download_mod11
    #     list.files(
    #       file.path(chr_input_dir, "modis", "raw", "61", "MOD11A1"),
    #       full.names = TRUE,
    #       recursive = TRUE
    #     )
    #   },
    #   description = "MODIS - MOD11 files"
    # )
    # ,
    # targets::tar_target(
    #   list_args_calc_mod11,
    #   command = list(
    #     from = grep(
    #       x = chr_args_calc_mod11_files,
    #       pattern = paste0(
    #         "MOD11A1.A", unlist(list_dates_julian), collapse = "|"
    #       ),
    #       value = TRUE
    #     ),
    #     name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
    #     subdataset = "^LST_",
    #     radius = chr_iter_radii
    #   ),
    #   pattern = map(list_dates_julian),
    #   iteration = "list",
    #   description = "MODIS - MOD11 arguments"
    # )
    # ,
    targets::tar_target(
      list_pred_calc_mod11,
      command = {
        calculate_modis_direct(
          file = chr_list_calc_mod11_files,
          site = list_pred_calc_grid,
          site_id = arglist_common[["char_siteid"]],
          radius = chr_iter_radii,
          colheader = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
          mark = TRUE
        )
      },
      pattern = cross(chr_list_calc_mod11_files, list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD11 features | prediction"
    )
    ,
    # targets::tar_target(
    #   list_pred_calc_mod11,
    #   command = beethoven::inject_modis(
    #     locs = list_pred_calc_grid,
    #     injection = list_args_calc_mod11
    #   ),
    #   pattern = cross(list_pred_calc_grid, list_args_calc_mod11),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_50")
    #   ),
    #   description = "Calculate MODIS - MOD11 features | fit"
    # )
    # ,
    ###########################       MODIS - MOD06       ######################
    targets::tar_target(
      list_pred_calc_mod06,
      command = {
        calculate_modis_direct(
          file = chr_list_calc_mod06_files,
          site = list_pred_calc_grid,
          site_id = arglist_common[["char_siteid"]],
          radius = chr_iter_radii,
          colheader = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
          mark = TRUE
        )
      },
      pattern = cross(chr_list_calc_mod06_files, list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD06 features | prediction"
    )
    ,
    # targets::tar_target(
    #   chr_args_calc_mod06_files,
    #   command = {
    #     download_mod06
    #     list.files(
    #       file.path(chr_input_dir, "modis", "raw", "61", "MOD06_L2"),
    #       full.names = TRUE,
    #       recursive = TRUE
    #     )
    #   },
    #   description = "MODIS - MOD06 files"
    # )
    # ,
    # targets::tar_target(
    #   list_args_calc_mod06,
    #   command = list(
    #     from = grep(
    #       x = chr_args_calc_mod06_files,
    #       pattern = paste0(
    #         "MOD06_L2.A", unlist(list_dates_julian), collapse = "|"
    #       ),
    #       value = TRUE
    #     ),
    #     name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
    #     subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
    #     preprocess = amadeus::process_modis_swath,
    #     radius = chr_iter_radii
    #   ),
    #   pattern = map(list_dates_julian),
    #   iteration = "list",
    #   description = "MODIS - MOD06 arguments"
    # )
    # ,
    # targets::tar_target(
    #   list_pred_calc_mod06,
    #   command = beethoven::inject_modis(
    #     locs = list_pred_calc_grid,
    #     injection = list_args_calc_mod06
    #   ),
    #   pattern = cross(list_pred_calc_grid, list_args_calc_mod06),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_50")
    #   ),
    #   description = "Calculate MODIS - MOD06 features | fit"
    # )
    # ,
    ###########################       MODIS - MOD13       ######################
    targets::tar_target(
      list_pred_calc_mod13,
      command = {
        calculate_modis_direct(
          file = chr_list_calc_mod13_files,
          site = list_pred_calc_grid,
          site_id = arglist_common[["char_siteid"]],
          radius = chr_iter_radii,
          colheader = "MOD_NDVIV_0_",
          mark = TRUE
        )
        # chopin::extract_at(
        #   chr_list_calc_mod13_files,
        #   sf::st_as_sf(
        #     list_feat_proc_aqs_sites,
        #     sf_column_name = "geometry"),
        #   radius = chr_iter_radii
        # )
      },
      pattern = cross(chr_list_calc_mod13_files, list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD13 features | prediction"
    )
    ,
    # targets::tar_target(
    #   chr_args_calc_mod13_files,
    #   command = {
    #     download_mod13
    #     list.files(
    #       file.path(chr_input_dir, "modis", "raw", "61", "MOD13A2"),
    #       full.names = TRUE,
    #       recursive = TRUE
    #     )
    #   },
    #   description = "MODIS - MOD13 files"
    # )
    # ,
    # targets::tar_target(
    #   list_args_calc_mod13,
    #   command = list(
    #     from = grep(
    #       x = chr_args_calc_mod13_files,
    #       pattern = paste0(
    #         "MOD13A2.A", unlist(list_dates_julian), collapse = "|"
    #       ),
    #       value = TRUE
    #     ),
    #     name_covariates = "MOD_NDVIV_0_",
    #     subdataset = "(NDVI)",
    #     radius = chr_iter_radii
    #   ),
    #   pattern = map(list_dates_julian),
    #   iteration = "list",
    #   description = "MODIS - MOD13 arguments"
    # )
    # ,
    # targets::tar_target(
    #   list_pred_calc_mod13,
    #   command = beethoven::inject_modis(
    #     locs = list_pred_calc_grid,
    #     injection = list_args_calc_mod13
    #   ),
    #   pattern = cross(list_pred_calc_grid, list_args_calc_mod13),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_100")
    #   ),
    #   description = "Calculate MODIS - MOD13 features | fit"
    # )
    # ,
    ###########################     MODIS - MCD19_1km     ######################
    targets::tar_target(
      list_pred_calc_mcd19_1km,
      command = {
        calculate_modis_direct(
          file = chr_list_calc_mcd19_1km_files,
          site = list_pred_calc_grid,
          site_id = arglist_common[["char_siteid"]],
          radius = chr_iter_radii,
          colheader = c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
          mark = TRUE
        )
        # chopin::extract_at(
        #   chr_list_calc_mcd19_files,
        #   sf::st_as_sf(
        #     list_feat_proc_aqs_sites,
        #     sf_column_name = "geometry"),
        #   radius = chr_iter_radii
        # )
      },
      pattern = cross(chr_list_calc_mcd19_1km_files, list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MCD19_1km features | prediction"
    )
    ,
    # targets::tar_target(
    #   chr_args_calc_mcd19_files,
    #   command = {
    #     download_mcd19
    #     list.files(
    #       file.path(chr_input_dir, "modis", "raw", "61", "MCD19A2"),
    #       full.names = TRUE,
    #       recursive = TRUE
    #     )
    #   },
    #   description = "MODIS - MCD19_*km files"
    # )
    # ,
    # targets::tar_target(
    #   list_args_calc_mcd19_1km,
    #   command = list(
    #     from = grep(
    #       x = chr_args_calc_mcd19_files,
    #       pattern = paste0(
    #         "MCD19A2.A", unlist(list_dates_julian), collapse = "|"
    #       ),
    #       value = TRUE
    #     ),
    #     name_covariates = c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
    #     subdataset = "^Optical_Depth",
    #     radius = chr_iter_radii
    #   ),
    #   pattern = map(list_dates_julian),
    #   iteration = "list",
    #   description = "MODIS - MCD19_1km arguments"
    # )
    # ,
    # targets::tar_target(
    #   list_pred_calc_mcd19_1km,
    #   command = beethoven::inject_modis(
    #     locs = list_pred_calc_grid,
    #     injection = list_args_calc_mcd19_1km
    #   ),
    #   pattern = cross(list_pred_calc_grid, list_args_calc_mcd19_1km),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_250")
    #   ),
    #   description = "Calculate MODIS - MCD19_1km features | fit"
    # )
    # ,
    ###########################     MODIS - MCD19_5km     ######################
    targets::tar_target(
      list_pred_calc_mcd19_5km,
      command = {
        calculate_modis_direct(
          file = chr_list_calc_mcd19_5km_files,
          site = list_pred_calc_grid,
          site_id = arglist_common[["char_siteid"]],
          radius = chr_iter_radii,
          colheader = c(
          "MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
          "MOD_SCTAN_0_", "MOD_GLNAN_0_"
          ),
          mark = TRUE
        )
      },
      pattern = cross(chr_list_calc_mcd19_5km_files, list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MCD19_5km features | prediction"
    )
    ,
    # targets::tar_target(
    #   list_args_calc_mcd19_5km,
    #   command = list(
    #     from = grep(
    #       x = chr_args_calc_mcd19_files,
    #       pattern = paste0(
    #         "MCD19A2.A", unlist(list_dates_julian), collapse = "|"
    #       ),
    #       value = TRUE
    #     ),
    #     name_covariates = c(
    #       "MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
    #       "MOD_SCTAN_0_", "MOD_GLNAN_0_"
    #     ),
    #     subdataset = "cos|RelAZ|Angle",
    #     radius = chr_iter_radii
    #   ),
    #   pattern = map(list_dates_julian),
    #   iteration = "list",
    #   description = "MODIS - MCD19_5km arguments"
    # )
    # ,
    # targets::tar_target(
    #   list_pred_calc_mcd19_5km,
    #   command = beethoven::inject_modis(
    #     locs = list_pred_calc_grid,
    #     injection = list_args_calc_mcd19_5km
    #   ),
    #   pattern = cross(list_pred_calc_grid, list_args_calc_mcd19_5km),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_250")
    #   ),
    #   description = "Calculate MODIS - MCD19_5km features | fit"
    # )
    # ,
    ###########################       MODIS - MOD09       ######################
    targets::tar_target(
      list_pred_calc_mod09,
      command = {
        calculate_modis_direct(
          file = chr_list_calc_mod09_files,
          site = list_pred_calc_grid,
          site_id = arglist_common[["char_siteid"]],
          radius = chr_iter_radii,
          colheader = c(
          "MOD_SFCRF_1_", "MOD_SFCRF_2_", "MOD_SFCRF_3_", "MOD_SFCRF_4_",
          "MOD_SFCRF_5_", "MOD_SFCRF_6_", "MOD_SFCRF_7_"
          ),
          mark = TRUE
        )
      },
      pattern = cross(chr_list_calc_mod09_files, list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD09GA features | prediction"
    )
    ,    # targets::tar_target(
    #   chr_args_calc_mod09_files,
    #   command = {
    #     download_mod09
    #     list.files(
    #       file.path(chr_input_dir, "modis", "raw", "61", "MOD09GA"),
    #       full.names = TRUE,
    #       recursive = TRUE
    #     )
    #   },
    #   description = "MODIS - MOD09 files"
    # )
    # ,
    # targets::tar_target(
    #   list_args_calc_mod09,
    #   command = list(
    #     from = grep(
    #       x = chr_args_calc_mod09_files,
    #       pattern = paste0(
    #         "MOD09GA.A", unlist(list_dates_julian), collapse = "|"
    #       ),
    #       value = TRUE
    #     ),
    #     name_covariates = c(
    #       "MOD_SFCRF_1_", "MOD_SFCRF_2_", "MOD_SFCRF_3_", "MOD_SFCRF_4_",
    #       "MOD_SFCRF_5_", "MOD_SFCRF_6_", "MOD_SFCRF_7_"
    #     ),
    #     subdataset = "^sur_refl_",
    #     radius = chr_iter_radii
    #   ),
    #   pattern = map(list_dates_julian),
    #   iteration = "list",
    #   description = "MODIS - MOD09 arguments"
    # )
    # ,
    # targets::tar_target(
    #   list_pred_calc_mod09,
    #   command = beethoven::inject_modis(
    #     locs = list_pred_calc_grid,
    #     injection = list_args_calc_mod09
    #   ),
    #   pattern = cross(list_pred_calc_grid, list_args_calc_mod09),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_25")
    #   ),
    #   description = "Calculate MODIS - MOD09 features | fit"
    # )
    # ,
    ###########################       MODIS - VIIRS       ######################
    targets::tar_target(
      list_pred_calc_viirs,
      command = {
        calculate_modis_direct(
          file = chr_list_calc_viirs_files,
          site = list_pred_calc_grid,
          site_id = arglist_common[["char_siteid"]],
          radius = chr_iter_radii,
          colheader = "MOD_LGHTN_0_",
          mark = TRUE
        )
      },
      pattern = cross(chr_list_calc_viirs_files, list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - VIIRS features | prediction"
    )
    ,
    # targets::tar_target(
    #   chr_args_calc_viirs_files,
    #   command = {
    #     download_viirs
    #     list.files(
    #       file.path(chr_input_dir, "modis", "raw", "5000", "VNP46A2"),
    #       full.names = TRUE,
    #       recursive = TRUE
    #     )
    #   },
    #   description = "MODIS - VIIRS files"
    # )
    # ,
    # targets::tar_target(
    #   list_args_calc_viirs,
    #   command = list(
    #     from = grep(
    #       x = chr_args_calc_viirs_files,
    #       pattern = paste0(
    #         "VNP46A2.A", unlist(list_dates_julian), collapse = "|"
    #       ),
    #       value = TRUE
    #     ),
    #     name_covariates = "MOD_LGHTN_0_",
    #     subdataset = 3,
    #     preprocess = amadeus::process_blackmarble,
    #     radius = chr_iter_radii
    #   ),
    #   pattern = map(list_dates_julian),
    #   iteration = "list",
    #   description = "MODIS - VIIRS arguments"
    # )
    # ,
    # targets::tar_target(
    #   list_pred_calc_viirs,
    #   command = beethoven::inject_modis(
    #     locs = list_pred_calc_grid,
    #     injection = list_args_calc_viirs
    #   ),
    #   pattern = cross(list_pred_calc_grid, list_args_calc_viirs),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_100")
    #   ),
    #   description = "Calculate MODIS - VIIRS features | fit"
    # )
    # ,
    ###########################        MODIS/VIIRS        ######################
    # targets::tar_target(
    #   dt_feat_calc_nasa,
    #   command = beethoven::reduce_merge(
    #     lapply(
    #       list(
    #         list_pred_calc_mod11,
    #         list_pred_calc_mod06,
    #         list_pred_calc_mod13,
    #         list_pred_calc_mcd19_1km,
    #         list_pred_calc_mcd19_5km,
    #         list_pred_calc_mod09,
    #         list_pred_calc_viirs
    #       ),
    #       function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
    #     ),
    #     by = NULL
    #   ),
    #   description = "data.table of MODIS/VIIRS features | fit"
    # )
    # ,
    ###########################        KOPPEN        ###########################
    # should be revised
    targets::tar_target(
      dt_pred_calc_koppen,
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
            locs = dplyr::bind_rows(list_pred_calc_grid),
            # NOTE: locs are all AQS sites for computational efficiency
            locs_id = "site_id",
            geom = FALSE
          )
        )
      },
      description = "data.table of Koppen Geiger features | fit"
    )
    ,
    ###########################      POPULATION      ###########################
    targets::tar_target(
      list_pred_calc_pop,
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
          locs = list_pred_calc_grid,
          locs_id = "site_id",
          geom = FALSE,
          radius = chr_iter_radii
        )
      },
      pattern = cross(list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      description = "Calculate population features | fit"
    )
    ,
    # targets::tar_target(
    #   dt_feat_calc_pop,
    #   command = beethoven::reduce_merge(
    #     beethoven::reduce_list(list_pred_calc_pop)
    #   ),
    #   description = "data.table of population features | fit"
    # )
    # ,
    ###########################         TRI          ###########################
    # should be revised
    # targets::tar_target(
    #   df_feat_calc_tri_params,
    #   command = expand.grid(
    #     year = chr_years,
    #     radius = chr_iter_radii
    #   ) %>%
    #     split(seq_len(nrow(.))),
    #   iteration = "list",
    #   description = "TRI features"
    # )
    # ,
    targets::tar_target(
      list_pred_calc_tri,
      command = {
        download_tri
        beethoven::inject_calculate(
          covariate = "tri",
          locs = dplyr::bind_rows(list_pred_calc_grid),
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
      description = "Calculate TRI features | prediction"
    )
    ,
    # targets::tar_target(
    #   dt_feat_calc_tri,
    #   command = beethoven::reduce_merge(
    #     lapply(
    #       list_pred_calc_tri,
    #       function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
    #     ),
    #     by = NULL
    #   ),
    #   description = "data.table of TRI features | fit"
    # )
    # ,
    ###########################         NEI          ###########################
    targets::tar_target(
      list_pred_calc_nei,
      command = {
        download_nei
        beethoven::inject_calculate(
          covariate = "nei",
          locs = list_pred_calc_grid_drop,
          injection = list(
            domain = chr_iter_calc_nei,
            domain_name = "year",
            path = file.path(chr_input_dir, "nei", "data_files"),
            covariate = "nei"
          )
        )
      },
      iteration = "list",
      pattern = cross(list_pred_calc_grid_drop, chr_iter_calc_nei),
      description = "Calculate NEI features | fit"
    )
    ,
    # targets::tar_target(
    #   dt_feat_calc_nei,
    #   command = beethoven::reduce_list(
    #     lapply(
    #       list_pred_calc_nei,
    #       function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
    #     )
    #   )[[1]],
    #   description = "data.table of NEI features | fit"
    # )
    # ,
    ###########################      ECOREGIONS      ###########################
    # should be revised
    targets::tar_target(
      dt_pred_calc_ecoregions,
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
            locs = dplyr::bind_rows(list_pred_calc_grid_drop),
            # NOTE: locs are all AQS sites for computational efficiency
            locs_id = "site_id"
          )
        )
      },
      description = "data.table of Ecoregions features | fit"
    )
    ,
    ###########################        GROADS        ###########################
    # should be revised
    targets::tar_target(
      list_pred_calc_groads,
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
          locs = dplyr::bind_rows(list_pred_calc_grid_drop),
          # NOTE: locs are all AQS sites for computational efficiency
          locs_id = "site_id",
          radius = chr_iter_radii
        )
      },
      iteration = "list",
      pattern = map(chr_iter_radii),
      description = "Calculate gRoads features | fit"
    )
    #,
    # targets::tar_target(
    #   dt_feat_calc_groads,
    #   command = beethoven::reduce_merge(
    #     beethoven::reduce_list(list_pred_calc_groads),
    #     by = c("site_id", "description")
    #   ),
    #   description = "data.table of gRoads features | fit"
    # )
    # ,
  )
