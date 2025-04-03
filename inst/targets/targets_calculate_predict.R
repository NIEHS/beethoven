
## Targets for prediction grid features ####
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
        init_grid <-
          chopin::par_pad_grid(
            sf_pred_calc_grid,
            mode = "grid",
            nx = 10L,
            ny = 5L,
            padding = 100
          )[[1]]
        grid_sample <- dplyr::sample_frac(sf_pred_raw_grid, 0.01)
        init_grid_intersect <-
          init_grid[grid_sample, ]
        init_grid_intersect
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
    ### VECTOR PROCESSING GRID ####
    targets::tar_target(
      sf_pred_calc_split_v,
      command = {
        init_grid <-
          chopin::par_pad_grid(
            sf_pred_calc_grid,
            mode = "grid",
            nx = 30L,
            ny = 15L,
            padding = 100
          )[[1]]
        grid_sample <- dplyr::sample_frac(sf_pred_raw_grid, 0.01)
        init_grid_intersect <-
          init_grid[grid_sample, ]
        init_grid_intersect
      },
      iteration = "vector",
      description = "sf split grid polygons (for vector processing)"
    )
    ,
    targets::tar_target(
      list_pred_calc_grid_v,
      command = {
        grid_unit <- sf::st_bbox(sf_pred_calc_split_v)
        sf::st_as_sf(
          df_pred_calc_gridcoords |>
            dplyr::filter((lon <= grid_unit[3] & lon >= grid_unit[1]) & (lat <= grid_unit[4] & lat >= grid_unit[2])),
          coords = c("lon", "lat"),
          crs = 4326,
          remove = FALSE
        )
      },
      iteration = "list",
      pattern = map(sf_pred_calc_split_v),
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
    # Added for test
    targets::tar_target(
      chr_iter_calc_nlcd,
      command = c(2019, 2021),
      description = "NLCD years | download"
    )
    ,
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
        SIMPLIFY = FALSE)# |>
        #beethoven::post_calc_merge_all()
      }
      ,
      pattern = cross(
        list_pred_calc_grid, df_feat_calc_nlcd_params
      ),
      iteration = "list",
      description = "Calculate NLCD features with branched sublists (pred)",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_15")
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
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | aqc | prediction"
    )
    ,
    targets::tar_target(
      list_pred_calc_geos_chm,
      command = {
        # download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[2]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = list_pred_calc_grid,
          locs_id = "site_id"
        )
      },
      pattern = cross(list_pred_calc_grid, list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
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
      },
      pattern = cross(chr_list_calc_mod13_files, list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD13 features | prediction"
    )
    ,
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
      },
      pattern = cross(chr_list_calc_mcd19_1km_files, list_pred_calc_grid, chr_iter_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MCD19_1km features | prediction"
    )
    ,
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
    ,
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
        # download_koppen
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
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
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
        # beethoven::inject_calculate(
          # covariate = "tri",
        base_tri <-
          amadeus::process_tri(
            year = df_feat_calc_tri_params$year,
            path = file.path(chr_input_dir, "tri"),
            variables = c(1, 13, 12, 14, 3 + c(20, 34, 36, 47, 48, 49))
          )
        res_tri <-
          beethoven::calc_tri_mod(
            from = base_tri,
            locs = list_pred_calc_grid_v,
            radius = df_feat_calc_tri_params$radius
          )
        res_tri
      },
      iteration = "list",
      pattern = cross(list_pred_calc_grid_v, df_feat_calc_tri_params),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_03")
      ),
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
      chr_iter_calc_nei,
      command = c(2017, 2020),
      description = "NEI features | download"
    )
    ,
    targets::tar_target(
      list_pred_calc_nei,
      command = {
        download_nei
        beethoven::inject_calculate(
          covariate = "nei",
          locs = list_pred_calc_grid_v,
          injection = list(
            domain = chr_iter_calc_nei,
            domain_name = "year",
            path = file.path(chr_input_dir, "nei", "data_files"),
            covariate = "nei"
          )
        )
      },
      iteration = "list",
      pattern = cross(list_pred_calc_grid_v, chr_iter_calc_nei),
      description = "Calculate NEI features | prediction grid"
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
        # download_ecoregions
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
            locs = list_pred_calc_grid,
            # NOTE: locs are all AQS sites for computational efficiency
            locs_id = "site_id"
          )
        )
      },
      iteration = "list",
      pattern = map(list_pred_calc_grid),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_25")
      ),
      description = "data.table of Ecoregions features | prediction grid"
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
          locs = list_pred_calc_grid_v,
          locs_id = "site_id",
          radius = chr_iter_radii
        )
      },
      iteration = "list",
      pattern = cross(list_pred_calc_grid_v, chr_iter_radii),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_15")
      ),
      description = "Calculate gRoads features | prediction grid"
    )
    ,
    targets::tar_target(
      dt_pred_calc_groads,
      command = collapse::rowbind(
        list_pred_calc_groads[
          !unlist(Map(function(x) tryCatch(is.null(x), error = function(e) return(TRUE)), list_pred_calc_groads))],
        fill = TRUE
      ),
      description = "data.table of gRoads features | prediction grid"
    )
    # ,
  )
