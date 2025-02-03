################################################################################
##### Calculate covariates at US EPA AQS sites
target_calculate_fit <-
  list(
    # targets::tar_target(
    #   sf_grid_split,
    #   command = chopin::par_pad_grid(sf_feat_proc_aqs_sites, mode = "grid", nx = 3L, ny = 2L, padding = 30000)[[1]] |> sf::st_as_sf(),
    #   iteration = "vector",
    #   description = "unpadded grid for split computation"
    # )
    # ,
    ###########################         GEOS         ###########################
    targets::tar_target(
      list_feat_calc_geos_aqc,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[1]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = list_feat_proc_aqs_sites[[1]],
          locs_id = "site_id"
        )
      },
      pattern = cross(list_feat_proc_aqs_sites, list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | aqc | fit"
    )
    ,
    targets::tar_target(
      list_feat_calc_geos_chm,
      command = {
        download_geos_buffer
        beethoven::calc_geos_strict(
          path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[2]),
          date = beethoven::fl_dates(unlist(list_dates)),
          locs = list_feat_proc_aqs_sites[[1]],
          locs_id = "site_id"
        )
      },
      pattern = cross(list_feat_proc_aqs_sites, list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      iteration = "list",
      description = "Calculate GEOS-CF features | chm | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_geos,
      command = beethoven::reduce_merge(
        c(
          beethoven::reduce_list(list_feat_calc_geos_aqc),
          beethoven::reduce_list(list_feat_calc_geos_chm)
        ),
        by = c("site_id", "time", "CO", "NO2", "SO2")
      ),
      description = "data.table of GEOS-CF features | fit"
    )
    ,
    ###########################         NARR         ###########################
    targets::tar_target(
      list_feat_calc_narr,
      command = {
        download_narr_buffer
        dt_iter_calc_narr <- amadeus::calculate_narr(
          from = amadeus::process_narr(
            path = file.path(chr_input_dir, "narr", chr_iter_calc_narr),
            variable = chr_iter_calc_narr,
            date = beethoven::fl_dates(unlist(list_dates))
          ),
          locs = list_feat_proc_aqs_sites[[1]],
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
      pattern = cross(list_feat_proc_aqs_sites, list_dates, chr_iter_calc_narr),
      iteration = "list",
      description = "Calculate NARR features | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_narr,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_narr),
        by = c("site_id", "time")
      ),
      description = "data.table of NARR features | fit"
    )
    ,
    ###########################         HMS          ###########################
    targets::tar_target(
      list_feat_calc_hms,
      command = {
        download_hms_buffer
        beethoven::inject_calculate(
          covariate = "hms",
          locs = list_feat_proc_aqs_sites[[1]],
          injection = list(
            path = file.path(chr_input_dir, "hms", "data_files"),
            date = beethoven::fl_dates(unlist(list_dates)),
            covariate = "hms"
          )
        )[[1]] |>
          dplyr::select(-dplyr::any_of(c("lon", "lat", "geometry", "hms_year")))
      },
      pattern = cross(list_feat_proc_aqs_sites, list_dates),
      iteration = "list",
      description = "Calculate HMS features | fit"     
    )
    ,
    targets::tar_target(
      dt_feat_calc_hms,
      command = beethoven::reduce_list(list_feat_calc_hms)[[1]],
      description = "data.table of HMS features | fit"
    )
    ,
    ###########################       MODIS - MOD11       ######################
    targets::tar_target(
      chr_args_calc_mod11_files,
      command = {
        download_mod11
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MOD11A1"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - MOD11 files"
    )
    ,
    targets::tar_target(
      list_args_calc_mod11,
      command = list(
        from = grep(
          x = chr_args_calc_mod11_files,
          pattern = paste0(
            "MOD11A1.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
        subdataset = "^LST_",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MOD11 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod11,
      command = beethoven::inject_modis(
        locs = list_feat_proc_aqs_sites[[1]],
        injection = list_args_calc_mod11
      ),
      pattern = cross(list_feat_proc_aqs_sites, list_args_calc_mod11),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD11 features | fit"
    )
    ,
    ###########################       MODIS - MOD06       ######################
    targets::tar_target(
      # what if the user downloaded all files but want to debug the pipeline with limited scope of file paths?
      chr_args_calc_mod06_files,
      command = {
        #download_mod06
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MOD06_L2"),
          pattern = "*.*A20203([3][5-9]|[4-6][0-9])*.*hdf",
          full.names = TRUE,
          recursive = TRUE
        )
      },
      iteration = "list",
      description = "MODIS - MOD06 files"
    )
    ,
    targets::tar_target(
      list_args_calc_mod06_files,
      command = {
        chr_a7s <- stringi::stri_extract_first_regex(
          chr_args_calc_mod06_files,
          pattern = "A2[0-9]{6,6}"
        )
        # Split the file paths
        split(chr_args_calc_mod06_files, chr_a7s)
      },
      iteration = "list",
      description = "MODIS - MOD06 file paths by day"
    )
    ,
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
    #   list_feat_calc_mod06,
    #   command = beethoven::inject_modis(
    #     locs = list_feat_proc_aqs_sites[[1]],
    #     injection = list_args_calc_mod06
    #   ),
    #   pattern = cross(list_feat_proc_aqs_sites, list_args_calc_mod06),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(controller = "controller_50")
    #   ),
    #   description = "Calculate MODIS - MOD06 features | fit"
    # )
    # ,
    # modified
    targets::tar_target(
      chr_list_calc_mod06_files,
      command = {
        export_tif(
          path_in = list_args_calc_mod06_files,
          product_code = "MOD06_L2",
          pat = "A\\d{7,7}",
          subdataset = "^Cloud_Fraction",
          dest = file.path(chr_input_dir, "modis_preprocessed", "MOD06_L2")
        )
      },
      iteration = "list",
      pattern = map(list_args_calc_mod06_files),
      description = "preprocessed MODIS MOD06 files"
    )
    ,
    # TODO: column names, shrink by radii
    targets::tar_target(
      list_feat_calc_mod06,
      command = chopin::extract_at(chr_list_calc_mod06_files, list_feat_proc_aqs_sites[[1]], radius = chr_iter_radii),
      pattern = cross(chr_list_calc_mod06_files, list_feat_proc_aqs_sites, chr_iter_radii),
      iteration = "vector",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_50")
      ),
      description = "Calculate MODIS - MOD06 features | fit -- redo"
    )
    ,
    ###########################       MODIS - MOD13       ######################
    targets::tar_target(
      chr_args_calc_mod13_files,
      command = {
        download_mod13
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MOD13A2"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - MOD13 files"
    )
    ,
    targets::tar_target(
      list_args_calc_mod13,
      command = list(
        from = grep(
          x = chr_args_calc_mod13_files,
          pattern = paste0(
            "MOD13A2.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = "MOD_NDVIV_0_",
        subdataset = "(NDVI)",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MOD13 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod13,
      command = beethoven::inject_modis(
        locs = list_feat_proc_aqs_sites[[1]],
        injection = list_args_calc_mod13
      ),
      pattern = cross(list_feat_proc_aqs_sites, list_args_calc_mod13),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "Calculate MODIS - MOD13 features | fit"
    )
    ,
    ###########################     MODIS - MCD19_1km     ######################
    targets::tar_target(
      chr_args_calc_mcd19_files,
      command = {
        download_mcd19
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MCD19A2"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - MCD19_*km files"
    )
    ,
    targets::tar_target(
      list_args_calc_mcd19_1km,
      command = list(
        from = grep(
          x = chr_args_calc_mcd19_files,
          pattern = paste0(
            "MCD19A2.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
        subdataset = "^Optical_Depth",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MCD19_1km arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mcd19_1km,
      command = beethoven::inject_modis(
        locs = list_feat_proc_aqs_sites[[1]],
        injection = list_args_calc_mcd19_1km
      ),
      pattern = cross(list_feat_proc_aqs_sites, list_args_calc_mcd19_1km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_250")
      ),
      description = "Calculate MODIS - MCD19_1km features | fit"
    )
    ,
    ###########################     MODIS - MCD19_5km     ######################
    targets::tar_target(
      list_args_calc_mcd19_5km,
      command = list(
        from = grep(
          x = chr_args_calc_mcd19_files,
          pattern = paste0(
            "MCD19A2.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = c(
          "MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
          "MOD_SCTAN_0_", "MOD_GLNAN_0_"
        ),
        subdataset = "cos|RelAZ|Angle",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MCD19_5km arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mcd19_5km,
      command = beethoven::inject_modis(
        locs = list_feat_proc_aqs_sites[[1]],
        injection = list_args_calc_mcd19_5km
      ),
      pattern = cross(list_feat_proc_aqs_sites, list_args_calc_mcd19_5km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_250")
      ),
      description = "Calculate MODIS - MCD19_5km features | fit"
    )
    ,
    ###########################       MODIS - MOD09       ######################
    targets::tar_target(
      chr_args_calc_mod09_files,
      command = {
        download_mod09
        list.files(
          file.path(chr_input_dir, "modis", "raw", "61", "MOD09GA"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - MOD09 files"
    )
    ,
    targets::tar_target(
      list_args_calc_mod09,
      command = list(
        from = grep(
          x = chr_args_calc_mod09_files,
          pattern = paste0(
            "MOD09GA.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = c(
          "MOD_SFCRF_1_", "MOD_SFCRF_2_", "MOD_SFCRF_3_", "MOD_SFCRF_4_",
          "MOD_SFCRF_5_", "MOD_SFCRF_6_", "MOD_SFCRF_7_"
        ),
        subdataset = "^sur_refl_",
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - MOD09 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod09,
      command = beethoven::inject_modis(
        locs = list_feat_proc_aqs_sites[[1]],
        injection = list_args_calc_mod09
      ),
      pattern = cross(list_feat_proc_aqs_sites, list_args_calc_mod09),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_25")
      ),
      description = "Calculate MODIS - MOD09 features | fit"
    )
    ,
    ###########################       MODIS - VIIRS       ######################
    targets::tar_target(
      chr_args_calc_viirs_files,
      command = {
        download_viirs
        list.files(
          file.path(chr_input_dir, "modis", "raw", "5000", "VNP46A2"),
          full.names = TRUE,
          recursive = TRUE
        )
      },
      description = "MODIS - VIIRS files"
    )
    ,
    targets::tar_target(
      list_args_calc_viirs,
      command = list(
        from = grep(
          x = chr_args_calc_viirs_files,
          pattern = paste0(
            "VNP46A2.A", unlist(list_dates_julian), collapse = "|"
          ),
          value = TRUE
        ),
        name_covariates = "MOD_LGHTN_0_",
        subdataset = 3,
        preprocess = amadeus::process_blackmarble,
        radius = chr_iter_radii
      ),
      pattern = map(list_dates_julian),
      iteration = "list",
      description = "MODIS - VIIRS arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_viirs,
      command = beethoven::inject_modis(
        locs = list_feat_proc_aqs_sites[[1]],
        injection = list_args_calc_viirs
      ),
      pattern = cross(list_feat_proc_aqs_sites, list_args_calc_viirs),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_100")
      ),
      description = "Calculate MODIS - VIIRS features | fit"
    )
    ,
    ###########################        MODIS/VIIRS        ######################
    targets::tar_target(
      dt_feat_calc_nasa,
      command = beethoven::reduce_merge(
        lapply(
          list(
            list_feat_calc_mod11,
            list_feat_calc_mod06,
            list_feat_calc_mod13,
            list_feat_calc_mcd19_1km,
            list_feat_calc_mcd19_5km,
            list_feat_calc_mod09,
            list_feat_calc_viirs
          ),
          function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
        ),
        by = NULL
      ),
      description = "data.table of MODIS/VIIRS features | fit"
    )
    ,
    ###########################         GMTED        ###########################
    targets::tar_target(
      chr_iter_calc_gmted_radii,
      command = c(0, 1e3, 1e4),
      description = "GMTED radii"
    )
    ,
    targets::tar_target(
      list_feat_calc_gmted,
      command = {
        download_gmted
        beethoven::calc_gmted_direct(
          variable = c(chr_iter_calc_gmted_vars, "7.5 arc-seconds"),
          path = file.path(chr_input_dir, "gmted", "data_files"),
          locs = list_feat_proc_aqs_sites[[1]],
          locs_id = "site_id",
          radius = chr_iter_calc_gmted_radii
        )
      },
      iteration = "list",
      pattern = cross(
        list_feat_proc_aqs_sites,
        chr_iter_calc_gmted_vars,
        chr_iter_calc_gmted_radii
      ),
      description = "Calculate GMTED features | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_gmted,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_gmted), by = "site_id"
      ),
      description = "data.table of GMTED features | fit"
    )
    ,
    ###########################         NLCD         ###########################
    targets::tar_target(
      df_feat_calc_nlcd_params,
      command = expand.grid(
        year = chr_iter_calc_nlcd,
        radius = chr_iter_radii
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "NLCD features"
    )
    ,
    targets::tar_target(
      list_feat_calc_nlcd,
      command = {
        download_nlcd
        beethoven::inject_nlcd(
          locs = dplyr::bind_rows(list_feat_proc_aqs_sites),
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
      description = "Calculate NLCD features | fit"
    )
    ,
    targets::tar_target(
      name = dt_feat_calc_nlcd,
      command = list_feat_calc_nlcd %>%
        collapse::rowbind(fill = TRUE) %>%
        collapse::funique() %>%
        collapse::pivot(
          ids = c("site_id", "time"),
          values = names(.)[!names(.) %in% c(
            "site_id",
            "time"
          )]
        ) %>%
        .[!is.na(.[["value"]]),] %>%
        collapse::pivot(
          ids = c("site_id", "time"),
          values = c("value"),
          how = "wider"
        ),
      description = "NLCD feature list (all dt) | fit"
    )
    ,
    ###########################        KOPPEN        ###########################
    targets::tar_target(
      dt_feat_calc_koppen,
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
            locs = dplyr::bind_rows(list_feat_proc_aqs_sites),
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
      list_feat_calc_pop,
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
          locs = list_feat_proc_aqs_sites[[1]],
          locs_id = "site_id",
          geom = FALSE,
          radius = chr_iter_radii
        )
      },
      pattern = cross(list_feat_proc_aqs_sites, chr_iter_radii),
      iteration = "list",
      description = "Calculate population features | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_pop,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_pop)
      ),
      description = "data.table of population features | fit"
    )
    ,
    ###########################         TRI          ###########################
    targets::tar_target(
      df_feat_calc_tri_params,
      command = expand.grid(
        year = chr_years,
        radius = chr_iter_radii
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "TRI features"
    )
    ,
    targets::tar_target(
      list_feat_calc_tri,
      command = {
        download_tri
        beethoven::inject_calculate(
          covariate = "tri",
          locs = dplyr::bind_rows(list_feat_proc_aqs_sites),
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
      description = "Calculate TRI features | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_tri,
      command = beethoven::reduce_merge(
        lapply(
          list_feat_calc_tri,
          function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
        ),
        by = NULL
      ),
      description = "data.table of TRI features | fit"
    )
    ,
    ###########################         NEI          ###########################
    targets::tar_target(
      list_feat_calc_nei,
      command = {
        download_nei
        beethoven::inject_calculate(
          covariate = "nei",
          locs = list_feat_proc_aqs_sites[[1]],
          injection = list(
            domain = chr_iter_calc_nei,
            domain_name = "year",
            path = file.path(chr_input_dir, "nei", "data_files"),
            covariate = "nei"
          )
        )
      },
      iteration = "list",
      pattern = cross(list_feat_proc_aqs_sites, chr_iter_calc_nei),
      description = "Calculate NEI features | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_nei,
      command = beethoven::reduce_list(
        lapply(
          list_feat_calc_nei,
          function(x) data.table::data.table(beethoven::reduce_list(x)[[1]])
        )
      )[[1]],
      description = "data.table of NEI features | fit"
    )
    ,
    ###########################      ECOREGIONS      ###########################
    targets::tar_target(
      dt_feat_calc_ecoregions,
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
            locs = dplyr::bind_rows(list_feat_proc_aqs_sites),
            # NOTE: locs are all AQS sites for computational efficiency
            locs_id = "site_id"
          )
        )
      },
      description = "data.table of Ecoregions features | fit"
    )
    ,
    ###########################        GROADS        ###########################
    targets::tar_target(
      list_feat_calc_groads,
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
          locs = dplyr::bind_rows(list_feat_proc_aqs_sites),
          # NOTE: locs are all AQS sites for computational efficiency
          locs_id = "site_id",
          radius = chr_iter_radii
        )
      },
      iteration = "list",
      pattern = map(chr_iter_radii),
      description = "Calculate gRoads features | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_groads,
      command = beethoven::reduce_merge(
        beethoven::reduce_list(list_feat_calc_groads),
        by = c("site_id", "description")
      ),
      description = "data.table of gRoads features | fit"
    )
    ,
    ########################       DATE FEATURES       #########################
    targets::tar_target(
      dt_feat_calc_date,
      command = Reduce(
        beethoven::post_calc_autojoin,
        list(
          dt_feat_calc_geos,
          dt_feat_calc_narr,
          dt_feat_calc_nasa
        )
      ),
      description = "data.table of all features | fit"
    )
    ,
    ########################       BASE FEATURES       #########################
    targets::tar_target(
      list_feat_calc_base_flat,
      command = lapply(
        list(
          list(dt_feat_calc_hms),
          list(dt_feat_calc_tri),
          list(dt_feat_calc_nei),
          list(dt_feat_calc_ecoregions),
          list(dt_feat_calc_koppen),
          list(dt_feat_calc_pop),
          list(dt_feat_calc_groads)
        ),
        function(x) {
          if (length(x) == 1) {
            x[[1]]
          } else if (
            sum(grepl("light|medium|heavy", sapply(x, \(t) names(t)))) == 3
          ) {
            xr <- lapply(x, \(dt) {
              dta <- data.table::copy(dt)
              dta <- dta[, time := as.character(time)]
              return(dta)
            })
            xrr <- Reduce(
              function(x, y) {
                collapse::join(x, y, on = c("site_id", "time"), how = "full")
              },
              xr
            )
            return(xrr)
          } else {
            collapse::rowbind(x, use.names = TRUE, fill = TRUE)
          }
        }
      ),
      description = "Calculated base feature list (all dt) | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_base,
      command = Reduce(
        beethoven::post_calc_autojoin,
        c(
          list(dt_feat_proc_aqs_sites_time),
          list_feat_calc_base_flat,
          list(dt_feat_calc_gmted),
          list(data.table::data.table(dt_feat_calc_nlcd))
        )
      ),
      description = "Base features with PM2.5 | fit"
    )
    ,
    #######################     CUMULATIVE FEATURES      #######################
    targets::tar_target(
      dt_feat_calc_design,
      command = beethoven::post_calc_autojoin(
        dt_feat_calc_base,
        dt_feat_calc_date,
        year_start = as.integer(substr(chr_daterange[1], 1, 4)),
        year_end = as.integer(substr(chr_daterange[2], 1, 4))
      ),
      description = "data.table of all features with PM2.5 | fit"
    )
    ,
    targets::tar_target(
      dt_feat_calc_imputed,
      command = beethoven::impute_all(
        dt_feat_calc_design,
        period = chr_daterange,
        nthreads_dt = 32,
        nthreads_collapse = 32,
        nthreads_imputation = 32
      ),
      description = "Imputed features + lags | fit"
    )
    ,
    targets::tar_target(
      name = dt_feat_calc_xyt,
      command = data.table::data.table(
        beethoven::attach_xy(
          dt_feat_calc_imputed, dplyr::bind_rows(list_feat_proc_aqs_sites)
        )
      ),
      description = "Imputed features + AQS sites (outcome and lat/lon) | fit"
    )
  )
