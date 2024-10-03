################################################################################
##### Calculate covariates at US EPA AQS sites
target_calculate_fit <-
  list(
    ###############################   GEOS-CF   ################################
    targets::tar_target(
      chr_iter_calc_geoscf,
      command = c(
        "aqc_tavg_1hr_g1440x721_v1",
        "chm_tavg_1hr_g1440x721_v1"
      ),
      iteration = "vector",
      description = "GEOS-CF features"
    )
    ,
    targets::tar_target(
      list_feat_calc_geoscf,
      command = inject_geos(
        locs = sf_feat_proc_aqs_sites,
        injection = list(
          date = fl_dates(list_dates[[chr_dates]]),
          path = paste0(
            arglist_common$char_input_dir,
            "/geos/",
            chr_iter_calc_geoscf
          ),
          nthreads = 1
        )
      ),
      pattern = cross(
        chr_dates,
        chr_iter_calc_geoscf
      ),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate GEOS-CF features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_geoscf,
      command = reduce_merge(reduce_list(list_feat_calc_geoscf)),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "data.table of GEOS-CF features (fit)"
    )
    ,
    ###############################    NARR     ################################
    targets::tar_target(
      chr_iter_calc_narr,
      command = c(
        "air.sfc", "albedo", "apcp"
        # "air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc", "hpbl",
        # "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr", "prate", "pres.sfc",
        # "shtfl", "shum", "snowc", "soilm", "tcdc", "ulwrf.sfc", "uwnd.10m",
        # "vis", "vwnd.10m", "weasd"
      ),
      iteration = "vector",
      description = "NARR features"
    )
    ,
    targets::tar_target(
      list_feat_calc_narr,
      command = par_narr(
        domain = chr_iter_calc_narr,
        path = paste0(arglist_common$char_input_dir, "/narr/"),
        date = fl_dates(list_dates[[chr_dates]]),
        locs = sf_feat_proc_aqs_sites,
        nthreads = 1
      ),
      pattern = cross(
        chr_dates,
        chr_iter_calc_narr
      ),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate NARR features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_narr,
      command = reduce_merge(
        lapply(
          list(list_feat_calc_narr),
          function(x) reduce_merge(reduce_list(lapply(x, "[[", 1)))
        ),
        by = c("site_id", "time")
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "data.table of NARR features (fit)"
    )
    ,
    ###############################     HMS     ################################
    # targets::tar_target(
    #   list_feat_calc_hms,
    #   command = inject_calculate(
    #     covariate = "hms",
    #     locs = sf_feat_proc_aqs_sites,
    #     injection = list(
    #       path = paste0(
    #         arglist_common$char_input_dir,
    #         "/HMS_Smoke/data_files/"
    #       ),
    #       date = fl_dates(list_dates[[chr_dates]]),
    #       covariate = "hms"
    #     )
    #   ),
    #   pattern = map(chr_dates),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(
    #       controller = "calc_controller"
    #     )
    #   ),
    #   cue = targets::tar_cue(mode = "never"),
    #   description = "Calculate HMS features (fit)"
    # )
    # ,
    # targets::tar_target(
    #   dt_feat_calc_hms,
    #   command = reduce_merge(reduce_list(list_feat_calc_hms)),
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(
    #       controller = "calc_controller"
    #     )
    #   ),
    #   description = "data.table of HMS features (fit)"
    # )
    # ,
    ############################    MODIS - MOD11     ##########################
    targets::tar_target(
      list_args_calc_mod11,
      command = list(
        from = query_modis_files(
          paste0(arglist_common$char_input_dir, "/modis/raw/61/MOD11A1/"),
          list_dates_julian,
          chr_dates
        ),
        name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
        subdataset = "^LST_",
        nthreads = 1,
        radius = c(1000, 10000, 50000)
      ),
      pattern = map(chr_dates),
      iteration = "list",
      # cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MOD11 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod11,
      command = inject_modis_par(
        locs = sf_feat_proc_aqs_sites,
        injection = list_args_calc_mod11
      ),
      pattern = map(list_args_calc_mod11),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      # cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MOD11 features (fit)"
    )
    ,
    ############################    MODIS - MOD06     ##########################
    # targets::tar_target(
    #   list_args_calc_mod06,
    #   command = list(
    #     from = query_modis_files(
    #       paste0(arglist_common$char_input_dir, "/modis/raw/61/MOD06_L2/"),
    #       list_dates_julian,
    #       chr_dates
    #     ),
    #     name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
    #     subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
    #     nthreads = 1,
    #     preprocess = amadeus::process_modis_swath,
    #     radius = c(1000, 10000, 50000)
    #   ),
    #   pattern = map(chr_dates),
    #   iteration = "list",
    #   # cue = targets::tar_cue(mode = "never"),
    #   description = "MODIS - MOD06 arguments"
    # )
    # ,
    # targets::tar_target(
    #   list_feat_calc_mod06,
    #   command = inject_modis_par(
    #     locs = sf_feat_proc_aqs_sites,
    #     injection = list_args_calc_mod06
    #   ),
    #   pattern = map(list_args_calc_mod06),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(
    #       controller = "calc_controller"
    #     )
    #   ),
    #   # cue = targets::tar_cue(mode = "never"),
    #   description = "Calculate MODIS - MOD06 features (fit)"
    # )
    # ,
    ############################    MODIS - MOD13     ##########################
    targets::tar_target(
      list_args_calc_mod13,
      command = list(
        from = query_modis_files(
          paste0(arglist_common$char_input_dir, "/modis/raw/61/MOD13A2/"),
          list_dates_julian,
          chr_dates
        ),
        name_covariates = "MOD_NDVIV_0_",
        subdataset = "(NDVI)",
        nthreads = 1,
        radius = c(1000, 10000, 50000)
      ),
      pattern = map(chr_dates),
      iteration = "list",
      # cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MOD13 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod13,
      command = inject_modis_par(
        locs = sf_feat_proc_aqs_sites,
        injection = list_args_calc_mod13
      ),
      pattern = map(list_args_calc_mod13),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      # cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MOD13 features (fit)"
    )
    ,
    ############################    MODIS - MCD19_1km     ######################
    targets::tar_target(
      list_args_calc_mcd19_1km,
      command = list(
        from = query_modis_files(
          paste0(arglist_common$char_input_dir, "/modis/raw/61/MCD19A2/"),
          list_dates_julian,
          chr_dates
        ),
        name_covariates = c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
        subdataset = "^Optical_Depth",
        nthreads = 1,
        radius = c(1000, 10000, 50000)
      ),
      pattern = map(chr_dates),
      iteration = "list",
      # cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MCD19_1km arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mcd19_1km,
      command = inject_modis_par(
        locs = sf_feat_proc_aqs_sites,
        injection = list_args_calc_mcd19_1km
      ),
      pattern = map(list_args_calc_mcd19_1km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      # cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MCD19_1km features (fit)"
    )
    ,
    ############################    MODIS - MCD19_5km     ######################
    targets::tar_target(
      list_args_calc_mcd19_5km,
      command = list(
        from = query_modis_files(
          paste0(arglist_common$char_input_dir, "/modis/raw/61/MCD19A2/"),
          list_dates_julian,
          chr_dates
        ),
        name_covariates = c(
          "MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
          "MOD_SCTAN_0_", "MOD_GLNAN_0_"
        ),
        subdataset = "cos|RelAZ|Angle",
        nthreads = 1,
        radius = c(1000, 10000, 50000)
      ),
      pattern = map(chr_dates),
      iteration = "list",
      # cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MCD19_5km arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mcd19_5km,
      command = inject_modis_par(
        locs = sf_feat_proc_aqs_sites,
        injection = list_args_calc_mcd19_5km
      ),
      pattern = map(list_args_calc_mcd19_5km),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      # cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MCD19_5km features (fit)"
    )
    ,
    ############################    MODIS - MOD09     ##########################
    targets::tar_target(
      list_args_calc_mod09,
      command = list(
        from = query_modis_files(
          paste0(arglist_common$char_input_dir, "/modis/raw/61/MOD09GA/"),
          list_dates_julian,
          chr_dates
        ),
        name_covariates = c(
          "MOD_SFCRF_1_", "MOD_SFCRF_2_", "MOD_SFCRF_3_", "MOD_SFCRF_4_",
          "MOD_SFCRF_5_", "MOD_SFCRF_6_", "MOD_SFCRF_7_"
        ),
        subdataset = "^sur_refl_",
        nthreads = 1,
        radius = c(1000, 10000, 50000)
      ),
      pattern = map(chr_dates),
      iteration = "list",
      # cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MOD09 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod09,
      command = inject_modis_par(
        locs = sf_feat_proc_aqs_sites,
        injection = list_args_calc_mod09
      ),
      pattern = map(list_args_calc_mod09),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      # cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MOD09 features (fit)"
    )
    ,
    ############################    MODIS - VIIRS     ##########################
    # targets::tar_target(
    #   list_args_calc_viirs,
    #   command = list(
    #     from = query_modis_files(
    #       paste0(arglist_common$char_input_dir, "/modis/raw/5000/VNP46A2/"),
    #       list_dates_julian,
    #       chr_dates
    #     ),
    #     name_covariates = "MOD_LGHTN_0_",
    #     subdataset = 3,
    #     preprocess = amadeus::process_blackmarble,
    #     nthreads = 1,
    #     radius = c(1000, 10000, 50000)
    #   ),
    #   pattern = map(chr_dates),
    #   iteration = "list",
    #   # cue = targets::tar_cue(mode = "never"),
    #   description = "MODIS - VIIRS arguments"
    # )
    # ,
    # targets::tar_target(
    #   list_feat_calc_viirs,
    #   command = inject_modis_par(
    #     locs = sf_feat_proc_aqs_sites,
    #     injection = list_args_calc_viirs
    #   ),
    #   pattern = map(list_args_calc_viirs),
    #   iteration = "list",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(
    #       controller = "calc_controller"
    #     )
    #   ),
    #   # cue = targets::tar_cue(mode = "never"),
    #   description = "Calculate MODIS - VIIRS features (fit)"
    # )
    # ,
    ############################     MODIS/VIIRS     ###########################
    targets::tar_target(
      dt_feat_calc_nasa,
      command = reduce_merge(
        lapply(
          list(
            list_feat_calc_mod11,
            # list_feat_calc_mod06,
            list_feat_calc_mod13,
            list_feat_calc_mcd19_1km,
            list_feat_calc_mcd19_5km,
            list_feat_calc_mod09
            # list_feat_calc_viirs
          ),
          function(x) data.table::data.table(reduce_list(x)[[1]])
        ),
        by = NULL
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "data.table of MODIS/VIIRS features (fit)"
    )
    ,
    ###############################    GMTED     ###############################
    targets::tar_target(
      chr_iter_calc_gmted_vars,
      command = c(
        "Breakline Emphasis", "Systematic Subsample",
        "Median Statistic", "Minimum Statistic",
        "Mean Statistic", "Maximum Statistic",
        "Standard Deviation Statistic"
      ),
      description = "GMTED features"
    )
    ,
    targets::tar_target(
      chr_iter_calc_gmted_radii,
      command = c(0, 1e3, 1e4, 5e4),
      description = "GMTED radii"
    )
    ,
    targets::tar_target(
      list_feat_calc_gmted,
      command = inject_gmted(
        locs = sf_feat_proc_aqs_sites,
        variable = chr_iter_calc_gmted_vars,
        radii = chr_iter_calc_gmted_radii,
        injection = list(
          path = paste0(arglist_common$char_input_dir, "/gmted/data_files"),
          covariate = "gmted"
        )
      ),
      iteration = "list",
      pattern = cross(chr_iter_calc_gmted_vars, chr_iter_calc_gmted_radii),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "Calculate GMTED features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_gmted,
      command = reduce_merge(list_feat_calc_gmted, by = "site_id"),
      description = "data.table of GMTED features (fit)"
    )
    ,
    ###############################    NLCD      ###############################
    # targets::tar_target(
    #   chr_iter_calc_nlcd_years,
    #   command = query_nlcd_years(unique(substr(chr_dates_julian, 1, 4))),
    #   description = "NLCD years"
    # )
    # ,
    targets::tar_target(
      df_feat_calc_nlcd_params,
      command = expand.grid(
        year = c(2019, 2021),
        radius = c(1000, 10000)# , 50000)
      ) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "NLCD features"
    )
    ,
    targets::tar_target(
      list_feat_calc_nlcd,
      command = inject_nlcd(
        locs = sf_feat_proc_aqs_sites,
        locs_id = arglist_common$char_siteid,
        year = df_feat_calc_nlcd_params$year,
        radius = df_feat_calc_nlcd_params$radius,
        from = amadeus::process_nlcd(
          path = paste0(arglist_common$char_input_dir, "/nlcd/data_files/"),
          year = df_feat_calc_nlcd_params$year
        ),
        nthreads = 1,
        mode = "exact",
        max_cells = 3e7
      ),
      iteration = "list",
      pattern = map(df_feat_calc_nlcd_params),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "Calculate NLCD features (fit)"
    )
    ,
    targets::tar_target(
      name = dt_feat_calc_nlcd,
      command = list_feat_calc_nlcd %>%
        collapse::rowbind(fill = TRUE) %>%
        collapse::funique() %>%
        collapse::pivot(
          ids = c(arglist_common$char_siteid, arglist_common$char_timeid),
          values = names(.)[!names(.) %in% c(
            arglist_common$char_siteid,
            arglist_common$char_timeid
          )]
        ) %>%
        .[!is.na(.[["value"]]),] %>%
        collapse::pivot(
          ids = c("site_id", "time"),
          values = c("value"),
          how = "wider"
        ),
      description = "NLCD feature list (all dt) (fit)"
    )
    ,
    ##############################    KOPPEN     ###################$###########
    targets::tar_target(
      dt_feat_calc_koppen,
      command = inject_calculate(
        covariate = "koppen",
        locs = sf_feat_proc_aqs_sites,
        injection = list(
          path = paste0(
            arglist_common$char_input_dir,
            "/koppen_geiger",
            "/data_files",
            "/Beck_KG_V1_present_0p0083.tif"
          ),
          nthreads = 1,
          covariate = "koppen"
        )
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "Calculate Koppen Geiger features (fit)"
    )
    ,
    ############################    POPULATION     #############################
    targets::tar_target(
      chr_iter_calc_pop_radii,
      command = c(1000, 10000, 50000),
      description = "Population radii"
    )
    ,
    targets::tar_target(
      list_feat_calc_pop,
      command = inject_calculate(
        covariate = "population",
        locs = sf_feat_proc_aqs_sites,
        injection = list(
          path = paste0(
            arglist_common$char_input_dir,
            "/sedac_population",
            "/data_files",
            "/gpw_v4_population_density_adjusted_to_",
            "2015_unwpp_country_totals_rev11_2020_30_sec.tif"
          ),
          fun = "mean",
          radius = chr_iter_calc_pop_radii,
          nthreads = 1,
          covariate = "population"
        )
      ),
      pattern = map(chr_iter_calc_pop_radii),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "Calculate population features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_pop,
      command = reduce_merge(
        lapply(
          list_feat_calc_pop,
          function(x) data.table::data.table(reduce_list(x)[[1]])
        ),
        c("site_id", "time", "population_year")
      ),
      description = "data.table of population features (fit)"
    )
    ,
    ############################################################################
    ##### Covariates to calculate
    # TRI
    # NEI
    # ECOREGIONS
    # GROADS
    ########################       DATE FEATURES       #########################
    targets::tar_target(
      dt_feat_calc_date,
      command = Reduce(
        post_calc_autojoin,
        list(
          dt_feat_calc_geoscf,
          dt_feat_calc_narr,
          dt_feat_calc_nasa
        )
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "data.table of all features (fit)"
    )
    ,
    ########################       BASE FEATURES       #########################
    targets::tar_target(
      list_feat_calc_base_flat,
      command = lapply(
        list(
          # dt_feat_calc_hms,
          # dt_feat_calc_tri,
          # dt_feat_calc_nei,
          # dt_feat_calc_ecoregions,
          dt_feat_calc_koppen,
          list(dt_feat_calc_pop)
          # dt_feat_calc_groads
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
        }),
      description = "Calculated base feature list (all dt) (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_base,
      command = Reduce(
        post_calc_autojoin,
        c(
          # list(dt_feat_proc_aqs_sites_time),
          list_feat_calc_base_flat,
          list(dt_feat_calc_gmted),
          list(dt_feat_calc_nlcd)
        )
      ),
      description = "Base features with PM2.5"
    )
    ,
    #######################     CUMULATIVE FEATURES      #######################
    targets::tar_target(
      dt_feat_calc_design,
      command = post_calc_autojoin(
        dt_feat_calc_base,
        dt_feat_calc_date,
        year_start = as.integer(substr(arglist_common$char_period[1], 1, 4)),
        year_end = as.integer(substr(arglist_common$char_period[2], 1, 4))
      ),
      description = "data.table of all features with PM2.5"
    )
    # ,
    # targets::tar_target(
    #   dt_feat_calc_cumulative,
    #   command = append_predecessors(
    #     path_qs = "output/qs",
    #     period_new = arglist_common$char_period,
    #     input_new = dt_feat_calc_design,
    #     nthreads = 1
    #   ),
    #   description = "Cumulative feature calculation",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(
    #       controller = "calc_controller"
    #     )
    #   )
    # ),
    # targets::tar_target(
    #   dt_feat_calc_imputed,
    #   command = impute_all(
    #     dt_feat_calc_cumulative,
    #     period = arglist_common$char_period,
    #     # nthreads_dt = arglist_common$nthreads_impute,
    #     # nthreads_collapse = arglist_common$nthreads_impute,
    #     # nthreads_imputation = arglist_common$nthreads_impute
    #     nthreads_dt = 1,
    #     nthreads_collapse = 1,
    #     nthreads_imputation = 1
    #   ),
    #   description = "Imputed features + lags",
    #   resources = targets::tar_resources(
    #     crew = targets::tar_resources_crew(
    #       controller = "calc_controller"
    #     )
    #   )
    # )
  )
