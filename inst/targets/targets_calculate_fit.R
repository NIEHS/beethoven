################################################################################
##### Calculate covariates at US EPA AQS sites
target_calculate_fit <-
  list(
    ###########################         GEOS         ###########################
    targets::tar_target(
      chr_iter_calc_geos,
      command = c(
        "aqc_tavg_1hr_g1440x721_v1",
        "chm_tavg_1hr_g1440x721_v1"
      ),
      iteration = "vector",
      description = "GEOS-CF features"
    )
    ,
    targets::tar_target(
      list_feat_calc_geos,
      command = {
        # download_geos
        inject_geos(
          locs = sf_feat_proc_aqs_sites,
          injection = list(
            date = fl_dates(unlist(list_dates)),
            path = file.path(
              arglist_common$char_input_dir,
              "geos",
              chr_iter_calc_geos
            ),
            nthreads = 1
          )
        )
      },
      pattern = cross(chr_iter_calc_geos, list_dates),
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
      dt_feat_calc_geos,
      command = reduce_merge(reduce_list(list_feat_calc_geos)),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "data.table of GEOS-CF features (fit)"
    )
    ,
    ###########################         NARR         ###########################
    targets::tar_target(
      name = chr_iter_calc_narr,
      command = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
                  "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
                  "pres.sfc", "shtfl", "snowc", "soilm",    
                  "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd"),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      name = chr_iter_calc_narr,
      command = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
                  "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
                  "pres.sfc", "shtfl", "snowc", "soilm",    
                  "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd"),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      list_feat_calc_narr,
      command = {
        # download_narr
        par_narr(
          domain = chr_iter_calc_narr,
          path = paste0(arglist_common$char_input_dir, "/narr/"),
          date = fl_dates(unlist(list_dates)),
          locs = sf_feat_proc_aqs_sites,
          nthreads = 1
        )
      },
      pattern = cross(list_dates, chr_iter_calc_narr),
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
    ###########################         HMS          ###########################
    targets::tar_target(
      list_feat_calc_hms,
      command = {
        # download_hms
        # download_hms
        inject_calculate(
          covariate = "hms",
          locs = sf_feat_proc_aqs_sites,
          injection = list(
            path = paste0(
              arglist_common$char_input_dir,
              "/hms/data_files/"
            ),
            date = fl_dates(unlist(list_dates)),
            covariate = "hms"
          )
        )
      },
      pattern = map(list_dates),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate HMS features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_hms,
      command = reduce_merge(
        lapply(
          list(list_feat_calc_hms),
          function(x) reduce_merge(reduce_list(lapply(x, "[[", 1)))
        ),
        by = c("site_id", "time")
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "data.table of HMS features (fit)"
    )
    ,
    ###########################       MODIS - MOD11       ######################
    targets::tar_target(
      list_args_calc_mod11,
      command = {
        # download_modis_clean # download_mod11
        list(
          from = query_modis_files(
            paste0(arglist_common$char_input_dir, "/modis/raw/61/MOD11A1/"),
            list_dates_julian,
            chr_dates
          ),
          name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
          subdataset = "^LST_",
          radius = chr_iter_radii
        )
      },
      pattern = map(chr_dates),
      iteration = "list",
      cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MOD11 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod11,
      command = inject_modis(
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
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MOD11 features (fit)"
    )
    ,
    ###########################       MODIS - MOD06       ######################
    targets::tar_target(
      list_args_calc_mod06,
      command = {
        # download_modis_clean # download_mod06
        list(
          from = query_modis_files(
            paste0(arglist_common$char_input_dir, "/modis/raw/61/MOD06_L2/"),
            list_dates_julian,
            chr_dates
          ),
          name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
          subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
          preprocess = amadeus::process_modis_swath,
          radius = chr_iter_radii
        )
      },
      pattern = map(chr_dates),
      iteration = "list",
      cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MOD06 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod06,
      command = inject_modis(
        locs = sf_feat_proc_aqs_sites,
        injection = list_args_calc_mod06
      ),
      pattern = map(list_args_calc_mod06),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MOD06 features (fit)"
    )
    ,
    ###########################       MODIS - MOD13       ######################
    targets::tar_target(
      list_args_calc_mod13,
      command = {
        # download_modis_clean # download_mod13
        list(
          from = query_modis_files(
            paste0(arglist_common$char_input_dir, "/modis/raw/61/MOD13A2/"),
            list_dates_julian,
            chr_dates
          ),
          name_covariates = "MOD_NDVIV_0_",
          subdataset = "(NDVI)",
          radius = chr_iter_radii
        )
      },
      pattern = map(chr_dates),
      iteration = "list",
      cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MOD13 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod13,
      command = inject_modis(
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
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MOD13 features (fit)"
    )
    ,
    ###########################     MODIS - MCD19_1km     ######################
    targets::tar_target(
      list_args_calc_mcd19_1km,
      command = {
        # download_modis_clean # download_mcd19
        list(
          from = query_modis_files(
            paste0(arglist_common$char_input_dir, "/modis/raw/61/MCD19A2/"),
            list_dates_julian,
            chr_dates
          ),
          name_covariates = c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
          subdataset = "^Optical_Depth",
          radius = chr_iter_radii
        )
      },
      pattern = map(chr_dates),
      iteration = "list",
      cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MCD19_1km arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mcd19_1km,
      command = inject_modis(
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
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MCD19_1km features (fit)"
    )
    ,
    ###########################     MODIS - MCD19_5km     ######################
    targets::tar_target(
      list_args_calc_mcd19_5km,
      command = {
        # download_modis_clean # download_mcd19
        list(
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
          radius = chr_iter_radii
        )
      },
      pattern = map(chr_dates),
      iteration = "list",
      cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MCD19_5km arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mcd19_5km,
      command = inject_modis(
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
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MCD19_5km features (fit)"
    )
    ,
    ###########################       MODIS - MOD09       ######################
    targets::tar_target(
      list_args_calc_mod09,
      command = {
        # download_modis_clean # download_mod09
        list(
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
          radius = chr_iter_radii
        )
      },
      pattern = map(chr_dates),
      iteration = "list",
      cue = targets::tar_cue(mode = "never"),
      description = "MODIS - MOD09 arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_mod09,
      command = inject_modis(
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
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - MOD09 features (fit)"
    )
    ,
    ###########################       MODIS - VIIRS       ######################
    targets::tar_target(
      list_args_calc_viirs,
      command = {
        # download_modis_clean # download_viirs
        list(
          from = query_modis_files(
            paste0(arglist_common$char_input_dir, "/modis/raw/5000/VNP46A2/"),
            list_dates_julian,
            chr_dates
          ),
          name_covariates = "MOD_LGHTN_0_",
          subdataset = 3,
          preprocess = amadeus::process_blackmarble,
          radius = chr_iter_radii
        )
      },
      pattern = map(chr_dates),
      iteration = "list",
      cue = targets::tar_cue(mode = "never"),
      description = "MODIS - VIIRS arguments"
    )
    ,
    targets::tar_target(
      list_feat_calc_viirs,
      command = inject_modis(
        locs = sf_feat_proc_aqs_sites,
        injection = list_args_calc_viirs
      ),
      pattern = map(list_args_calc_viirs),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      cue = targets::tar_cue(mode = "never"),
      description = "Calculate MODIS - VIIRS features (fit)"
    )
    ,
    ###########################        MODIS/VIIRS        ######################
    targets::tar_target(
      dt_feat_calc_nasa,
      command = reduce_merge(
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
    ###########################         GMTED        ###########################
    targets::tar_target(
      name = chr_iter_calc_gmted_vars,
      command = c(
          "Breakline Emphasis", "Systematic Subsample",
          "Median Statistic", "Minimum Statistic",
          "Mean Statistic", "Maximum Statistic",
          "Standard Deviation Statistic"
        ),
      iteration = "list",
      description = "GMTED variables"
    )
    ,
    targets::tar_target(
      chr_iter_calc_gmted_radii,
      command = c(0, 1e3, 1e4),
      description = "GMTED radii"
    )
    ,
    targets::tar_target(
      list_feat_calc_gmted,
      command = {
        # download_gmted
        inject_gmted(
          locs = sf_feat_proc_aqs_sites,
          variable = chr_iter_calc_gmted_vars,
          radii = chr_iter_calc_gmted_radii,
          injection = list(
            path = paste0(arglist_common$char_input_dir, "/gmted/data_files"),
            covariate = "gmted"
          )
        )
      },
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
      chr_iter_calc_nlcd,
      command = c(2019, 2021),
      description = "NLCD years"
    )
    ,
    targets::tar_target(
      list_feat_calc_nlcd,
      command = {
        # download_nlcd
        inject_nlcd(
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
        )
      },
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
    ###########################        KOPPEN        ###########################
    targets::tar_target(
      dt_feat_calc_koppen,
      command = {
        # download_koppen
        inject_calculate(
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
        )
      },
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "Calculate Koppen Geiger features (fit)"
    )
    ,
    ###########################      POPULATION      ###########################
    targets::tar_target(
      list_feat_calc_pop,
      command = {
        # download_population
        inject_calculate(
          covariate = "population",
          locs = sf_feat_proc_aqs_sites,
          injection = list(
            path = paste0(
              arglist_common$char_input_dir,
              "/population",
              "/data_files",
              "/gpw_v4_population_density_adjusted_to_",
              "2015_unwpp_country_totals_rev11_2020_30_sec.tif"
            ),
            fun = "mean",
            radius = chr_iter_radii,
            nthreads = 1,
            covariate = "population"
          )
        )
      },
      pattern = map(chr_iter_radii),
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
    ###########################         TRI          ###########################
    targets::tar_target(
      df_feat_calc_tri_params,
      command = expand.grid(year = chr_years, radius = chr_iter_radii) %>%
        split(seq_len(nrow(.))),
      iteration = "list",
      description = "TRI features"
    )
    ,
    targets::tar_target(
      list_feat_calc_tri,
      command = {
        # download_tri
        inject_calculate(
          covariate = "tri",
          locs = sf_feat_proc_aqs_sites,
          injection = list(
            domain = df_feat_calc_tri_params$year,
            domain_name = "year",
            path = paste0(arglist_common$char_input_dir, "/tri/"),
            variables = c(1, 13, 12, 14, 3 + c(20, 34, 36, 47, 48, 49)),
            radius = df_feat_calc_tri_params$radius,
            nthreads = 1,
            covariate = "tri"
          )
        )
      },
      iteration = "list",
      pattern = map(df_feat_calc_tri_params),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "Calculate TRI features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_tri,
      command = reduce_merge(
        lapply(
          list_feat_calc_tri,
          function(x) data.table::data.table(reduce_list(x)[[1]])
        ),
        c("site_id", "time")
      ),
      description = "data.table of TRI features (fit)"
    )
    ,
    ###########################         NEI          ###########################
    targets::tar_target(
      chr_iter_calc_nei,
      command = c("2017", "2020"),
      #iteration = "list",
      description = "NEI domain dummy"
    )
    ,
    targets::tar_target(
      list_feat_calc_nei,
      command = {
        # download_nei
        inject_calculate(
          covariate = "nei",
          locs = sf_feat_proc_aqs_sites,
          injection = list(
            domain = chr_iter_calc_nei,
            domain_name = "year",
            path = paste0(arglist_common$char_input_dir, "/nei/data_files"),
            covariate = "nei"
          )
        )
      },
       #iteration = "list",
       #pattern = map(chr_iter_calc_nei),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "Calculate NEI features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_nei,
      command = reduce_list(
        lapply(
          list_feat_calc_nei,
          function(x) data.table::data.table(reduce_list(x)[[1]])
        )
      )[[1]],
      description = "data.table of NEI features (fit)"
    )
    ,
    ############################################################################
    ############################################################################
    ###########################      ECOREGIONS      ###########################
    ##### Ecoregions covariates have been calculated manually due to ongoing
    ##### issues with the `process_ecoregions` and `calc_ecoregions` functions.
    ##### Covariates have been calculated at all sites from 2018 to 2024, and
    ##### are filtered to the relevant sites after import.
    ##### amadeus::download_aqs(
    #####   year = c(2018, 2024),
    #####   directory_to_save = paste0(arglist_common$char_input_dir, "/aqs"),
    #####   unzip = TRUE,
    #####   remove_zip = TRUE,
    #####   acknowledgement = TRUE,
    #####   download = TRUE
    ##### )
    ##### sf_aqs_2018_2024 <- amadeus::process_aqs(
    #####   path = list.files(
    #####     path = paste0(arglist_common$char_input_dir, "/aqs/data_files"),
    #####     full.names = TRUE,
    #####     recursive = TRUE
    #####   ),
    #####   date = c("2018-01-01", "2024-12-31"),
    #####   mode = "location",
    #####   return_format = "sf"
    ##### )
    ##### qs_feat_calc_ecoregions <- data.table::data.table(
    #####   amadeus::calc_ecoregion(
    #####     from = amadeus::process_ecoregion(
    #####       path = paste0(
    #####         paste0(
    #####           arglist_common$char_input_dir,
    #####           "ecoregions/",
    #####           "data_files/us_eco_l3_state_boundaries.shp"
    #####         )
    #####       )
    #####     ),
    #####     locs = sf_aqs_2018_2024,
    #####     locs_id = "site_id",
    #####   )
    ##### )
    ##### qs::qsave(
    #####   qs_feat_calc_ecoregions,
    #####   file = "./inst/extdata/dt_feat_calc_ecoregion.qs"
    ##### )
    targets::tar_target(
      qs_feat_calc_ecoregions,
      command = qs::qread("/mnt/inst/extdata/qs_feat_calc_ecoregions.qs"),
      description = "Import calculated ecoregion features (2018 - 2024)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_ecoregions,
      command = qs_feat_calc_ecoregions[
        qs_feat_calc_ecoregions$site_id %in% sf_feat_proc_aqs_sites$site_id,
      ],
      description = "data.table of Ecoregions features (fit)"
    )
    ############################################################################
    ############################################################################
    ############################################################################
    ,
    ###########################        GROADS        ###########################
    targets::tar_target(
      list_feat_calc_groads,
      command = {
        # download_groads
        inject_calculate(
          covariate = "groads",
          locs = sf_feat_proc_aqs_sites,
          injection = list(
            path = paste0(
              arglist_common$char_input_dir,
              "/groads/data_files",
              "/gROADS-v1-americas.gdb"
            ),
            radius = chr_iter_radii,
            nthreads = 1,
            covariate = "groads"
          )
        )
      },
      iteration = "list",
      pattern = map(chr_iter_radii),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "Calculate gRoads features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_groads,
      command = reduce_merge(
        lapply(
          list_feat_calc_groads,
          function(x) data.table::data.table(reduce_list(x)[[1]])
        ),
        by = c("site_id", "groads_year", "description")
      ),
      description = "data.table of gRoads features (fit)"
    )
    ,
    ########################       DATE FEATURES       #########################
    targets::tar_target(
      dt_feat_calc_date,
      command = Reduce(
        post_calc_autojoin,
        list(
          dt_feat_calc_geos,
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
          list(dt_feat_calc_hms),
          list(dt_feat_calc_tri),
          list(dt_feat_calc_nei),
          list(dt_feat_calc_ecoregions),
          dt_feat_calc_koppen,
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
        }),
      description = "Calculated base feature list (all dt) (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_base,
      command = Reduce(
        post_calc_autojoin,
        c(
          list(dt_feat_proc_aqs_sites_time),
          list_feat_calc_base_flat,
          list(dt_feat_calc_gmted),
          list(data.table::data.table(dt_feat_calc_nlcd))
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
    # targets::tar_target(
    #   dt_feat_calc_imputed,
    #   command = impute_all(
    #     dt_feat_calc_design,
    #     period = arglist_common$char_period,
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
