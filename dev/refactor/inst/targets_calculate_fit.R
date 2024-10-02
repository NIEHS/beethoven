################################################################################
##### Calculate covariates at US EPA AQS sites
target_calculate_fit <-
  list(
    tarchetypes::tar_files_input(
      file_prep_calc_args,
      files =
        list.files("inst/", pattern = "^calc*.*.qs$", full.names = TRUE),
      format = "file",
      iteration = "vector",
      description = "Calculation arguments in QS file"
    )
    ,
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
      resources = tar_resources(
        crew = tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      cue = tar_cue(mode = "never"),
      description = "Calculate GEOS-CF features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_geoscf,
      command = reduce_merge(reduce_list(list_feat_calc_geoscf)),
      resources = tar_resources(
        crew = tar_resources_crew(
          controller = "highmem_controller"
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
      resources = tar_resources(
        crew = tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      cue = tar_cue(mode = "never"),
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
      resources = tar_resources(
        crew = tar_resources_crew(
          controller = "highmem_controller"
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
    #   resources = tar_resources(
    #     crew = tar_resources_crew(
    #       controller = "calc_controller"
    #     )
    #   ),
    #   cue = tar_cue(mode = "never"),
    #   description = "Calculate HMS features (fit)"
    # )
    # ,
    # targets::tar_target(
    #   dt_feat_calc_hms,
    #   command = reduce_merge(reduce_list(list_feat_calc_hms)),
    #   resources = tar_resources(
    #     crew = tar_resources_crew(
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
        from = list.files(
          paste0(arglist_common$char_input_dir, "/modis/raw/61/MOD11A1/"),
          full.names = TRUE,
          recursive = TRUE
        ) |> grep(
          pattern = paste(list_dates_julian[[chr_dates]], collapse = "|"),
          value = TRUE
        ),
        name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
        subdataset = "^LST_",
        nthreads = 1,
        radius = c(1000, 10000, 50000)
      ),
      pattern = map(chr_dates),
      iteration = "list",
      cue = tar_cue(mode = "never"),
      description = "MODIS [MOD11] arguments"
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
      resources = tar_resources(
        crew = tar_resources_crew(
          controller = "nasa_controller"
        )
      ),
      cue = tar_cue(mode = "never"),
      description = "Calculate MODIS [MOD11] features (fit)"
    )
    ,
    targets::tar_target(
      dt_feat_calc_mod11,
      command = data.table::data.table(reduce_list(list_feat_calc_mod11)[[1]]),
      description = "data.table of MODIS [MOD11] features (fit)"
    )
    ,
    ############################     MODIS/VIIRS     ###########################
    targets::tar_target(
      dt_feat_calc_nasa,
      command = reduce_merge(
        list(
          dt_feat_calc_mod11
          # dt_feat_calc_mod13
        ),
        by = NULL
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
      command = c(0, 1e3), # 1e4, 5e4),
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
      resources = tar_resources(
        crew = tar_resources_crew(
          controller = "calc_controller"
        )
      ),
      description = "Calculate GMTED features (fit)"
    )
    ,
    ############################################################################
    ##### Covariates to calculate
    # NLCD
    # TRI
    # NEI
    # ECOREGIONS
    # KOPPEN
    # POPULATION
    # GROADS
    ############################################################################
    targets::tar_target(
      dt_feat_calc_date,
      command = Reduce(
        post_calc_autojoin,
        list(
          dt_feat_calc_geoscf,
          dt_feat_calc_narr,
          # dt_feat_calc_hms,
          dt_feat_calc_nasa
        )
      ),
      resources = tar_resources(
        crew = tar_resources_crew(
          controller = "highmem_controller"
        )
      ),
      description = "data.table of all features (fit)"
    )
  )
