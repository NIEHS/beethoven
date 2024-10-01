################################################################################
##### Calculate covariates at US EPA AQS sites
target_calculate_fit <-
  list(
    tarchetypes::tar_files_input(
      name = file_prep_calc_args,
      files =
        list.files("inst/", pattern = "^calc*.*.qs$", full.names = TRUE),
      format = "file",
      iteration = "vector",
      description = "Calculation arguments in QS file"
    )
    ,
    ###########################################################################
    ##### GEOS-CF covariates
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
    ###########################################################################
    ##### NARR covariates
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
      name = list_feat_calc_narr,
      command =
        par_narr(
          domain = chr_iter_calc_narr,
          path = paste0(
            arglist_common$char_input_dir,
            "/narr/"
          ),
          date = fl_dates(list_dates[[chr_dates]]),
          locs = sf_feat_proc_aqs_sites,
          nthreads = 1
        )
      ,
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
    ###########################################################################
    ##### Covariates to calculate
    # GMTED
    # MODIS
    # NLCD
    # HMS
    # TRI
    # NEI
    # ECOREGIONS
    # KOPPEN
    # POPULATION
    # GROADS
  )
