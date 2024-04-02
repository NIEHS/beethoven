## TODO: abide by the new naming convention

# meta_run, resting in the tools/pipeline/pipeline_base_functions.R,
# is a function that returns a list of parameters for the pipeline
# for users' convenience and make the pipeline less prone to errors.

library(dplyr)
# for reference, full list of parameters in amadeus::process_*
# and amadeus::calc_*.
# This list will be useful for entering parameters by rlang::inject.
amadeusArgs <- list(
  path = NULL,
  date = NULL,
  variable = NULL,
  year = NULL,
  county = NULL,
  variables = NULL,
  from = NULL,
  locs = NULL,
  locs_id = NULL,
  radius = NULL,
  fun = NULL,
  name_extracted = NULL,
  fun_summary = NULL,
  max_cells = NULL,
  preprocess = NULL,
  name_covariates = NULL,
  subdataset = NULL,
  nthreads = NULL,
  package_list_add = NULL,
  export_list_add = NULL,
  sedc_bandwidth = NULL,
  target_fields = NULL
)

## Targets for feature calculation ####
target_calculate_fit <-
  list(
    # covariate calculation: multi vs single cases
    # single: ecoregion, koppen
    # multi: tri, nlcd, hms, sedac_population, sedac_groads,
    # narrmono, narrplevels, nei, gmted, geos,
    # modis_mod11, modis_mod06, modis_mod13,
    # modis_mcd19, modis_mod09, viirs
    targets::tar_target(
      dt_feat_calc_tri,
      calculate_multi(
        domain =
          unique(
            as.integer(
              meta_run("tri_year_sequence_test", split = "|", fixed = TRUE)[[1]]
            )
          ),
        #c(2018, 2019, 2020, 2021, 2022),
        path = meta_run("dir_input_tri"),
        covariate = "tri",
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid")
      )
    )
    ,
    targets::tar_target(
      dt_feat_calc_ecoregions,
      calculate_single(
        locs = sf_feat_proc_aqs_sites,
        path =
          list.files(
            meta_run("dir_input_ecoregions"),
            "us_eco_l3*.*.shp$",
            full.names = TRUE,
            recursive = TRUE
          ),
        locs_id = meta_run("char_siteid"),
        covariate = "ecoregion"
      )
    )
    ,
    targets::tar_target(
      dt_feat_calc_koppen,
      calculate_single(
        locs = sf_feat_proc_aqs_sites,
        path =
          file.path(
            meta_run("dir_input_koppen"), "Beck_KG_V1_present_0p083.tif"
          ),
        locs_id = meta_run("char_siteid"),
        covariate = "koppen"
      )
    )
    ,
    # 3 branches
    # NLCD years should be defined in the punchcard
    # revert to the original range for running the entire pipeline
    targets::tar_target(
      list_feat_calc_nlcd,
      command = calculate_multi(
        domain = as.integer(meta_run("nlcd_year_sequence_test")[[1]]),
        locs = terra::vect(sf_feat_proc_aqs_sites),
        path = meta_run("dir_input_nlcd"),
        locs_id = meta_run("char_siteid"),
        covariate = "nlcd",
        radius = int_feat_calc_radii
      ),
      pattern = map(int_feat_calc_radii),
      iteration = "list"
    )
    ,
    targets::tar_target(
      dt_feat_calc_nlcd,
      Reduce(function(x, y) dplyr::full_join(x[, -2:-3], y[, -2:-3], by = c(meta_run("char_siteid"), "time")),
        list_feat_calc_nlcd)
    )
    ,
    targets::tar_target(
      char_feat_proc_hms_level,
      command = c("Light", "Medium", "Heavy"),
      iteration = "list"
    )
    ,
    # 3 branches
    targets::tar_target(
      list_feat_calc_hms,
      calculate_single(
        locs = sf_feat_proc_aqs_sites,
        path = meta_run("dir_input_hms"),
        covariate = "hms",
        date = char_feat_proc_timerange,
        variable = char_feat_proc_hms_level,
        locs_id = meta_run("char_siteid")
      ),
      iteration = "list",
      pattern = map(char_feat_proc_hms_level)
    )
    ,
    targets::tar_target(
      dt_feat_calc_hms,
      Reduce(function(x, y) {
        merge(x, y, by = c(meta_run("char_siteid"), "date"), all = TRUE)
      },
      list_feat_calc_hms) |>
        post_calc_unify_timecols() |>
        post_calc_convert_time()
    )
    ,
    targets::tar_target(
      dt_feat_calc_population,
      calculate_multi(
        domain = meta_run("sedac_population_year_test"),
        locs = sf_feat_proc_aqs_sites,
        path =
          file.path(
            meta_run("dir_input_sedac_population"),
            meta_run("file_input_sedac_population")
          ),
        locs_id = meta_run("char_siteid"),
        covariate = "sedac_population",
        radius = 0,
        fun = "mean"
      )
    )
    ,
    # SEDAC GRoads calculation by three radii ####
    targets::tar_target(
      dt_feat_calc_groads,
      command = calculate_single(
        locs = as.data.frame(sf_feat_proc_aqs_sites),
        path =
          file.path(
            meta_run("dir_input_sedac_groads"),
            meta_run("file_input_sedac_groads")
          ),
        locs_id = meta_run("char_siteid"),
        covariate = "sedac_groads",
        radius = int_feat_calc_radii
      ),
      pattern = map(int_feat_calc_radii),
      iteration = "vector"
    )
    ,
    # NARR variables calculation ####
    targets::tar_target(
      char_feat_proc_narr_variables,
      command = meta_run("file_narr_variables")$dirs,
      iteration = "list"
    )
    ,
    targets::tar_target(
      dt_feat_calc_narr,
      calculate_single(
        locs = sf_feat_proc_aqs_sites,
        path = char_feat_proc_narr_variables,
        date = c(meta_run("date_start"), meta_run("date_end")),
        variable = strsplit(char_feat_proc_narr_variables, "/")[[1]][3],
        covariate = "narr",
        locs_id = meta_run("char_siteid")
      ),
      pattern = map(char_feat_proc_narr_variables),
      iteration = "list"
    )
    ,
    targets::tar_target(
      sf_feat_proc_counties_2020,
      command = process_counties(year = 2020)
    )
    ,
    # Read NEI year range from configuration file ####
    # revert to the original for the operation
    targets::tar_target(
      char_feat_proc_nei_dirs,
      command =
        meta_run("dir_input_nei2017"),
        #c(rep(meta_run("dir_input_nei2017"), 2), rep(meta_run("dir_input_nei2020"), 3)),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      int_feat_proc_nei_years,
      command = as.integer(meta_run("nei_year_sequence_test")[[1]]),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      list_feat_calc_nei,
      calculate_multi(
        domain = int_feat_proc_nei_years,
        locs = sf_feat_proc_aqs_sites,
        path = char_feat_proc_nei_dirs,
        covariate = "nei",
        county = sf_feat_proc_counties_2020,
        locs_id = meta_run("char_siteid")
      ),
      pattern = map(int_feat_proc_nei_years, char_feat_proc_nei_dirs),
      iteration = "list"
    )
    ,
    targets::tar_target(
      dt_feat_calc_nei,
      data.table::rbindlist(list_feat_calc_nei, fill = TRUE)
    )
    ,
    targets::tar_target(
      char_feat_proc_gmted_vars,
      command =
        c(
          "Breakline Emphasis", "Systematic Subsample",
          "Median Statistic", "Minimum Statistic",
          "Mean Statistic", "Maximum Statistic",
          "Standard Deviation Statistic"
        ),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      char_feat_proc_gmted_res,
      command =
        c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      list_feat_calc_gmted,
      calculate_single(
        locs = sf_feat_proc_aqs_sites,
        path = meta_run("dir_input_gmted"),
        locs_id = meta_run("char_siteid"),
        covariate = "gmted",
        radius = 0,
        variable = c(char_feat_proc_gmted_vars, char_feat_proc_gmted_res)
      ),
      pattern = cross(char_feat_proc_gmted_vars, char_feat_proc_gmted_res),
      iteration = "list"
    )
    ,
    targets::tar_target(
      dt_feat_calc_gmted,
      Reduce(function(x, y) {
        merge(x, y, by = meta_run("char_siteid"), all = TRUE)
      },
      list_feat_calc_gmted)
    )
    ,
    targets::tar_target(
      list_config_timerange,
      # revert to the original range for running the entire pipeline
      command =
      as.character(seq(meta_run("date_start"), meta_run("date_end"), by = "1 day")),
      iteration = "list"
    )
    ,
    targets::tar_target(
      list_feat_calc_geoscf_aqc,
      calc_geos_strict(
        date = c(list_config_timerange, list_config_timerange),
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid"),
        path = file.path(meta_run("dir_input_geoscf"), "aqc_tavg_1hr_g1440x721_v1"),
        win = as.numeric(meta_run("extent", split = "|", fixed = TRUE)[[1]]),
        snap = "out"
      ),
      pattern = map(list_config_timerange),
      iteration = "list"
    )
    ,
    targets::tar_target(
      list_feat_calc_geoscf_chm,
      calc_geos_strict(
        date = c(list_config_timerange, list_config_timerange),
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid"),
        path = file.path(meta_run("dir_input_geoscf"), "chm_tavg_1hr_g1440x721_v1"),
        win = as.numeric(meta_run("extent", split = "|", fixed = TRUE)[[1]]),
        snap = "out"
      ),
      pattern = map(list_config_timerange),
      iteration = "list"
    )
    ,
    targets::tar_target(
      dt_feat_calc_geoscf_aqc,
      data.table::rbindlist(list_feat_calc_geoscf_aqc, fill = TRUE) |>
        # setNames(c(meta_run("char_siteid"), paste0(names(x = _)[c(-1, -7)], "_AQC"), "time")) |>
        post_calc_convert_time()
    )
    ,
    targets::tar_target(
      dt_feat_calc_geoscf_chm,
      data.table::rbindlist(list_feat_calc_geoscf_chm, fill = TRUE) |>
        post_calc_convert_time()
    )
    ,
    targets::tar_target(
      char_filepaths_raw_modis_mod06,
      read_paths(
        meta_run("dir_input_modis_mod06"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          meta_run("date_start"),
          meta_run("date_end")
        )
      )
    )
    ,
    targets::tar_target(
      char_filepaths_raw_modis_mod11,
      read_paths(
        meta_run("dir_input_modis_mod11"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          meta_run("date_start"),
          meta_run("date_end")
        )
      )
    )
    ,
    targets::tar_target(
      char_filepaths_raw_modis_mod13,
      read_paths(
        meta_run("dir_input_modis_mod13"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          meta_run("date_start"),
          meta_run("date_end")
        )
      )
    )
    ,
    targets::tar_target(
      char_filepaths_raw_modis_mod09,
      read_paths(
        meta_run("dir_input_modis_mod09"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          meta_run("date_start"),
          meta_run("date_end")
        )
      )
    )
    ,
    targets::tar_target(
      char_filepaths_raw_modis_mcd19,
      read_paths(
        meta_run("dir_input_modis_mcd19"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          meta_run("date_start"),
          meta_run("date_end")
        )
      )
    )
    ,
    targets::tar_target(
      char_filepaths_raw_viirs,
      read_paths(
        meta_run("dir_input_viirs"),
        extension = "h5",
        julian = TRUE,
        target_dates = c(
          as.Date(meta_run("date_start")),
          as.Date(meta_run("date_end"))
        )
      )
    )
    ,
    targets::tar_target(
      dt_feat_calc_modis_mod11,
      amadeus::calc_modis_par(
        from = char_filepaths_raw_modis_mod11,
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid"),
        name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
        subdataset = "(LST_Day_|LST_Night_)",
        preprocess = amadeus::process_modis_merge,
        nthreads = meta_run("nthreads")
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 10
      )
    )
    ,
    targets::tar_target(
      dt_feat_calc_modis_mod06,
      amadeus::calc_modis_par(
        from = char_filepaths_raw_modis_mod06,
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid"),
        preprocess = amadeus::process_modis_swath,
        name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
        subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
        nthreads = meta_run("nthreads")
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 10
      )
    )
    ,
    targets::tar_target(
      dt_feat_calc_modis_mod09,
      amadeus::calc_modis_par(
        from = char_filepaths_raw_modis_mod09,
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid"),
        preprocess = amadeus::process_modis_merge,
        name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
        subdataset = seq(2, 8),
        nthreads = meta_run("nthreads")
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 10
      )
    )
    ,
    targets::tar_target(
      dt_feat_calc_modis_mcd19_1km,
      amadeus::calc_modis_par(
        from = char_filepaths_raw_modis_mcd19,
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid"),
        name_covariates =
          c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
        subdataset = "(Optical_Depth)",
        preprocess = amadeus::process_modis_merge,
        nthreads = meta_run("nthreads")
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 10
      )
    )
    ,
    targets::tar_target(
      dt_feat_calc_modis_mcd19_5km,
      amadeus::calc_modis_par(
        from = char_filepaths_raw_modis_mcd19,
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid"),
        name_covariates =
          c("MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
            "MOD_SCTAN_0_", "MOD_GLNAN_0_"),
        subdataset = "(cos|RelAZ|Angle)",
        preprocess = amadeus::process_modis_merge,
        nthreads = meta_run("nthreads")
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 10
      )
    )
    ,
    targets::tar_target(
      dt_feat_calc_viirs,
      amadeus::calc_modis_par(
        from = char_filepaths_raw_viirs,
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid"),
        name_covariates = "MOD_LGHTN_0_",
        subdataset = 3,
        preprocess = amadeus::process_bluemarble,
        nthreads = meta_run("nthreads")
      )
      ,
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 10
      )
    )
    ,
    targets::tar_target(
      dt_feat_calc_modis_mod13,
      amadeus::calc_modis_par(
        from = char_filepaths_raw_modis_mod13,
        locs = sf_feat_proc_aqs_sites,
        locs_id = meta_run("char_siteid"),
        name_covariates = "MOD_NDVIV_0_",
        subdataset = "(NDVI)",
        preprocess = amadeus::process_modis_merge,
        nthreads = meta_run("nthreads")
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 10
      )
    )
    ,
    # Merge spatial-only features ####
    targets::tar_target(
      dt_feat_fit_xsp_base,
      post_calc_merge_features(
        by = meta_run("char_siteid"),
        time = FALSE,
        dt_feat_calc_koppen,
        dt_feat_calc_ecoregions,
        dt_feat_calc_gmted,
        dt_feat_calc_nei
      )
    )
    ,
    # Merge spatiotemporal features ####
    targets::tar_target(
      dt_feat_fit_xst_base,
      post_calc_merge_features(
        by = meta_run("char_siteid"),
        time = TRUE,
        dt_feat_calc_hms,
        dt_feat_calc_geoscf_aqc,
        dt_feat_calc_geoscf_chm,
        dt_feat_calc_modis_mod06 |> data.table::as.data.table(),
        dt_feat_calc_modis_mod09 |> data.table::as.data.table(),
        dt_feat_calc_modis_mod11 |> data.table::as.data.table(),
        dt_feat_calc_modis_mod13 |> data.table::as.data.table(),
        dt_feat_calc_modis_mcd19_1km |> data.table::as.data.table(),
        dt_feat_calc_modis_mcd19_5km |> data.table::as.data.table(),
        dt_feat_calc_viirs  |> data.table::as.data.table()
      )
    )
    ,
    targets::tar_target(
      dt_feat_fit_xst_nlcd,
      post_calc_join_yeardate(
        dt_feat_calc_nlcd,
        dt_feat_fit_xst_base,
        field_year = "time",
        field_date = "time",
        spid = meta_run("char_siteid")
      )
    )
    ,
    targets::tar_target(
      dt_feat_fit_xst,
      post_calc_join_yeardate(
        dt_feat_calc_tri,
        dt_feat_fit_xst_nlcd,
        field_year = "time",
        field_date = "time",
        spid = meta_run("char_siteid")
      )
    )
    ,
    targets::tar_target(
      dt_feat_fit_x,
      post_calc_merge_all(
        locs = sf_feat_proc_aqs_sites_time,
        locs_id = meta_run("char_siteid"),
        time_id = meta_run("char_timeid"),
        target_years =
          seq(
            as.integer(format(meta_run("date_start"), "%Y")),
            as.integer(format(meta_run("date_end"), "%Y"))
          ),
        df_sp = dt_feat_fit_xsp_base,
        df_spt = dt_feat_fit_xst
      )
    )
    ,
    targets::tar_target(
      dt_feat_fit,
      post_calc_join_pm25_features(
        df_pm = sf_feat_proc_aqs_pm25,
        df_covar = dt_feat_fit_x,
        locs_id = meta_run("char_siteid"),
        time_id = meta_run("char_timeid")
      )
    )
  )




## Targets for prediction grid features ####
## FIXME: align with the "_fit" targets
## TODO: chopin's gridset implementation
target_calculate_predict <-
  list(

  )