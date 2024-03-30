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
    # modis_mod11, modis_mod06, modis_mod13, modis_mcd19, modis_mod09, modis_vnp46
    targets::tar_target(
      covariates_tri,
      calculate_multi(
        # sequence: could be refered from dates
        domain =
          unique(
            as.integer(
              meta_run("tri_year_sequence_test", split = "|", fixed = TRUE)[[1]]
            )
          ),
        #c(2018, 2019, 2020, 2021, 2022),
        path = meta_run("dir_input_tri"),
        covariate = "tri",
        locs = sites_spat,
        locs_id = meta_run("pointid")
      )
    )
    ,
    targets::tar_target(
      covariates_ecoregion,
      calculate_single(
        locs = sites_spat,
        path =
          list.files(
            meta_run("dir_input_ecoregion"),
            "us_eco_l3*.*.shp$",
            full.names = TRUE,
            recursive = TRUE
          ),
        locs_id = meta_run("pointid"),
        covariate = "ecoregion"
      )
    )
    ,
    targets::tar_target(
      covariates_koppen,
      calculate_single(
        locs = sites_spat,
        path =
          file.path(
            meta_run("dir_input_koppen"), "Beck_KG_V1_present_0p083.tif"
          ),
        locs_id = meta_run("pointid"),
        covariate = "koppen"
      )
    )
    ,
    # 3 branches
    # NLCD years should be defined in the punchcard
    # revert to the original range for running the entire pipeline
    targets::tar_target(
      covariates_nlcd_list,
      command = calculate_multi(
        domain = as.integer(meta_run("nlcd_year_sequence_test")[[1]]),
        locs = terra::vect(sites_spat),
        path = meta_run("dir_input_nlcd"),
        locs_id = meta_run("pointid"),
        covariate = "nlcd",
        radius = radii
      ),
      pattern = map(radii),
      iteration = "list"
    )
    ,
    targets::tar_target(
      covariates_nlcd,
      Reduce(function(x, y) dplyr::full_join(x[, -2:-3], y[, -2:-3], by = c(meta_run("pointid"), "time")),
        covariates_nlcd_list)
    )
    ,
    targets::tar_target(
      hms_level,
      command = c("Light", "Medium", "Heavy"),
      iteration = "list"
    )
    ,
    # 3 branches
    targets::tar_target(
      covariates_hms_list,
      calculate_single(
        locs = sites_spat,
        path = meta_run("dir_input_hms"),
        covariate = "hms",
        date = time_range,
        variable = hms_level,
        locs_id = meta_run("pointid")
      ),
      iteration = "list",
      pattern = map(hms_level)
    )
    ,
    targets::tar_target(
      covariates_hms_c,
      Reduce(function(x, y) dplyr::full_join(x, y, by = c(meta_run("pointid"), "date")),
        covariates_hms_list)
    )
    ,
    targets::tar_target(
      covariates_hms,
      unify_timecols(covariates_hms_c) %>%
        dplyr::mutate(time = as.character(time))
    )
    ,
    targets::tar_target(
      covariates_sedac_population,
      calculate_multi(
        domain = meta_run("sedac_population_year_test"),
        locs = sites_spat,
        path =
          file.path(
            meta_run("dir_input_sedac_population"),
            meta_run("file_input_sedac_population")
          ),
        locs_id = meta_run("pointid"),
        covariate = "sedac_population",
        radius = 0,
        fun = "mean"
      )
    )
    ,
    # SEDAC GRoads calculation by three radii ####
    targets::tar_target(
      covariates_sedac_groads,
      command = calculate_single(
        locs = as.data.frame(sites_spat),
        path =
          file.path(
            meta_run("dir_input_sedac_groads"),
            meta_run("file_input_sedac_groads")
          ),
        locs_id = meta_run("pointid"),
        covariate = "sedac_groads",
        radius = radii
      ),
      pattern = map(radii),
      iteration = "vector"
    )
    ,
    # NARR variables calculation ####
    targets::tar_target(
      narr_variables,
      command = read.csv(meta_run("file_narr_variables"))$dirs,
      iteration = "list"
    )
    ,
    targets::tar_target(
      covariates_narr,
      calculate_single(
        locs = sites_spat,
        path = narr_variables,
        date = c(meta_run("date_start"), meta_run("date_end")),
        variable = strsplit(narr_variables, "/")[[1]][3],
        covariate = "narr",
        locs_id = meta_run("pointid")
      ),
      pattern = map(narr_variables),
      iteration = "list"
    )
    ,
    targets::tar_target(
      county_poly,
      command = load_county()
    )
    ,
    # Read NEI year range from punchcard ####
    # revert to the original for the operation
    targets::tar_target(
      nei_dirs,
      command =
        meta_run("dir_input_nei2017"),
        #c(rep(meta_run("dir_input_nei2017"), 2), rep(meta_run("dir_input_nei2020"), 3)),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      nei_years,
      command = as.integer(meta_run("nei_year_sequence_test")[[1]]),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      covariates_nei_list,
      calculate_multi(
        domain = nei_years,
        locs = sites_spat,
        path = nei_dirs,
        covariate = "nei",
        county = county_poly,
        locs_id = meta_run("pointid")
      ),
      pattern = map(nei_years, nei_dirs),
      iteration = "list"
    )
    ,
    targets::tar_target(
      covariates_nei,
      data.table::rbindlist(covariates_nei_list, fill = TRUE)
    )
    ,
    targets::tar_target(
      gmted_combination_stat,
      command =
        rep(
          c(
            "Breakline Emphasis", "Systematic Subsample",
            "Median Statistic", "Minimum Statistic",
            "Mean Statistic", "Maximum Statistic",
            "Standard Deviation Statistic"
          ),
          3L
        ),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      gmted_combination_res,
      command = rep(
        c("7.5 arc-seconds", "15 arc-seconds", "30 arc-seconds"),
        each = 7L
      ),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      covariates_gmted_list,
      calculate_single(
        locs = sites_spat,
        path = meta_run("dir_input_gmted"),
        locs_id = meta_run("pointid"),
        covariate = "gmted",
        radius = 0,
        variable = c(gmted_combination_stat, gmted_combination_res)
      ),
      pattern = map(gmted_combination_stat, gmted_combination_res),
      iteration = "list"
    )
    ,
    targets::tar_target(
      covariates_gmted,
      Reduce(function(x, y) dplyr::full_join(x, y, by = meta_run("pointid")),
        covariates_gmted_list)
    )
    ,
    targets::tar_target(
      geos_dates,
      # revert to the original range for running the entire pipeline
      command = as.character(seq(meta_run("date_start"), meta_run("date_end"), by = "1 day")),
      iteration = "list"
    )
    ,
    targets::tar_target(
      covariates_geos_aqc_list,
      calc_geos_strict(
        date = c(geos_dates, geos_dates),
        locs = sites_spat,
        locs_id = meta_run("pointid"),
        path = file.path(meta_run("dir_input_geos"), "aqc_tavg_1hr_g1440x721_v1"),
        win = as.numeric(meta_run("extent", split = "|", fixed = TRUE)[[1]]),
        snap = "out"
      ),
      pattern = map(geos_dates),
      iteration = "list"
    )
    ,
    targets::tar_target(
      covariates_geos_chm_list,
      calc_geos_strict(
        date = c(geos_dates, geos_dates),
        locs = sites_spat,
        locs_id = meta_run("pointid"),
        path = file.path(meta_run("dir_input_geos"), "chm_tavg_1hr_g1440x721_v1"),
        win = as.numeric(meta_run("extent", split = "|", fixed = TRUE)[[1]]),
        snap = "out"
      ),
      pattern = map(geos_dates),
      iteration = "list"
    )
    ,
    targets::tar_target(
      covariates_geos_aqc,
      data.table::rbindlist(covariates_geos_aqc_list, fill = TRUE) %>%
        setNames(c("site_id", paste0(names(x = .)[c(-1, -7)], "_AQC"), "time")) %>%
        dplyr::mutate(time = as.character(time))
    )
    ,
    targets::tar_target(
      covariates_geos_chm,
      data.table::rbindlist(covariates_geos_chm_list, fill = TRUE) %>%
        dplyr::mutate(time = as.character(time))
    )
    ,
    targets::tar_target(
      modis_mod06_paths,
      read_paths(
        meta_run("dir_input_modis_mod06"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          as.Date(meta_run("date_start")),
          as.Date(meta_run("date_end"))
        )
      )
    )
    ,
    targets::tar_target(
      modis_mod11_paths,
      read_paths(
        meta_run("dir_input_modis_mod11"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          as.Date(meta_run("date_start")),
          as.Date(meta_run("date_end"))
        )
      )
    )
    ,
    targets::tar_target(
      modis_mod13_paths,
      read_paths(
        meta_run("dir_input_modis_mod13"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          as.Date(meta_run("date_start")),
          as.Date(meta_run("date_end"))
        )
      )
    )
    ,
    targets::tar_target(
      modis_mod09_paths,
      read_paths(
        meta_run("dir_input_modis_mod09"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          as.Date(meta_run("date_start")),
          as.Date(meta_run("date_end"))
        )
      )
    )
    ,
    targets::tar_target(
      modis_mcd19_paths,
      read_paths(
        meta_run("dir_input_modis_mcd19"),
        extension = "hdf",
        julian = TRUE,
        target_dates = c(
          as.Date(meta_run("date_start")),
          as.Date(meta_run("date_end"))
        )
      )
    )
    ,
    targets::tar_target(
      modis_vnp46_paths,
      read_paths(
        meta_run("dir_input_modis_vnp46"),
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
      covariates_modis_mod11,
      amadeus::calc_modis_par(
        from = modis_mod11_paths,
        locs = sites_spat,
        locs_id = meta_run("pointid"),
        name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
        subdataset = "(LST_Day_|LST_Night_)",
        preprocess = amadeus::process_modis_merge,
        nthreads = meta_run("nthreads_calc")
      # calculate_multi(
      #   locs = sites_spat,
      #   path = meta_run("dir_input_modis_mod11"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = meta_run("pointid"),
      #   from = meta_run("dir_input_modis_mod11"),
      #   preprocess = amadeus::process_modis_merge,
      #   name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
      #   subdataset = "(LST_Day_|LST_Night_)",
      #   nthreads = 8
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 12
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mod06,
      amadeus::calc_modis_par(
        from = modis_mod06_paths,
        locs = sites_spat,
        locs_id = meta_run("pointid"),
        preprocess = amadeus::process_modis_swath,
        name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
        subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
        nthreads = meta_run("nthreads_calc")
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 12
      )
      # calculate_multi(
      #   locs = sites_spat,
      #   path = meta_run("dir_input_modis_mod06"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = meta_run("pointid"),
      #   from = meta_run("dir_input_modis_mod06"),
      #   preprocess = amadeus::process_modis_swath,
      #   name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
      #   subdataset = "(Cloud_Fraction_Day|Cloud_Fraction_Night)",
      #   nthreads = 8
      # )
    )
    ,
    targets::tar_target(
      covariates_modis_mod09,
      amadeus::calc_modis_par(
        from = modis_mod09_paths,
        locs = sites_spat,
        locs_id = meta_run("pointid"),
        preprocess = amadeus::process_modis_merge,
        name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
        subdataset = seq(2, 8),
        nthreads = meta_run("nthreads_calc")
      # calculate_multi(
      #   locs = sites_spat,
      #   path = meta_run("dir_input_modis_mod09"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = meta_run("pointid"),
      #   from = meta_run("dir_input_modis_mod09"),
      #   preprocess = amadeus::process_modis_merge,
      #   name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
      #   subdataset = seq(2, 8),
      #   nthreads = 8
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 12
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mcd19_1km,
      amadeus::calc_modis_par(
        from = modis_mcd19_paths,
        locs = sites_spat,
        locs_id = meta_run("pointid"),
        name_covariates =
          c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
        subdataset = "(Optical_Depth)",
        preprocess = amadeus::process_modis_merge,
        nthreads = meta_run("nthreads_calc")
      # calculate_multi(
      #   locs = sites_spat,
      #   path = meta_run("dir_input_modis_mcd19"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = meta_run("pointid"),
      #   from = meta_run("dir_input_modis_mcd19"),
      #   preprocess = amadeus::process_modis_merge,
      #   name_covariates =
      #     c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
      #   subdataset = "(Optical_Depth)",
      #   nthreads = 8
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 12
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mcd19_5km,
      amadeus::calc_modis_par(
        from = modis_mcd19_paths,
        locs = sites_spat,
        locs_id = meta_run("pointid"),
        name_covariates =
          c("MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
            "MOD_SCTAN_0_", "MOD_GLNAN_0_"),
        subdataset = "(cos|RelAZ|Angle)",
        preprocess = amadeus::process_modis_merge,
        nthreads = meta_run("nthreads_calc")
      # calculate_multi(
      #   locs = sites_spat,
      #   path = meta_run("dir_input_modis_mcd19"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = meta_run("pointid"),
      #   from = meta_run("dir_input_modis_mcd19"),
      #   preprocess = amadeus::process_modis_merge,
      #   name_covariates =
      #     c("MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
      #       "MOD_SCTAN_0_", "MOD_GLNAN_0_"),
      #   subdataset = "(cos|RelAZ|Angle)",
      #   nthreads = 8
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 12
      )
    )
    ,
    targets::tar_target(
      covariates_modis_vnp46,
      amadeus::calc_modis_par(
        from = modis_vnp46_paths,
        locs = sites_spat,
        locs_id = meta_run("pointid"),
        name_covariates = "MOD_LGHTN_0_",
        subdataset = 3,
        preprocess = amadeus::process_bluemarble,
        nthreads = meta_run("nthreads_calc")
      # calculate_multi(
      #         locs = sites_spat,
      #         path = meta_run("dir_input_modis_vnp46"),
      #         process_function = NULL,
      #         calc_function = amadeus::calc_modis_par,
      #         locs_id = meta_run("pointid"),
      #         from = meta_run("dir_input_modis_vnp46"),
      #         preprocess = amadeus::process_bluemarble,
      #         name_covariates = "MOD_LGHTN_0_",
      #         subdataset = 3,
      #         nthreads = 8
      #       )
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 12
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mod13,
      amadeus::calc_modis_par(
        from = modis_mod13_paths,
        locs = sites_spat,
        locs_id = meta_run("pointid"),
        name_covariates = "MOD_NDVIV_0_",
        subdataset = "(NDVI)",
        preprocess = amadeus::process_modis_merge,
        nthreads = meta_run("nthreads_calc")
      ),
      resources = set_slurm_resource(
        ntasks = 1, ncpus = 20, memory = 12
      )
    )
    ,
    # combine each covariate set into one data.frame (data.table; if any)
    targets::tar_target(
      covariates_combined_sp,
      combine(
        by = meta_run("pointid"),
        time = FALSE,
        covariates_koppen,
        covariates_ecoregion,
        covariates_gmted,
        covariates_nei[, -2:-3]
      )
    )
    ,
    targets::tar_target(
      covariates_combined_spt_base,
      combine(
        by = meta_run("pointid"),
        time = TRUE,
      #   # covariates_nlcd,
        covariates_hms,
        covariates_geos_aqc,
        covariates_geos_chm,
        # covariates_tri,
        covariates_modis_mod11,
        covariates_modis_mod06,
        covariates_modis_mod13,
        covariates_modis_mod09,
        covariates_modis_mcd19_1km,
        covariates_modis_mcd19_5km,
        covariates_modis_vnp46
      )
    )
    ,
    targets::tar_target(
      covariates_combined_spt_nlcd,
      join_yeardate(
        covariates_nlcd,
        covariates_combined_spt_base,
        field_year = "time",
        field_date = "time",
        spid = meta_run("pointid")
      )
    )
    ,
    targets::tar_target(
      covariates_combined_spt,
      join_yeardate(
        covariates_tri,
        covariates_combined_spt_nlcd,
        field_year = "time",
        field_date = "time",
        spid = meta_run("pointid")
      )
    )
    ,
    targets::tar_target(
      covariates_final,
      combine_final(
        locs = sites_time,
        locs_id = meta_run("pointid"),
        time_id = meta_run("timeid"),
        target_years =
          seq(
            as.integer(format(meta_run("date_start"), "%Y")),
            as.integer(format(meta_run("date_end"), "%Y"))
          ),
        df_sp = covariates_combined_sp,
        df_spt = covariates_combined_spt
      )
    )
    ,
    targets::tar_target(
      data_full,
      join_yx(
        df_pm = sites_pm,
        df_covar = covariates_final,
        locs_id = meta_run("pointid"),
        time_id = meta_run("timeid")
      )
    )
  )




## Targets for prediction grid features ####
## FIXME: align with the "_fit" targets
## TODO: chopin's gridset implementation
target_calculate_predict <-
  list(
    # ... calculate covariates for prediction grid
    targets::tar_target(
      covar_prediction_grid,
      read_covar_pred(meta_run("file_grid_prediction"))
    )
    ,
    # covariate calculation: multi vs single cases
    targets::tar_target(
      covariates_predict_tri,
      calculate_multi(
        domain = c(2018, 2019, 2020, 2021, 2022),
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_tri")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_tri"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_ecoregion,
      calculate_single(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_ecoregion")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_ecoregion"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_koppen,
      calculate_single(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_koppen")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_koppen"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_nlcd,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_nlcd")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_nlcd"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_hms,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_hms")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_hms"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_sedac_population,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_sedac_population")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_sedac_population"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_sedac_groads,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_sedac_groads")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_sedac_groads"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_narrmono,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_narrmono")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_narrmono"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_narrplevels,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_narrplevels")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_narrplevels"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_nei,
      calculate_multi(
        domain = c(2017, 2017, 2020, 2020, 2020),
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_nei")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_nei"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_gmted,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_gmted")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_gmted"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_geos,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_geos")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_geos"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mod11,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_modis_mod11")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_modis_mod11"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mod06,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_modis_mod06")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_modis_mod06"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mod13,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_modis_mod13")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_modis_mod13"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mcd19,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_modis_mcd19")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_modis_mcd19"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mod09,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_modis_mod09")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_modis_mod09"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_vnp46,
      calculate_multi(
        outpath =
          file.path(
            meta_run("dir_output"), meta_run("file_covar_predict_modis_vnp46")
          ),
        locs = covar_prediction_grid,
        path = meta_run("dir_input_modis_vnp46"),
        ... # other args
      )
    )
    ,
    # combine each covariate set into one data.frame (data.table; if any)
    targets::tar_target(
      covariates_predict_combined_sp,
      combine(
        by = meta_run("pointid"),
        time = FALSE,
        covariates_predict_koppen,
        covariates_predict_ecoregion,
        covariates_predict_gmted
      )
    )
    ,
    targets::tar_target(
      covariates_predict_combined_spt,
      combine(
        by = meta_run("pointid"),
        time = TRUE,
        #covariates_predict_nlcd,
        covariates_predict_hms,
        covariates_predict_geos,
        covariates_predict_nei,
        covariates_predict_tri,
        covariates_predict_modis_mod11,
        covariates_predict_modis_mod06,
        covariates_predict_modis_mod13,
        covariates_predict_modis_mod09,
        covariates_predict_modis_mcd19_1km,
        covariates_predict_modis_mcd19_5km,
        covariates_predict_modis_vnp46
      )
    )
    ,
    targets::tar_target(
      covariates_predict_final,
      combine_final(
        locs = covar_prediction_grid,
        locs_id = meta_run("pointid"),
        time_id = meta_run("timeid"),
        target_years = seq(2018, 2022),
        df_sp = covariates_predict_combined_sp,
        df_spt = covariates_predict_combined_spt
      )
    )
  )


