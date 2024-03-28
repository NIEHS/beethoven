## Some domains are manually defined

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
        domain = 2018, #c(2018, 2019, 2020, 2021, 2022),
        path = mr("dir_input_tri"),
        covariate = "tri",
        locs = sites_spat,
        locs_id = mr("pointid")
      )
    )
    ,
    targets::tar_target(
      covariates_ecoregion,
      calculate_single(
        locs = sites_spat,
        path = list.files(mr("dir_input_ecoregion"), "us_eco_l3*.*.shp$", full.names = TRUE, recursive = TRUE),
        locs_id = mr("pointid"),
        covariate = "ecoregion"
      )
    )
    ,
    targets::tar_target(
      covariates_koppen,
      calculate_single(
        locs = sites_spat,
        path = file.path(mr("dir_input_koppen"), "Beck_KG_V1_present_0p083.tif"),
        locs_id = mr("pointid"),
        covariate = "koppen"
      )
    )
    ,
    # 3 branches
    targets::tar_target(
      covariates_nlcd,
      command = calculate_multi(
        domain = 2019, #c(2019, 2019, 2019, 2021, 2021),
        locs = terra::vect(sites_spat),
        path = mr("dir_input_nlcd"),
        locs_id = mr("pointid"),
        covariate = "nlcd",
        radius = radii
      ),
      pattern = map(radii),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      hms_dates,
      command =
      as.Date(
        stringi::stri_extract_first_regex(
          list.files(
            mr("dir_input_hms"), pattern = "*.shp$", full.names = FALSE
          ),
          pattern = "2[0-1][0-9]{2}[0-1][0-9]([0-2][0-9]|3[0-1])"
        ), format = "%Y-%m-%d"
      )
    )
    ,
    targets::tar_target(
      hms_level,
      command = c("Light", "Medium", "Heavy"),
      iteration = "vector"
    )
    ,
    # 3 branches
    targets::tar_target(
      covariates_hms,
      do.call(rbind,
        purrr::map(
          .x = hms_dates,
          .f = function(d) {
            calculate_single(
              locs = sites_spat,
              path = mr("dir_input_hms"),
              date = c(d, d),
              variable = hms_level,
              locs_id = mr("pointid")
            )
          }
        )
      ),
      iteration = "vector",
      pattern = map(hms_level)
    )
    ,
    targets::tar_target(
      covariates_sedac_population,
      calculate_multi(
        domain = 2020, #rep(2020, 5)
        locs = sites_spat,
        path = file.path(mr("dir_input_sedac_population"), mr("file_input_sedac_population")),
        locs_id = mr("pointid"),
        covariate = "sedac_population",
        radius = 0,
        fun = "mean"
      )
    )
    ,
    # 3 branches
    targets::tar_target(
      covariates_sedac_groads,
      command = calculate_single(
        locs = as.data.frame(sites_spat),
        path = file.path(mr("dir_input_sedac_groads"), mr("file_input_sedac_groads")),
        locs_id = mr("pointid"),
        covariate = "sedac_groads",
        radius = radii
      ),
      pattern = map(radii),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      narr_variables,
      command = read.csv(mr("file_narr_variables"))$dirs,
      iteration = "vector"
    )
    ,
    targets::tar_target(
      covariates_narr,
      calculate_multi(
        domain = 2018, #seq(2018, 2022),
        locs = sites_spat,
        path = narr_variables,
        date = c("2020-01-01", "2020-01-01"),#c(mr("date_start"), mr("date_end")),
        variable = strsplit(narr_variables, "/")[[1]][3],
        covariate = "narr",
        locs_id = mr("pointid")
      ),
      pattern = map(narr_variables),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      county_poly,
      command = load_county()
    )
    ,
    targets::tar_target(
      nei_dirs,
      command =
        mr("dir_input_nei2017"),
        #c(rep(mr("dir_input_nei2017"), 2), rep(mr("dir_input_nei2020"), 3)),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      nei_years,
      command = c(2017), #c(2017, 2017, 2020, 2020, 2020),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      covariates_nei,
      calculate_multi(
        domain = nei_years,
        locs = sites_spat,
        path = nei_dirs,
        covariate = "nei",
        county = county_poly,
        locs_id = mr("pointid")
      ),
      pattern = map(nei_years, nei_dirs),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      gmted_combination_stat,
      command = rep(
        c(
          "Breakline Emphasis", "Systematic Subsample",
          "Median Statistic", "Minimum Statistic",
          "Mean Statistic", "Maximum Statistic",
          "Standard Deviation Statistic"
        ), 3L),
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
      covariates_gmted,
      calculate_single(
        locs = sites_spat,
        path = mr("dir_input_gmted"),
        locs_id = mr("pointid"),
        covariate = "gmted",
        radius = 0,
        variable = c(gmted_combination_stat, gmted_combination_res)
      ),
      pattern = map(gmted_combination_stat, gmted_combination_res),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      geos_dates,
      # revert to the original range for running the entire pipeline
      command = "2020-01-01",#as.character(seq(as.Date("2018-01-01"), as.Date("2022-12-31"), by = "1 day")),
      iteration = "list"
    )
    ,
    targets::tar_target(
      covariates_geos_list,
      calc_geos_strict(
        date = c(geos_dates, geos_dates),
        locs = sites_spat,
        locs_id = mr("pointid"),
        path = file.path(mr("dir_input_geos"), "aqc_tavg_1hr_g1440x721_v1"),
        win = c(-126, -62, 22, 52),
        snap = "out"
      ),
      pattern = map(geos_dates),
      iteration = "list"
    )
    ,
    targets::tar_target(
      covariates_geos,
      command = do.call(rbind, covariates_geos_list)
    )
    ,
    targets::tar_target(
      modis_mod06_paths,
      list.files(
        mr("dir_input_modis_mod06"),
        pattern = "hdf$",
        full.names = TRUE,
        recursive = TRUE
      )
    )
    ,
    targets::tar_target(
      modis_mod11_paths,
      list.files(
        mr("dir_input_modis_mod11"),
        pattern = "hdf$",
        full.names = TRUE,
        recursive = TRUE
      )
    )
    ,
    targets::tar_target(
      modis_mod13_paths,
      list.files(
        mr("dir_input_modis_mod13"),
        pattern = "hdf$",
        full.names = TRUE,
        recursive = TRUE
      )
    )
    ,
    targets::tar_target(
      modis_mod09_paths,
      list.files(
        mr("dir_input_modis_mod09"),
        pattern = "hdf$",
        full.names = TRUE,
        recursive = TRUE
      )
    )
    ,
    targets::tar_target(
      modis_mcd19_paths,
      list.files(
        mr("dir_input_modis_mcd19"),
        pattern = "hdf$",
        full.names = TRUE,
        recursive = TRUE
      )
    )
    ,
    targets::tar_target(
      modis_vnp46_paths,
      list.files(
        mr("dir_input_modis_vnp46"),
        pattern = "h5$",
        full.names = TRUE,
        recursive = TRUE
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mod11,
      amadeus::calc_modis_par(
        from = modis_mod11_paths[1:23],
        locs = sites_spat,
        locs_id = mr("pointid"),
        name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
        subdataset = "(LST_Day_|LST_Night_)",
        preprocess = amadeus::process_modis_merge,
        nthreads = 1
      # calculate_multi(
      #   locs = sites_spat,
      #   path = mr("dir_input_modis_mod11"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = mr("pointid"),
      #   from = mr("dir_input_modis_mod11"),
      #   preprocess = amadeus::process_modis_merge,
      #   name_covariates = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
      #   subdataset = "(LST_Day_|LST_Night_)",
      #   nthreads = 8
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mod06,
      amadeus::calc_modis_par(
        from = modis_mod06_paths[1:23],
        locs = sites_spat,
        locs_id = mr("pointid"),
        name_covariates = c("MOD_CLCVD_0_", "MOD_CLCVN_0_"),
        subdataset = "(Cloud_Fraction_Day|Cloud_Fraction_Night)",
        nthreads = 1
      )
      # calculate_multi(
      #   locs = sites_spat,
      #   path = mr("dir_input_modis_mod06"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = mr("pointid"),
      #   from = mr("dir_input_modis_mod06"),
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
        from = modis_mod09_paths[1:23],
        locs = sites_spat,
        locs_id = mr("pointid"),
        preprocess = amadeus::process_modis_merge,
        name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
        subdataset = seq(2, 8),
        nthreads = 1
      # calculate_multi(
      #   locs = sites_spat,
      #   path = mr("dir_input_modis_mod09"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = mr("pointid"),
      #   from = mr("dir_input_modis_mod09"),
      #   preprocess = amadeus::process_modis_merge,
      #   name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
      #   subdataset = seq(2, 8),
      #   nthreads = 8
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mcd19_1km,
      amadeus::calc_modis_par(
        from = modis_mcd19_paths[1:23],
        locs = sites_spat,
        locs_id = mr("pointid"),
        name_covariates =
          c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
        subdataset = "(Optical_Depth)",
        preprocess = amadeus::process_modis_merge,
        nthreads = 1
      # calculate_multi(
      #   locs = sites_spat,
      #   path = mr("dir_input_modis_mcd19"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = mr("pointid"),
      #   from = mr("dir_input_modis_mcd19"),
      #   preprocess = amadeus::process_modis_merge,
      #   name_covariates =
      #     c("MOD_AD4TA_0_", "MOD_AD5TA_0_"),
      #   subdataset = "(Optical_Depth)",
      #   nthreads = 8
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mcd19_5km,
      amadeus::calc_modis_par(
        from = modis_mcd19_paths[1:23],
        locs = sites_spat,
        locs_id = mr("pointid"),
        name_covariates =
          c("MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
            "MOD_SCTAN_0_", "MOD_GLNAN_0_"),
        subdataset = "(cos|RelAZ|Angle)",
        preprocess = amadeus::process_modis_merge,
        nthreads = 1
      # calculate_multi(
      #   locs = sites_spat,
      #   path = mr("dir_input_modis_mcd19"),
      #   process_function = NULL,
      #   calc_function = amadeus::calc_modis_par,
      #   locs_id = mr("pointid"),
      #   from = mr("dir_input_modis_mcd19"),
      #   preprocess = amadeus::process_modis_merge,
      #   name_covariates =
      #     c("MOD_CSZAN_0_", "MOD_CVZAN_0_", "MOD_RAZAN_0_",
      #       "MOD_SCTAN_0_", "MOD_GLNAN_0_"),
      #   subdataset = "(cos|RelAZ|Angle)",
      #   nthreads = 8
      )
    )
    ,
    targets::tar_target(
      covariates_modis_vnp46,
      amadeus::calc_modis_par(
        from = modis_vnp46_paths[1:23],
        locs = sites_spat,
        locs_id = mr("pointid"),
        name_covariates = "MOD_LGHTN_0_",
        subdataset = 3,
        preprocess = amadeus::process_bluemarble,
        nthreads = 1
      # calculate_multi(
      #         locs = sites_spat,
      #         path = mr("dir_input_modis_vnp46"),
      #         process_function = NULL,
      #         calc_function = amadeus::calc_modis_par,
      #         locs_id = mr("pointid"),
      #         from = mr("dir_input_modis_vnp46"),
      #         preprocess = amadeus::process_bluemarble,
      #         name_covariates = "MOD_LGHTN_0_",
      #         subdataset = 3,
      #         nthreads = 8
      #       )
    )
  )
  ,
    targets::tar_target(
      covariates_modis_mod13,
      amadeus::calc_modis_par(
        from = modis_mod13_paths[1:23],
        locs = sites_spat,
        locs_id = mr("pointid"),
        name_covariates = "MOD_NDVIV_0_",
        subdataset = "(NDVI)",
        preprocess = amadeus::process_modis_merge,
        nthreads = 1
    )
  )
  ,
  # combine each covariate set into one data.frame (data.table; if any)
  targets::tar_target(
    covariates_combined_sp,
    combine(
      by = mr("pointid"),
      time = FALSE,
      covariates_koppen,
      covariates_ecoregion
    )
  )
  ,
  targets::tar_target(
    covariates_combined_spt,
    combine(
      by = mr("pointid"),
      time = TRUE,
      covariates_nlcd,
      covariates_hms,
      covariates_geos,
      covariates_gmted,
      covariates_nei,
      covariates_tri,
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
    covariates_final,
    combine_final(
      locs = sites_time,
      locs_id = mr("pointid"),
      time_id = mr("timeid"),
      target_years = seq(2018, 2022),
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
      locs_id = mr("pointid"),
      time_id = mr("timeid")
    )
  )
)





target_calculate_predict <-
  list(
    # ... calculate covariates for prediction grid
    targets::tar_target(
      covar_prediction_grid,
      read_covar_pred(mr("file_grid_prediction"))
    )
    ,
    # covariate calculation: multi vs single cases
    targets::tar_target(
      covariates_predict_tri,
      calculate_multi(
        domain = c(2018, 2019, 2020, 2021, 2022),
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_tri")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_tri"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_ecoregion,
      calculate_single(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_ecoregion")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_ecoregion"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_koppen,
      calculate_single(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_koppen")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_koppen"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_nlcd,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_nlcd")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_nlcd"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_hms,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_hms")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_hms"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_sedac_population,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_sedac_population")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_sedac_population"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_sedac_groads,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_sedac_groads")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_sedac_groads"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_narrmono,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_narrmono")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_narrmono"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_narrplevels,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_narrplevels")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_narrplevels"),
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
            mr("dir_output"), mr("file_covar_predict_nei")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_nei"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_gmted,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_gmted")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_gmted"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_geos,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_geos")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_geos"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mod11,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_modis_mod11")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_modis_mod11"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mod06,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_modis_mod06")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_modis_mod06"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mod13,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_modis_mod13")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_modis_mod13"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mcd19,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_modis_mcd19")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_modis_mcd19"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_mod09,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_modis_mod09")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_modis_mod09"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_predict_modis_vnp46,
      calculate_multi(
        outpath =
          file.path(
            mr("dir_output"), mr("file_covar_predict_modis_vnp46")
          ),
        locs = covar_prediction_grid,
        path = mr("dir_input_modis_vnp46"),
        ... # other args
      )
    )
    ,
    # combine each covariate set into one data.frame (data.table; if any)
    targets::tar_target(
      covariates_predict_combined_sp,
      combine(
        by = mr("pointid"),
        time = FALSE,
        covariates_predict_koppen,
        covariates_predict_ecoregion
      )
    )
    ,
    targets::tar_target(
      covariates_predict_combined_spt,
      combine(
        by = mr("pointid"),
        time = TRUE,
        covariates_predict_nlcd,
        covariates_predict_hms,
        covariates_predict_geos,
        covariates_predict_gmted,
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
        locs_id = mr("pointid"),
        time_id = mr("timeid"),
        target_years = seq(2018, 2022),
        df_sp = covariates_predict_combined_sp,
        df_spt = covariates_predict_combined_spt
      )
    )
  )


