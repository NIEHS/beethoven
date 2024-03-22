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
        domain = c(2018, 2019, 2020, 2021, 2022),
        locs = sites_spat,
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
        path = mr("dir_input_ecoregion"),
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
        domain = c(2019, 2019, 2019, 2021, 2021),
        locs = sites_spat,
        path = mr("dir_input_nlcd"),
        locs_id = mr("pointid"),
        covariate = "nlcd",
        buffer = radii
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
        locs = sites_spat,
        path = mr("dir_input_sedac_population"),
        ... # other args
      )
    )
    ,
    # 3 branches
    targets::tar_target(
      covariates_sedac_groads,
      command = calculate_single(
        locs = as.data.frame(sites_spat),
        path = mr("dir_input_sedac_groads"),
        locs_id = mr("pointid"),
        covariate = "sedac_groads",
        radius = radii
      ),
      pattern = map(radii),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      covariates_narrmono,
      calculate_multi(
        locs = sites_spat,
        path = mr("dir_input_narrmono"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_narrplevels,
      calculate_multi(
        status = status_narrplevels,
        locs = sites_spat,
        path = mr("dir_input_narrplevels"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      county_poly,
      command = load_county()
    )
    ,
    targets::tar_target(
      nei_dirs,
      command = c(rep(mr("dir_input_nei2017"), 2), rep(mr("dir_input_nei2020"), 3)),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      covariates_nei,
      calculate_multi(
        domain = c(2017, 2017, 2020, 2020, 2020),
        locs = sites_spat,
        path = nei_dirs,
        county = county_poly,
        locs_id = mr("pointid")
      ),
      pattern = map(nei_dirs),
      iteration = "vector"
    )
    ,
    targets::tar_target(
      covariates_gmted,
      calculate_multi(
        status = status_gmted,
        locs = sites_spat,
        path = mr("dir_input_gmted"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_geos,
      calculate_multi(
        locs = sites_spat,
        path = mr("dir_input_geos"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mod11,
      calculate_multi(
        locs = sites_spat,
        path = mr("dir_input_modis_mod11"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mod06,
      calculate_multi(
        locs = sites_spat,
        path = mr("dir_input_modis_mod06"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mod13,
      calculate_multi(
        locs = sites_spat,
        path = mr("dir_input_modis_mod13"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mcd19,
      calculate_multi(
        locs = sites_spat,
        path = mr("dir_input_modis_mcd19"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_modis_mod09,
      calculate_multi(
        locs = sites_spat,
        path = mr("dir_input_modis_mod09"),
        ... # other args
      )
    )
    ,
    targets::tar_target(
      covariates_modis_vnp46,
      calculate_multi(
        locs = sites_spat,
        path = mr("dir_input_modis_vnp46"),
        ... # other args
      )
    ),
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
      time = FALSE,
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
      covariates_modis_mcd19,
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
        status = status_tri,
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
        status = status_ecoregion,
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
        status = status_koppen,
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
        status = status_nlcd,
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
        status = status_hms,
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
        status = status_sedac_population,
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
        status = status_sedac_groads,
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
        status = status_narrmono,
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
        status = status_narrplevels,
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
        status = status_nei,
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
        status = status_gmted,
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
        status = status_geos,
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
        status = status_modis_mod11,
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
        status = status_modis_mod06,
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
        status = status_modis_mod13,
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
        status = status_modis_mcd19,
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
        status = status_modis_mod09,
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
        status = status_modis_vnp46,
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
        time = FALSE,
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
        covariates_predict_modis_mcd19,
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


