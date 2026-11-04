if (!require(targets)) {
  pak::pak("targets")
  library(targets)
}

source("./tools/pipeline/pipeline_base_functions.R")

# targets options
tar_option_set(
  packages =
    c("beethoven", "amadeus", "chopin",
      "data.table", "sf", "terra", "exactextractr",
      "sftime", "stars", "rlang", "foreach", "parallelly"),
  repository = "local",
  controller =
  crew.cluster::crew_class_launcher_slurm(
    slurm_log_output = "output/slurm_pipeline_log.out",
    slurm_log_error = "output/slurm_pipeline_error.err",
    slurm_memory_gigabytes_per_cpu = "8g",
    slurm_cpus_per_task = 8L,
    slurm_time_minutes = NULL,
    slurm_partition = "geo"
  ),
  error = "null",
  memory = "persistent",
  storage = "worker",
  seed = 202401L
)

list(
  # tar_target for base directories and files
  targets::tar_target(sites_spat, read_locs(mr("dir_input_aqs")))
  ,
  targets::tar_target(sites_time, read_locs(mr("dir_input_aqs")))
  ,
  targets::tar_target(
    sites_pm,
    get_aqs_data()
  )
  ,
  # tar_target for download and checking existence (for fitting models)
  targets::tar_target(
    status_modis_mod11,
    fastdown(mr("dir_input_mod11"))
  )
  ,
  targets::tar_target(
    status_modis_mod13,
    fastdown(mr("dir_input_mod13"))
  )
  ,
  targets::tar_target(
    status_modis_mod09,
    fastdown(mr("dir_input_mod09"))
  )
  ,
  targets::tar_target(
    status_modis_mod06,
    fastdown(mr("dir_input_mod06"))
  )
  ,
  targets::tar_target(
    status_modis_vnp46,
    fastdown(mr("dir_input_vnp46"))
  )
  ,
  targets::tar_target(
    status_ecoregion,
    fastdown(mr("dir_input_ecoregion"))
  )
  ,
  targets::tar_target(
    status_koppen,
    fastdown(mr("dir_input_koppen"))
  )
  ,
  targets::tar_target(
    status_nlcd,
    fastdown(mr("dir_input_nlcd"))
  )
  ,
  targets::tar_target(
    status_nei,
    fastdown(mr("dir_input_nei"))
  )
  ,
  targets::tar_target(
    status_geos,
    fastdown(mr("dir_input_geos"))
  )
  ,
  targets::tar_target(
    status_gmted,
    fastdown(mr("dir_input_gmted"))
  )
  ,
  targets::tar_target(
    status_tri,
    fastdown(mr("dir_input_tri"))
  )
  ,
  targets::tar_target(
    status_narrmono,
    fastdown(mr("dir_input_narrmono"))
  )
  ,
  targets::tar_target(
    status_narrplevels,
    fastdown(mr("dir_input_narrplevels"))
  )
  ,
  targets::tar_target(
    status_hms,
    fastdown(mr("dir_input_hms"))
  )
  ,
  targets::tar_target(
    status_sedac_population,
    fastdown(mr("dir_input_sedac_population"))
  )
  ,
  targets::tar_target(
    status_sedac_groads,
    fastdown(mr("dir_input_sedac_groads"))
  )
  ,
  # covariate calculation: multi vs single cases
  targets::tar_target(
    covariates_tri,
    calculate_multi(
      status = status_tri,
      domain = c(2018, 2019, 2020, 2021, 2022),
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_tri")
        ),
      locs = sites_spat,
      path = mr("dir_input_tri"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_ecoregion,
    calculate_single(
      status = status_ecoregion,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_ecoregion")
        ),
      locs = sites_spat,
      path = mr("dir_input_ecoregion"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_koppen,
    calculate_single(
      status = status_koppen,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_koppen")
        ),
      locs = sites_spat,
      path = mr("dir_input_koppen"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_nlcd,
    calculate_multi(
      status = status_nlcd,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_nlcd")
        ),
      locs = sites_spat,
      path = mr("dir_input_nlcd"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_hms,
    calculate_multi(
      status = status_hms,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_hms")
        ),
      locs = sites_spat,
      path = mr("dir_input_hms"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_sedac_population,
    calculate_multi(
      status = status_sedac_population,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_sedac_population")
        ),
      locs = sites_spat,
      path = mr("dir_input_sedac_population"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_sedac_groads,
    calculate_multi(
      status = status_sedac_groads,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_sedac_groads")
        ),
      locs = sites_spat,
      path = mr("dir_input_sedac_groads"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_narrmono,
    calculate_multi(
      status = status_narrmono,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_narrmono")
        ),
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
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_narrplevels")
        ),
      locs = sites_spat,
      path = mr("dir_input_narrplevels"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_nei,
    calculate_multi(
      status = status_nei,
      domain = c(2017, 2017, 2020, 2020, 2020),
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_nei")
        ),
      locs = sites_spat,
      path = mr("dir_input_nei"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_gmted,
    calculate_multi(
      status = status_gmted,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_gmted")
        ),
      locs = sites_spat,
      path = mr("dir_input_gmted"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_geos,
    calculate_multi(
      status = status_geos,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_geos")
        ),
      locs = sites_spat,
      path = mr("dir_input_geos"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_modis_mod11,
    calculate_multi(
      status = status_modis_mod11,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_modis_mod11")
        ),
      locs = sites_spat,
      path = mr("dir_input_modis_mod11"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_modis_mod06,
    calculate_multi(
      status = status_modis_mod06,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_modis_mod06")
        ),
      locs = sites_spat,
      path = mr("dir_input_modis_mod06"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_modis_mod13,
    calculate_multi(
      status = status_modis_mod13,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_modis_mod13")
        ),
      locs = sites_spat,
      path = mr("dir_input_modis_mod13"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_modis_mcd19,
    calculate_multi(
      status = status_modis_mcd19,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_modis_mcd19")
        ),
      locs = sites_spat,
      path = mr("dir_input_modis_mcd19"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_modis_mod09,
    calculate_multi(
      status = status_modis_mod09,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_modis_mod09")
        ),
      locs = sites_spat,
      path = mr("dir_input_modis_mod09"),
      ... # other args
    )
  )
  ,
  targets::tar_target(
    covariates_modis_vnp46,
    calculate_multi(
      status = status_modis_vnp46,
      outpath =
        file.path(
          mr("dir_output"), mr("file_covar_modis_vnp46")
        ),
      locs = sites_spat,
      path = mr("dir_input_modis_vnp46"),
      ... # other args
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
  ,
  targets::tar_target(
    cv_config,
    configure_cv(
      covars = covariates_final
    )
  )
  ,
  targets::tar_target(
    base_prep_rf,
    base_learner_prep(
      learner = "randomforest",
      data = data_full,
      dependent_name = mr("name_dep"),
      independent_name = file.path(mr("dir_output"), mr("file_name_indep"))
    )
  )
  ,
  targets::tar_target(
    base_prep_xgboost,
    base_learner_prep(
      learner = "xgboost",
      data = data_full,
      dependent_name = mr("name_dep"),
      independent_name = file.path(mr("dir_output"), mr("file_name_indep"))
    )
  )
  ,
  targets::tar_target(
    base_prep_cnn,
    base_learner_prep(
      learner = "cnn",
      data = data_full,
      dependent_name = mr("name_dep"),
      independent_name = file.path(mr("dir_output"), mr("file_name_indep"))
    )
  )
  ,
  targets::tar_target(
    base_fit_rf,
    base_learner_cv_fit(
      "randomforest",
      ymat = base_prep_rf$ymat,
      xmat = base_prep_rf$xmat,
      cv_index = cv_config$lblto,
      fun = base_learner_fit_ranger
    )
  )
  ,
  targets::tar_target(
    base_fit_xgboost,
    base_learner_cv_fit(
      "xgboost",
      ymat = base_prep_xgboost$ymat,
      xmat = base_prep_xgboost$xmat,
      cv_index = cv_config$lblto,
      fun = base_learner_fit_xgboost
    )
  )
  ,
  targets::tar_target(
    base_fit_cnn,
    base_learner_cv_fit(
      "cnn",
      ymat = base_prep_cnn$ymat,
      xmat = base_prep_cnn$xmat,
      cv_index = cv_config$lblto,
      fun = base_learner_fit_cnn
    )
  )
  ,
  # meta learner
  targets::tar_target(
    meta_fit,
    meta_learner(
      list(base_fit_rf, base_fit_xgboost, base_fit_cnn),
      kfolds = 10L,
      y = data_full[[mr("name_dep")]]
    )
  )
  ,
  # tar_target for initial model update if pre-fitted
  targets::tar_target(
    meta_exported,
    export_res(meta_fit)
  )
  ,
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
  ,
  # ... calculate covariates at prediction grid
  # something similar to calculation at sites
  # predict
  targets::tar_target(
    grid_filled,
    predict_meta(
      metalearner = meta_fit,
      targetdf = covariates_predict_final,
      threads = mr("nthreads_predict")
    )
  )
  ,
  # documents and summary statistics
  targets::tar_target(
    summary_urban_rural,
    summary_prediction(
      grid_filled,
      level = "point",
      contrast = "urbanrural"))
  ,
  targets::tar_target(
    summary_state,
    summary_prediction(
      grid_filled,
      level = "point",
      contrast = "state"
    )
  )
)

# targets::tar_visnetwork()
# END OF FILE
