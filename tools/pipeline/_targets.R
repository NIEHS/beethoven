if (!require(targets)) {
  pak::pak("targets")
  library(targets)
}

source("./tools/pipeline/pipeline_base_functions.R")

# targets options
tar_option_set(
  packages =
    c("beethoven", "amadeus", "scomps",
      "data.table", "sf", "terra", "exactextractr",
      "sftime", "stars", "rlang", "foreach", "parallelly"),
  repository = "local",
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
  # tar_target for download and checking presence
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
    status_sedacpop,
    fastdown(mr("dir_input_sedacpop"))
  )
  ,
  targets::tar_target(
    status_sedacroad,
    fastdown(mr("dir_input_sedacroad"))
  )
  ,
  targets::tar_target(
    calculate_tri,
    calculate_multi(
      status = status_tri,
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
  # ...
  # covariate file existence check then calculate covariates
  targets::tar_target(covar_modis, calculate_covariates("modis", ...))
  ,
  # ...
  # combine each covariate set into one data.frame (data.table; if any)
  targets::tar_target(covar_all, concatenate_covariates(...))
  ,
  # tar_target for initial model fitting and tuning
  targets::tar_target(fitted_ranger, base_learner("randomforest"))
  ,
  targets::tar_target(fitted_xgb, base_learner("xgboost"))
  ,
  # CNN, if any
  # meta learner
  targets::tar_target(fitted_meta, meta_learner(list(fitted_ranger, fitted_xgb, fitted_cnn, ...)))
  ,
  # tar_target for initial model update if pre-fitted
  # model exists; is it possible to nest pipelines?
  targets::tar_target(data_updated, foo_above_all(...))
  ,
  targets::tar_target(fitted_2025_ranger, base_learner("randomforest", data_updated, ...))
  ,
  targets::tar_target(fitted_2025_xgb, base_learner("xgboost", data_updated, ...))
  ,
  # if any
  # tar_target for 8+M point covariate calculation
  targets::tar_target(covar_modis_pred, calculate_covariates("modis", usmain_p8m))
  ,
  targets::tar_target(covar_ncep_pred, calculate_covariates("ncep", usmain_p8m, ...))
  ,
  # others
  targets::tar_target(covar_all_pred, concateneate_covariates(...))
  ,
  # tar_target for prediction using pre-fitted models
  targets::tar_target(pred_p8m, predict_meta(fitted_meta, covar_all_pred))
  ,
  # documents and summary statistics
  targets::tar_target(summary_urban_rural, summary_prediction(pred_p8m, level = "point", contrast = "urbanrural"))
  ,
  targets::tar_target(summary_state, summary_prediction(pred_p8m, level = "point", contrast = "state"))
)


# targets::tar_visnetwork()
# END OF FILE
