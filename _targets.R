##### Libraries for the _targets.R file #####
library(targets)
library(crew)
library(crew.cluster)


##############################        STORE       ##############################
targets::tar_config_set(
  store = "/ddn/gs1/group/set/beethoven_store/_targets"
)

###########################      SOURCE TARGETS      ###########################
targets::tar_source("inst/targets/targets_critical.R")
targets::tar_source("inst/targets/targets_initiate.R")
targets::tar_source("inst/targets/targets_download.R")
targets::tar_source("inst/targets/targets_aqs.R")
targets::tar_source("inst/targets/targets_calculate_fit.R")
targets::tar_source("inst/targets/targets_baselearner.R")
targets::tar_source("inst/targets/targets_metalearner.R")
targets::tar_source("inst/targets/targets_calculate_predict.R")
targets::tar_source("R/controllers.R")
targets::tar_source("R/")

beethoven_packages <- c(
  "amadeus",
  "targets",
  "tarchetypes",
  "dplyr",
  "data.table",
  "sf",
  "crew",
  "crew.cluster",
  "lubridate",
  "qs2",
  "torch",
  "parsnip",
  "bonsai",
  "dials",
  "lightgbm",
  "glmnet",
  "finetune",
  "spatialsample",
  "tidymodels",
  "brulee",
  "workflows",
  "terra"
)

targets::tar_option_set(
  repository = "local",
  packages = beethoven_packages,
  error = "continue",
  memory = "auto",
  format = "auto",
  storage = "worker",
  deployment = "worker",
  garbage_collection = 10L,
  seed = 202401L,
  controller = crew::crew_controller_group(
    controller_general_beethoven,
    controller_aqs,
    controller_download_geo,
    controller_download_norm,
    controller_download_norm_big,
    controller_calc_fit_geo,
    controller_calc_fit_norm,
    controller_calc_pred_geo,
    controller_calc_pred_norm,
    controller_calc_pred_norm_big,
    controller_calc_pred_norm_huge,
    controller_baselearners_geo,
    controller_baselearners_norm,
    controller_baselearners_norm_big,
    controller_baselearners_norm_huge,
    controller_metalearners_geo,
    controller_metalearners_norm,
    controller_metalearners_norm_big,
    controller_metalearners_norm_huge,
    controller_predict_geo,
    controller_predict_norm,
    controller_predict_norm_big,
    controller_predict_norm_huge,
    controller_initiate
  ),
  resources = targets::tar_resources(
    crew = targets::tar_resources_crew(
      controller = "controller_general_beethoven"
    )
  ),
  retrieval = "worker"
)
# sqltargets::sqltargets_option_set("sqltargets.template_engine", "jinjar")

# targets::tar_source("/inst/targets/targets_predict.R")

##############################      PIPELINE      ##############################
list(
  target_critical,
  target_initiate,
  target_download,
  target_aqs
  # target_calculate_fit,
  # target_baselearner,
  # target_baselearner_elnet,
  # target_baselearner_mlp,
  # target_baselearner_lgb,
  # target_metalearner,
  # target_calculate_predict
  # target_predict
)
