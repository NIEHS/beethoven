##### Libraries for the _targets.R file #####
library(targets)
library(crew)
library(crew.cluster)


###########################      SOURCE TARGETS      ###########################
targets::tar_source("inst/targets/targets_critical.R")
targets::tar_source("inst/targets/targets_initiate.R")
targets::tar_source("inst/targets/targets_download.R")
targets::tar_source("inst/targets/targets_aqs.R")
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
        controller_initiate
    ),
    resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
            controller = "controller_general_beethoven"
        )
    ),
    retrieval = "worker"
)


##############################      PIPELINE      ##############################
list(
    target_critical,
    target_initiate,
    target_download,
    target_aqs
)
