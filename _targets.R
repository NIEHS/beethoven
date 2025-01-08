################################################################################
##############################      BEETHOVEN      #############################
##### Main file controlling the settings, options, and sourcing of targets
##### for the beethoven analysis pipeline.

#############################      CONTROLLER      #############################
##### `controller_250` uses full allocation of workers (~4.0 Gb per worker).
controller_250 <- crew::crew_controller_local(
  name = "controller_250",
  workers = 250,
  seconds_idle = 30
)
##### `controller_100` uses 100 workers (~10.0 Gb per worker).
controller_100 <- crew::crew_controller_local(
  name = "controller_100",
  workers = 100,
  seconds_idle = 30
)
##### `controller_75` uses 75 workers (~13.33 Gb per worker).
controller_75 <- crew::crew_controller_local(
  name = "controller_75",
  workers = 75,
  seconds_idle = 30
)
##### `controller_50` uses 50 workers (~20.0 Gb per worker).
controller_50 <- crew::crew_controller_local(
  name = "controller_50",
  workers = 50,
  seconds_idle = 30
)
##### `controller_25` uses 25 workers (~40.0 Gb per worker).
controller_25 <- crew::crew_controller_local(
  name = "controller_25",
  workers = 25,
  seconds_idle = 30
)

##############################        STORE       ##############################
targets::tar_config_set(store = "/opt/_targets")

##############################       OPTIONS      ##############################
targets::tar_option_set(
  packages = c(
    "amadeus", "targets", "tarchetypes", "dplyr", "tidyverse",
    "data.table", "sf", "crew", "crew.cluster", "lubridate", "qs2",
    "torch"
  ),
  repository = "local",
  error = "continue",
  memory = "transient",
  format = "qs",
  storage = "worker",
  deployment = "worker",
  garbage_collection = TRUE,
  seed = 202401L,
  controller = crew::crew_controller_group(
    controller_250, controller_100, controller_75,
    controller_50, controller_25
  )
)

###########################      SOURCE TARGETS      ###########################
targets::tar_source("inst/targets/targets_critical.R")
targets::tar_source("inst/targets/targets_initiate.R")
targets::tar_source("inst/targets/targets_download.R")
targets::tar_source("inst/targets/targets_aqs.R")
targets::tar_source("inst/targets/targets_calculate_fit.R")
# targets::tar_source("inst/targets/targets_calculate_predict.R")
# targets::tar_source("inst/targets/targets_baselearner.R")
# targets::tar_source("inst/targets/targets_metalearner.R")
# targets::tar_source("inst/targets/targets_predict.R")

###########################      SYSTEM SETTINGS      ##########################
if (Sys.getenv("BEETHOVEN") == "covariates") {
  target_baselearner <- target_metalearner <- target_predict <- NULL
}

##############################      PIPELINE      ##############################
list(
  target_critical,
  target_initiate,
  target_download,
  target_aqs,
  target_calculate_fit
  # target_calculate_predict,
  # target_baselearner,
  # target_metalearner,
  # target_predict
)
