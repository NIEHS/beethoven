################################################################################
##############################      BEETHOVEN      #############################
##### Main file controlling the settings, options, and sourcing of targets
##### for the beethoven analysis pipeline.
.libPaths(
  c("/mnt/lib-flex", .libPaths())
)

#############################      CONTROLLER      #############################
##### `controller_250` uses full allocation of workers (~4.0 Gb per worker).
controller_250 <- crew::crew_controller_local(
  name = "controller_250",
  workers = 250,
  seconds_idle = 30
)
calc_controller <- crew::crew_controller_local(
  name = "calc_controller",
  workers = 20,
  seconds_idle = 30
)

##############################        STORE       ##############################
targets::tar_config_set(store = "/opt/_targets")

##############################       OPTIONS      ##############################


targets::tar_option_set(
  packages = c(
    "amadeus", "targets", "tarchetypes", "dplyr", "tidyverse",
    "data.table", "sf", "crew", "crew.cluster", "lubridate", "qs2",
    "chopin"
  ),
  # add
  repository = "local",
  error = "continue",
  memory = "transient",
  format = "qs",
  storage = "worker",
  deployment = "worker",
  library = .libPaths(),
  garbage_collection = TRUE,
  seed = 202401L,
  controller = crew::crew_controller_group(
    controller_250, calc_controller
  )
)

###########################      SOURCE TARGETS      ###########################
targets::tar_source("inst/targets/targets_critical.R")
targets::tar_source("inst/targets/targets_initiate.R")
targets::tar_source("inst/targets/targets_download.R")
targets::tar_source("inst/targets/targets_aqs.R")
targets::tar_source("inst/targets/targets_calculate_fit.R")

##############################      PIPELINE      ##############################
list(
  target_critical,
  target_initiate,
  #target_download,
  target_aqs,
  target_calculate_fit,
  target_calculate_predict
)
