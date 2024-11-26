################################################################################
##############################      BEETHOVEN      #############################
##### Main file controlling the settings, options, and sourcing of targets
##### for the beethoven analysis pipeline.

#############################      CONTROLLER      #############################
##### `default_controller` uses full allocation of workers (~4.5 Gb per worker).
default_controller <- crew::crew_controller_local(
  name = "default_controller",
  workers = 200,
  seconds_idle = 30
)
##### `midmem_controller` uses 50 workers (~18 Gb per worker).
midmem_controller <- crew::crew_controller_local(
  name = "midmem_controller",
  workers = 50,
  seconds_idle = 30
)

##############################        STORE       ##############################
targets::tar_config_set(store = "/opt/_targets")

##############################       OPTIONS      ##############################
targets::tar_option_set(
  packages = c(
    "amadeus", "targets", "tarchetypes", "dplyr", "tidyverse",
    "data.table", "sf", "crew", "crew.cluster", "lubridate", "qs2"
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
    default_controller, midmem_controller
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
  target_download,
  target_aqs,
  target_calculate_fit
)
