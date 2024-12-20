################################################################################
##############################      BEETHOVEN      #############################
##### Main file controlling the settings, options, and sourcing of targets
##### for the beethoven analysis pipeline.
.libPaths(
  c("/mnt/lib-flex", .libPaths())
)

#############################      CONTROLLER      #############################
default_controller <- crew::crew_controller_local(
  name = "default_controller",
  workers = 4,
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
    "beethoven", "targets", "tarchetypes", "dplyr",
    "data.table", "sf", "crew", "crew.cluster",
    "amadeus"
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
    default_controller,
    calc_controller
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
  target_calculate_fit
)
