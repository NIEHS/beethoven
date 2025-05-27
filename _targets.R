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
  workers = 250#,
  # seconds_idle = 30
)
##### `controller_100` uses 100 workers (~10.0 Gb per worker).
controller_100 <- crew::crew_controller_local(
  name = "controller_100",
  workers = 100#,
  # seconds_idle = 30
)
##### `controller_75` uses 75 workers (~13.33 Gb per worker).
controller_75 <- crew::crew_controller_local(
  name = "controller_75",
  workers = 75#,
  # seconds_idle = 30
)
##### `controller_50` uses 50 workers (~20.0 Gb per worker).
controller_50 <- crew::crew_controller_local(
  name = "controller_50",
  workers = 50
)
##### `controller_25` uses 25 workers (~40.0 Gb per worker).
controller_25 <- crew::crew_controller_local(
  name = "controller_25",
  workers = 25#,
  # seconds_idle = 30
)

##### `controller_15` uses 15 workers (50~66.0 GB per worker).
controller_15 <- crew::crew_controller_local(
  name = "controller_15",
  workers = 15
)

##### `controller_10` uses 10 workers (66~75.0 GB per worker).
controller_10 <- crew::crew_controller_local(
  name = "controller_10",
  workers = 10
)

##### `controller_03` uses 3 workers (250.0-330.0 GB per worker).
controller_03 <- crew::crew_controller_local(
  name = "controller_03",
  workers = 3
)
##### `controller_01` uses 1 workers (750.0 GB per worker).
controller_01 <- crew::crew_controller_local(
  name = "controller_01",
  workers = 1
)

##### `controller_gpu` uses 4 GPU workers.
scriptlines_apptainer <- "apptainer"
scriptlines_basedir <- "$PWD"
scriptlines_container <- "container_models.sif"
scriptlines_inputdir <- "/ddn/gs1/group/set/Projects/NRT-AP-Model/input"
scriptlines_gpu <- glue::glue(
  "#SBATCH --job-name=beethovengpu \
  #SBATCH --partition=geo \
  #SBATCH --gres=gpu:1 \
  #SBATCH --error=slurm/beethovengpu_%j.out \
  {scriptlines_apptainer} exec --nv --bind {scriptlines_basedir}:/mnt ",
  "--bind {scriptlines_basedir}/inst:/inst ",
  "--bind {scriptlines_inputdir}:/input ",
  "--bind {scriptlines_basedir}/_targets:/opt/_targets ",
  "{scriptlines_container} \\"
)
controller_gpu <- crew.cluster::crew_controller_slurm(
  name = "controller_gpu",
  workers = 4,
  # seconds_idle = 30,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_gpu
  )
)

##############################        STORE       ##############################
targets::tar_config_set(store = "/opt/_targets")

##############################       OPTIONS      ##############################
if (Sys.getenv("BEETHOVEN") == "covariates") {
  beethoven_packages <- c(
    "amadeus", "targets", "tarchetypes", "dplyr", "tidyverse",
    "data.table", "sf", "crew", "crew.cluster", "lubridate", "qs2",
    "beethoven", "chopin"
  )
} else {
  beethoven_packages <- c(
    "amadeus", "targets", "tarchetypes", "dplyr", "tidyverse",
    "data.table", "sf", "crew", "crew.cluster", "lubridate", "qs2",
    "torch", "bonsai", "dials", "lightgbm", "xgboost", "glmnet",
    "beethoven", "chopin", "arrow", "nanoparquet"
  )
}
targets::tar_option_set(
  packages = beethoven_packages,
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
    controller_250, controller_100, controller_75,
    controller_50, controller_25, controller_15,
    controller_10, controller_03, controller_01#, controller_gpu
  ),
  resources = targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "controller_50")
  ),
  retrieval = "worker"
)

###########################      SOURCE TARGETS      ###########################
# dev
targets::tar_source("inst/targets/targets_dynamic_dev.R")

targets::tar_source("inst/targets/targets_critical.R")
targets::tar_source("inst/targets/targets_initiate.R")
targets::tar_source("inst/targets/targets_download.R")
targets::tar_source("inst/targets/targets_aqs.R")
targets::tar_source("inst/targets/targets_calculate_fit.R")
targets::tar_source("inst/targets/targets_baselearner.R")
# targets::tar_source("inst/targets/targets_metalearner.R")
targets::tar_source("inst/targets/targets_calculate_predict_dev.R")
# targets::tar_source("inst/targets/targets_predict.R")

###########################      SYSTEM SETTINGS      ##########################
if (Sys.getenv("BEETHOVEN") == "covariates") {
  target_baselearner <- target_metalearner <- target_predict <- NULL
}

##############################      PIPELINE      ##############################
list(
  target_critical,
  target_initiate,
  #target_download,
  target_aqs,
  target_calculate_fit,
  # target_baselearner
  # target_metalearner,
  target_calculate_predict
  # target_predict
)
