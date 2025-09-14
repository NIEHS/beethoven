################################################################################
##############################      BEETHOVEN      #############################
##### Main file controlling the settings, options, and sourcing of targets
##### for the beethoven analysis pipeline.
# Sys.setenv(
#   BEETHOVEN = "covariates")
bypass_condition <- function() {
  # Your logic here - return TRUE when you want to skip computation
  Sys.getenv("BEETHOVEN") == "covariates"
}
.libPaths(c("/mnt/lib-flex", .libPaths()))

#############################      CONTROLLER      #############################
##### `controller_250` uses full allocation of workers (~4.0 Gb per worker).
controller_250 <- crew::crew_controller_local(
  name = "controller_250",
  workers = 250
)
##### `controller_100` uses 100 workers (~10.0 Gb per worker).
controller_100 <- crew::crew_controller_local(
  name = "controller_100",
  workers = 100
)
##### `controller_50` uses 50 workers (~20.0 Gb per worker).
controller_50 <- crew::crew_controller_local(
  name = "controller_50",
  workers = 50
)
##### `controller_40` uses 40 workers (~25.0 Gb per worker).
controller_40 <- crew::crew_controller_local(
  name = "controller_40",
  workers = 40
)
##### `controller_30` uses 30 workers (~33.0 Gb per worker).
controller_30 <- crew::crew_controller_local(
  name = "controller_30",
  workers = 30
)
##### `controller_25` uses 25 workers (~40.0 Gb per worker).
controller_25 <- crew::crew_controller_local(
  name = "controller_25",
  workers = 25
)
##### `controller_10` uses 10 workers (~90.0 Gb per worker).
controller_10 <- crew::crew_controller_local(
  name = "controller_10",
  workers = 10
)
##### `controller_5` uses 5 workers (~180.0 Gb per worker).
controller_5 <- crew::crew_controller_local(
  name = "controller_5",
  workers = 5
)
##### `controller_1` uses 1 worker for sequential {lightGBM} models.
controller_1 <- crew::crew_controller_local(
  name = "controller_1",
  workers = 1
)
##### `controller_geo` uses 4 GPU workers (undefined memory allocation).
scriptlines_apptainer <- "apptainer"
scriptlines_basedir <- "$PWD"
scriptlines_targetdir <- "/ddn/gs1/group/set/Projects/beethoven"
scriptlines_inputdir <- "/ddn/gs1/group/set/Projects/NRT-AP-Model/input"
scriptlines_container <- "container_models.sif"
scriptlines_geo <- glue::glue(
  "#SBATCH --job-name=submodel \
  #SBATCH --partition=geo \
  #SBATCH --gres=gpu:1 \
  #SBATCH --error=slurm/submodel_%j.out \
  {scriptlines_apptainer} exec --nv --env ",
  "CUDA_VISIBLE_DEVICES=${{GPU_DEVICE_ORDINAL}} ",
  "--bind {scriptlines_basedir}:/mnt ",
  "--bind {scriptlines_basedir}/inst:/inst ",
  "--bind {scriptlines_inputdir}:/input ",
  "--bind {scriptlines_targetdir}/targets:/opt/_targets ",
  "{scriptlines_container} \\"
)
controller_geo <- crew.cluster::crew_controller_slurm(
  name = "controller_geo",
  workers = 4,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_geo
  )
)
##### `controller_sequential` uses 1 GPU worker for {lightGBM} models..
controller_sequential <- crew.cluster::crew_controller_slurm(
  name = "controller_sequential",
  workers = 1,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_geo
  )
)

scriptlines_backup <- glue::glue(
  "#SBATCH --job-name=g_back \
  #SBATCH --partition=normal,highmem \
  #SBATCH --ntasks=1 \
  #SBATCH --cpus-per-task=1 \
  #SBATCH --mem=200G \
  #SBATCH --error=slurm/grid_%j.out \
  export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK \
  {scriptlines_apptainer} exec --env OMP_NUM_THREADS=$OMP_NUM_THREADS ",
  "--bind {scriptlines_basedir}:/mnt ",
  "--bind {scriptlines_basedir}/inst:/inst ",
  "--bind {scriptlines_inputdir}:/input ",
  "--bind {scriptlines_targetdir}/targets:/opt/_targets ",
  "{scriptlines_container} \\"
)

controller_backup <- crew.cluster::crew_controller_slurm(
  name = "controller_backup",
  workers = 100,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_backup
  ),
  garbage_collection = TRUE
)

##### `controller_grid` uses 100 CPUs for {grid covariates} models.
scriptlines_grid <- glue::glue(
  "#SBATCH --job-name=grid \
  #SBATCH --partition=normal,highmem \
  #SBATCH --ntasks=1 \
  #SBATCH --cpus-per-task=1 \
  #SBATCH --mem=35G \
  #SBATCH --error=slurm/grid_%j.out \
  export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK \
  {scriptlines_apptainer} exec --env OMP_NUM_THREADS=$OMP_NUM_THREADS ",
  "--bind {scriptlines_basedir}:/mnt ",
  "--bind {scriptlines_basedir}/inst:/inst ",
  "--bind {scriptlines_inputdir}:/input ",
  "--bind {scriptlines_targetdir}/targets:/opt/_targets ",
  "{scriptlines_container} \\"
)

controller_grid <- crew.cluster::crew_controller_slurm(
  name = "controller_grid",
  workers = 1000,
  crashes_max = 5L,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_grid
  ),
  backup = controller_backup
  # Removed options_metrics to avoid dplyr error: 'Unknown or uninitialised column: worker'
  # Re-enable after updating crew and crew.cluster to compatible versions.
)

scriptlines_big_grid <- glue::glue(
  "#SBATCH --job-name=biggrid \
  #SBATCH --partition=normal,highmem \
  #SBATCH --ntasks=1 \
  #SBATCH --cpus-per-task=2 \
  #SBATCH --mem=100G \
  #SBATCH --error=slurm/bgrid_%j.out \
  export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK \
  {scriptlines_apptainer} exec --env OMP_NUM_THREADS=$OMP_NUM_THREADS ",
  "--bind {scriptlines_basedir}:/mnt ",
  "--bind {scriptlines_basedir}/inst:/inst ",
  "--bind {scriptlines_inputdir}:/input ",
  "--bind {scriptlines_targetdir}/targets:/opt/_targets ",
  "{scriptlines_container} \\"
)

controller_big_grid <- crew.cluster::crew_controller_slurm(
  name = "controller_big_grid",
  workers = 500,
  crashes_max = 5L,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_big_grid
  ),
  backup = controller_backup,
  garbage_collection = TRUE
  # Removed options_metrics to avoid dplyr error: 'Unknown or uninitialised column: worker'
  # Re-enable after updating crew and crew.cluster to compatible versions.
)
#   autometric::log_start(
#     path = "main_process.txt", # Statistics on the main process go here.
#     seconds = 1
#   )
# }

##############################        STORE       ##############################
targets::tar_config_set(store = "/opt/_targets")

##############################       OPTIONS      ##############################
# Sys.setenv("BEETHOVEN" = "covariates")
if (Sys.getenv("BEETHOVEN") == "covariates") {
  beethoven_packages <- c(
    "amadeus",
    "beethoven",
    "targets",
    "tarchetypes",
    "dplyr",
    "tidyverse",
    "data.table",
    "sf",
    "mirai",
    "crew",
    "crew.cluster",
    "lubridate",
    "qs2",
    "kernlab",
    "workflows",
    "h3",
    "h3r",
    "autometric"
  )
} else {
  beethoven_packages <- c(
    "amadeus",
    "beethoven",
    "targets",
    "tarchetypes",
    "dplyr",
    "tidyverse",
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
    # "lightgbm",
    "glmnet",
    "finetune",
    "spatialsample",
    # "tidymodels",
    # "brulee",
    "h3",
    "h3r",
    "workflows"
  )
}
targets::tar_option_set(
  packages = beethoven_packages,
  repository = "local",
  library = .libPaths(c("/mnt/lib-flex", .libPaths())),
  error = "continue",
  memory = "auto",
  format = "qs",
  storage = "worker",
  deployment = "worker",
  garbage_collection = TRUE,
  seed = 202401L,
  controller = crew::crew_controller_group(
    controller_250,
    controller_100,
    controller_50,
    controller_40,
    controller_30,
    controller_25,
    controller_10,
    controller_5,
    controller_1,
    controller_geo,
    controller_sequential,
    controller_grid,
    controller_backup,
    controller_big_grid
  ),
  resources = targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "controller_25")
  ),
  retrieval = "worker"
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
# targets::tar_source("inst/targets/targets_predict.R")
targets::tar_source() #All of the R/

###########################           STAGES          ##########################
if (Sys.getenv("BEETHOVEN") == "covariates") {
  target_baselearner <-
    target_baselearner_elnet <-
      target_baselearner_lgb <-
        target_baselearner_mlp <-
          target_metalearner <-
            target_calculate_predict <-
              target_predict <- list()
} else if (Sys.getenv("BEETHOVEN") == "elnet") {
  target_baselearner_lgb <-
    target_baselearner_mlp <-
      target_metalearner <-
        target_calculate_predict <-
          target_predict <- list()
} else if (Sys.getenv("BEETHOVEN") == "mlp") {
  target_baselearner_lgb <-
    target_metalearner <-
      target_calculate_predict <-
        target_predict <- list()
} else if (Sys.getenv("BEETHOVEN") == "lgb") {
  target_metalearner <-
    target_calculate_predict <-
      target_predict <- list()
} else if (Sys.getenv("BEETHOVEN") == "meta") {
  target_calculate_predict <-
    target_predict <- list()
}

##############################      PIPELINE      ##############################
list(
  target_critical,
  target_initiate,
  # target_download,
  target_aqs,
  target_calculate_fit,
  target_baselearner,
  target_baselearner_elnet,
  target_baselearner_mlp,
  target_baselearner_lgb,
  # target_metalearner
  target_calculate_predict
  # target_predict
)
