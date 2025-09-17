################################################################################
##############################      BEETHOVEN      #############################
##### Main file controlling the settings, options, and sourcing of targets
##### for the beethoven analysis pipeline.

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
##### `controller_gpu` uses 4 GPU workers (undefined memory allocation).
scriptlines_apptainer <- "apptainer"
scriptlines_basedir <- "$PWD"
scriptlines_targetdir <- "/ddn/gs1/group/set/Projects/beethoven"
scriptlines_inputdir <- "/ddn/gs1/group/set/Projects/NRT-AP-Model/input"
scriptlines_container <- "container_models.sif"
scriptlines_mlp <- glue::glue(
  "#SBATCH --job-name=mlp \
  #SBATCH --partition=geo \
  #SBATCH --gres=gpu:1 \
  #SBATCH --error=slurm/mlp_%j.out \
  {scriptlines_apptainer} exec --nv --env ",
  "CUDA_VISIBLE_DEVICES=${{GPU_DEVICE_ORDINAL}} ",
  "--bind {scriptlines_basedir}:/mnt ",
  "--bind {scriptlines_basedir}/inst:/inst ",
  "--bind {scriptlines_inputdir}:/input ",
  "--bind {scriptlines_targetdir}/targets:/opt/_targets ",
  "{scriptlines_container} \\"
)
controller_mlp <- crew.cluster::crew_controller_slurm(
  name = "controller_mlp",
  workers = 4,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_mlp
  )
)

##### `controller_lgb` uses 100 CPUs for {lightGBM} models.
scriptlines_lgb <- glue::glue(
  "#SBATCH --job-name=lgb \
  #SBATCH --partition=gpu \
  #SBATCH --nodelist=gn040809 \
  #SBATCH --ntasks=1 \
  #SBATCH --cpus-per-task=25 \
  #SBATCH --mem=500G \
  #SBATCH --error=slurm/lgb_%j.out \
  export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK \
  export LIGHTGBM_NUM_THREADS=$SLURM_CPUS_PER_TASK \
  {scriptlines_apptainer} exec --env OMP_NUM_THREADS=$OMP_NUM_THREADS ",
  "--env LIGHTGBM_NUM_THREADS=$LIGHTGBM_NUM_THREADS ",
  "--bind {scriptlines_basedir}:/mnt ",
  "--bind {scriptlines_basedir}/inst:/inst ",
  "--bind {scriptlines_inputdir}:/input ",
  "--bind {scriptlines_targetdir}/targets:/opt/_targets ",
  "{scriptlines_container} \\"
)
controller_lgb <- crew.cluster::crew_controller_slurm(
  name = "controller_lgb",
  workers = 25,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_lgb
  )
)

scriptlines_backup <- glue::glue(
  "#SBATCH --job-name=g_back \
  #SBATCH --partition=normal,highmem,geo \
  #SBATCH --ntasks=1 \
  #SBATCH --cpus-per-task=1 \
  #SBATCH --mem=200G \
  #SBATCH --error=slurm/backup_%j.out \
  module load R \
  set -euo pipefail \  
  {scriptlines_apptainer} exec ",
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
  #SBATCH --requeue \  
  #SBATCH --ntasks=1 \
  #SBATCH --cpus-per-task=1 \
  #SBATCH --mem=35G \
  #SBATCH --error=slurm/grid_%j.out \
  module load R \
  set -euo pipefail \
  export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK \
  {scriptlines_apptainer} exec --env OMP_NUM_THREADS=$OMP_NUM_THREADS ",
  "--bind {scriptlines_basedir}:/mnt ",
  "--bind {scriptlines_basedir}/inst:/inst ",
  "--bind {scriptlines_inputdir}:/input ",
  "--bind /run/munge:/run/munge ",
  "--bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm ",
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
  options_metrics = crew::crew_options_metrics(
    path = "pipeline/",
    seconds_interval = 1
  ),
  backup = controller_backup,
  tasks_max = 1L
)

#   #SBATCH --nodelist=cn040301,cn040609,cn030307,cn030309,cn030311 \

scriptlines_big_grid <- glue::glue(
  "#SBATCH --job-name=biggrid \
  #SBATCH --partition=normal,highmem \
  #SBATCH --requeue \  
  #SBATCH --ntasks=1 \
  #SBATCH --cpus-per-task=1 \
  #SBATCH --mem=50G \
  #SBATCH --error=slurm/bgrid_%j.out \
  module load R \
  set -euo pipefail \  
  export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK \
  {scriptlines_apptainer} exec --env OMP_NUM_THREADS=$OMP_NUM_THREADS ",
  "--bind {scriptlines_basedir}:/mnt ",
  "--bind {scriptlines_basedir}/inst:/inst ",
  "--bind {scriptlines_inputdir}:/input ",
  "--bind /run/munge:/run/munge ",
  "--bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm ",
  "--bind {scriptlines_targetdir}/targets:/opt/_targets ",
  "{scriptlines_container} \\"
)

controller_big_grid <- crew.cluster::crew_controller_slurm(
  name = "controller_big_grid",
  workers = 1000,
  crashes_max = 5L,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_big_grid
  ),
  options_metrics = crew::crew_options_metrics(
    path = "pipeline/",
    seconds_interval = 1
  ),
  backup = controller_backup,
  tasks_max = 1L
)


# if (targets::tar_active()) {
#   autometric::log_start(
#     path = "main_process.txt", # Statistics on the main process go here.
#     seconds = 1
#   )
# }

##############################        STORE       ##############################
targets::tar_config_set(store = "/opt/_targets")

##############################       OPTIONS      ##############################
if (Sys.getenv("BEETHOVEN") == "covariates") {
  beethoven_packages <- c(
    "amadeus",
    "targets",
    "tarchetypes",
    # "sqltargets",
    "dplyr",
    "tidyverse",
    "data.table",
    "sf",
    "crew",
    "crew.cluster",
    "mirai",
    "lubridate",
    "qs2",
    "kernlab",
    "DBI"
  )
} else {
  beethoven_packages <- c(
    "amadeus",
    "targets",
    "tarchetypes",
    # "sqltargets",
    "dplyr",
    "tidyverse",
    "data.table",
    "sf",
    "crew",
    "crew.cluster",
    "lubridate",
    "mirai",
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
    "h3",
    "h3r",
    "autometric"
  )
}
targets::tar_option_set(
  packages = beethoven_packages,
  repository = "local",
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
    controller_25,
    controller_10,
    controller_5,
    controller_1,
    controller_mlp,
    controller_lgb,
    controller_grid,
    controller_backup,
    controller_big_grid
  ),
  resources = targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "controller_250")
  ),
  retrieval = "worker"
)
sqltargets::sqltargets_option_set("sqltargets.template_engine", "jinjar")

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
            target_predict <- list()
} else if (Sys.getenv("BEETHOVEN") == "models") {
  target_predict <- list()
}


##############################      PIPELINE      ##############################
list(
  target_critical,
  target_initiate,
  target_download,
  target_aqs,
  target_calculate_fit,
  target_baselearner,
  target_baselearner_elnet,
  target_baselearner_mlp,
  target_baselearner_lgb,
  target_metalearner,
  target_calculate_predict
  # target_predict
)
