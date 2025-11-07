library(targets)
library(tidyverse)
library(crew)
library(crew.cluster)

################################################################################
##############################      BEETHOVEN      #############################
##### Main file controlling the settings, options, and sourcing of targets
##### for the beethoven analysis pipeline.

#############################      CONTROLLER      #############################

##### `controller_geo` uses 4 GPU workers (undefined memory allocation).
scriptlines_apptainer <- "apptainer"
scriptlines_container_covariates <- "container_covariates.sif"

scriptlines_geo <- glue::glue(
  "#SBATCH --job-name=geo \
  #SBATCH --partition=geo \
  #SBATCH --gres=gpu:1 \
  #SBATCH --error=slurm/geo_%j.out \
  #SBATCH --ntasks=1 \
  srun \
  apptainer exec --nv --env ",
  "--containall ",
  "--env R_LIBS='/opt/Rlibs' ",
  "--env R_LIBS_USER='/opt/Rlibs' ",
  "--env R_LIBS_SITE='/opt/Rlibs' ",
  "CUDA_VISIBLE_DEVICES=${{GPU_DEVICE_ORDINAL}} ",
  "--bind /ddn/gs1/home/messierkp/projects/beethoven:/mnt ",
  "--bind /ddn/gs1/home/messierkp/projects/beethoven/inst:/inst ",
  "--bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input ",
  "--bind /ddn/gs1/group/set/Projects/beethoven/targets:/opt/_targets ",
  "--bind /run/munge:/run/munge ",
  "--bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm ",
  "container_models.sif \\"
)


controller_geo <- crew.cluster::crew_controller_slurm(
  name = "controller_geo",
  workers = 4,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_geo
  ),
  tasks_max = Inf
)

##### `controller_lgb` uses 100 CPUs for {lightGBM} models.
scriptlines_gpu <- glue::glue(
  "#SBATCH --job-name=gpu \
  #SBATCH --partition=gpu \
  #SBATCH --nodelist=gn040809 \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=100G \
  #SBATCH --error=slurm/gpu_%j.out \
  export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK \
  export LIGHTGBM_NUM_THREADS=$SLURM_CPUS_PER_TASK \
  srun \
  apptainer exec --cleanenv --env OMP_NUM_THREADS=$OMP_NUM_THREADS ",
  "--containall ",
  "--env R_LIBS='/opt/Rlibs' ",
  "--env R_LIBS_USER='/opt/Rlibs' ",
  "--env R_LIBS_SITE='/opt/Rlibs' ",
  "--env LIGHTGBM_NUM_THREADS=$LIGHTGBM_NUM_THREADS ",
  "--bind /ddn/gs1/home/messierkp/projects/beethoven:/mnt ",
  "--bind /ddn/gs1/home/messierkp/projects/beethoven/inst:/inst ",
  "--bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input ",
  "--bind /ddn/gs1/group/set/Projects/beethoven/targets:/opt/_targets ",
  "--bind /run/munge:/run/munge ",
  "--bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm ",
  "container_models.sif \\"
)
controller_gpu <- crew.cluster::crew_controller_slurm(
  name = "controller_gpu",
  workers = 25,
  options_cluster = crew.cluster::crew_options_slurm(
    verbose = TRUE,
    script_lines = scriptlines_gpu
  ),
  tasks_max = Inf
)


#####

scriptlines_common <- glue::glue(
  "#SBATCH --partition=normal \
  #SBATCH --job-name=common \
  #SBATCH --ntasks=1 \
  srun \
  apptainer exec ",
  "--containall ",
  "--env R_LIBS='/opt/Rlibs' ",
  "--env R_LIBS_USER='/opt/Rlibs' ",
  "--env R_LIBS_SITE='/opt/Rlibs' ",
  "--bind /ddn/gs1/home/messierkp/projects/beethoven:/mnt ",
  "--bind /ddn/gs1/home/messierkp/projects/beethoven/inst:/inst ",
  "--bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input ",
  "--bind /ddn/gs1/group/set/Projects/beethoven/targets:/opt/_targets ",
  "--bind /run/munge:/run/munge ",
  "--bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm ",
  "container_models.sif \\"
)


#### mem for different tasks:
# regular grid covariates: 35 GB
# big grid covariates: 50 GB
# super big grid covariates: 100 GB

options_regular = crew.cluster::crew_options_slurm(
  verbose = TRUE,
  log_output = "slurm/regular_%j.out",
  log_error = "slurm/regular_%j.err",
  memory_gigabytes_required = 35,
  script_lines = scriptlines_common
)

options_big = crew.cluster::crew_options_slurm(
  verbose = TRUE,
  log_output = "slurm/big_%j.out",
  log_error = "slurm/big_%j.err",
  memory_gigabytes_required = 50,
  script_lines = scriptlines_common
)

options_super = crew.cluster::crew_options_slurm(
  verbose = TRUE,
  log_output = "slurm/super_%j.out",
  log_error = "slurm/super_%j.err",
  memory_gigabytes_required = 100,
  script_lines = scriptlines_common
)

controller_regular <- crew.cluster::crew_controller_slurm(
  name = "controller_regular",
  workers = 200,
  crashes_max = 5L,
  options_metrics = crew::crew_options_metrics(
    path = "pipeline/",
    seconds_interval = 1
  ),
  options_cluster = options_regular,
  tasks_max = Inf
)

controller_big <- crew.cluster::crew_controller_slurm(
  name = "controller_big",
  workers = 100,
  crashes_max = 5L,
  options_metrics = crew::crew_options_metrics(
    path = "pipeline/",
    seconds_interval = 1
  ),
  options_cluster = options_big,
  tasks_max = Inf
)

controller_super <- crew.cluster::crew_controller_slurm(
  name = "controller_super",
  workers = 50,
  crashes_max = 5L,
  options_metrics = crew::crew_options_metrics(
    path = "pipeline/",
    seconds_interval = 1
  ),
  options_cluster = options_super,
  tasks_max = Inf
)

# if (targets::tar_active()) {
#   autometric::log_start(
#     path = "main_process.txt", # Statistics on the main process go here.
#     seconds = 1
#   )
# }

##############################        STORE       ##############################
targets::tar_config_set(store = "/opt/_targets")
# targets::tar_config_set(
#   store = "/ddn/gs1/group/set/Projects/beethoven/targets/"
# )
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
  library = "/opt/Rlibs",
  error = "continue",
  memory = "auto",
  format = "qs",
  storage = "worker",
  deployment = "worker",
  garbage_collection = 10L,
  seed = 202401L,
  controller = crew::crew_controller_group(
    controller_geo,
    controller_gpu,
    controller_regular,
    controller_big,
    controller_super
  ),
  resources = targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "controller_regular")
  ),
  retrieval = "worker"
)
# sqltargets::sqltargets_option_set("sqltargets.template_engine", "jinjar")

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
  target_baselearner <-
    target_baselearner_elnet <-
      target_baselearner_lgb <-
        target_baselearner_mlp <-
          target_metalearner <-
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
