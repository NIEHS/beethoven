################################################################################
##############################      REFACTOR      ##############################
##### Development work to refactor the pipeline for non-injected targets.

################################################################################
##### libraries
library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)
library(beethoven)
library(dplyr)
library(tidymodels)
library(bonsai)

################################################################################
##### environmental variables
Sys.setenv(
  "LD_LIBRARY_PATH" = paste(
    "/ddn/gs1/tools/set/R432/lib64/R/lib",
    Sys.getenv("LD_LIBRARY_PATH"),
    sep = ":"
  )
)

################################################################################
##### skip download targets
Sys.setenv("BTV_DOWNLOAD_PASS" = "TRUE")

################################################################################
##############################      TARGETS      ###############################
##### controllers
# nolint start
script_lines <- paste0("
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=manwareme@nih.gov

export PATH=/ddn/gs1/tools/set/R432/bin/R:/ddn/gs1/tools/cuda11.8/bin:$PATH
export LD_LIBRARY_PATH=/ddn/gs1/tools/set/R432/lib64/R/lib:/ddn/gs1/tools/cuda11.8/lib64:$LD_LIBRARY_PATH
export R_LIBS_USER=/ddn/gs1/tools/set/R432/lib64/R/library:$R_LIBS_USER

module load /ddn/gs1/tools/set/R432/bin/R
"
)
# nolint end

default_controller <- crew.cluster::crew_controller_slurm(
  name = "default_controller",
  workers = 4,
  seconds_idle = 30,
  slurm_partition = "geo",
  slurm_memory_gigabytes_per_cpu = 4,
  slurm_cpus_per_task = 2,
  script_lines = script_lines
)
calc_controller <- crew.cluster::crew_controller_slurm(
  name = "calc_controller",
  workers = 32,
  seconds_idle = 30,
  slurm_partition = "geo",
  slurm_memory_gigabytes_per_cpu = 8,
  slurm_cpus_per_task = 2,
  script_lines = script_lines
)
nasa_controller <- crew.cluster::crew_controller_slurm(
  name = "nasa_controller",
  workers = 16,
  seconds_idle = 30,
  slurm_partition = "geo",
  slurm_memory_gigabytes_per_cpu = 4,
  slurm_cpus_per_task = 8,
  script_lines = script_lines
)
highmem_controller <- crew.cluster::crew_controller_slurm(
  name = "highmem_controller",
  workers = 1,
  seconds_idle = 30,
  slurm_partition = "geo",
  slurm_memory_gigabytes_per_cpu = 64,
  slurm_cpus_per_task = 2,
  script_lines = script_lines,
  launch_max = 10
)

################################################################################
##### store
targets::tar_config_set(
  store = "/ddn/gs1/home/manwareme/beethoven/beethoven_refactor"
)

################################################################################
##### options
targets::tar_option_set(
  packages = c(
    "beethoven", "targets", "tarchetypes", "dplyr",
    "data.table", "sf", "crew", "crew.cluster"
  ),
  library = c("/ddn/gs1/tools/set/R432/lib64/R/library"),
  repository = "local",
  error = "continue",
  memory = "transient",
  format = "qs",
  storage = "worker",
  deployment = "worker",
  garbage_collection = TRUE,
  seed = 202401L,
  controller = crew_controller_group(
    default_controller,
    calc_controller,
    nasa_controller,
    highmem_controller
  ),
  resources = tar_resources(
    crew = tar_resources_crew(
      controller = "default_controller"
    )
  )
)

################################################################################
##### source
targets::tar_source("inst/targets_arglist.R")
targets::tar_source("inst/targets_aqs.R")
targets::tar_source("inst/targets_download.R")
targets::tar_source("inst/targets_calculate_fit.R")

if (Sys.getenv("BTV_DOWNLOAD_PASS") == "TRUE") {
  target_download <- NULL
}

################################################################################
##### pipeline
list(
  target_arglist,
  target_aqs,
  target_download,
  target_calculate_fit
)
