if (!require(targets)) {
  pak::pak("targets")
  library(targets)
}

source("./tools/pipeline/pipeline_base_functions.R")
source("./tools/pipeline/targets_initialize.R")
source("./tools/pipeline/targets_download.R")
source("./tools/pipeline/targets_calculate.R")
source("./tools/pipeline/targets_baselearner.R")
source("./tools/pipeline/targets_metalearner.R")
source("./tools/pipeline/targets_predict.R")

# bypass option
Sys.setenv("BTV_DOWNLOAD_PASS" = "FALSE")

# nullify download target if bypass option is set
if (Sys.getenv("BTV_DOWNLOAD_PASS") == "TRUE") {
  target_download <- NULL
}

# targets options
tar_option_set(
  packages =
    c("beethoven", "amadeus", "chopin",
      "data.table", "sf", "terra", "exactextractr",
      "crew", "crew.cluster",
      "future", "future.apply", "future.callr",
      "sftime", "stars", "rlang", "foreach", "parallelly"),
  repository = "local",
  controller =
  crew.cluster::crew_controller_slurm(
    slurm_log_output = "output/slurm_pipeline_log.out",
    slurm_log_error = "output/slurm_pipeline_error.err",
    tasks_max = 32L,
    slurm_memory_gigabytes_per_cpu = 8,
    slurm_cpus_per_task = 8L,
    slurm_time_minutes = NULL,
    slurm_partition = "geo"
  ),
  error = "null",
  memory = "persistent",
  storage = "worker",
  seed = 202401L
)

list(
  target_init,
  targets::tar_target(
    radii,
    command = c(1e3, 1e4, 5e4),
    iteration = "vector"
  ),
  target_download,
  target_calculate_fit,
  target_calculate_predict,
  target_baselearner,
  target_metalearner,
  target_predict,
  # documents and summary statistics
  targets::tar_target(
    summary_urban_rural,
    summary_prediction(
      grid_filled,
      level = "point",
      contrast = "urbanrural"))
  ,
  targets::tar_target(
    summary_state,
    summary_prediction(
      grid_filled,
      level = "point",
      contrast = "state"
    )
  )
)

# targets::tar_visnetwork()
# END OF FILE
