library(targets)
library(tarchetypes)
library(future)
library(future.batchtools)

tar_source("inst/targets/pipeline_base_functions.R")
tar_source("inst/targets/targets_initialize.R")
tar_source("inst/targets/targets_download.R")
tar_source("inst/targets/targets_calculate.R")
tar_source("inst/targets/targets_baselearner.R")
tar_source("inst/targets/targets_metalearner.R")
tar_source("inst/targets/targets_predict.R")
tar_source("inst/targets/targets_arglist.R")

# bypass option
Sys.setenv("BTV_DOWNLOAD_PASS" = "TRUE")

plan(
  list(
    tweak(
      future.batchtools::batchtools_slurm,
      template = "inst/targets/template_slurm.tmpl",
      resources =
        list(
          memory = 8,
          log.file = "slurm_run.log",
          ncpus = 1, partition = "geo", ntasks = 1,
          email = arglist_common$user_email,
          error.file = "slurm_error.log"
        )
    ),
    multicore
  )
)

# # invalidate any nodes older than 180 days: force running the pipeline
# tar_invalidate(any_of(tar_older(Sys.time() - as.difftime(180, units = "days"))))


# # nullify download target if bypass option is set
if (Sys.getenv("BTV_DOWNLOAD_PASS") == "TRUE") {
  target_download <- NULL
}

# targets options
# TODO: check if the controller and resources setting are required
tar_option_set(
  packages =
    c("amadeus", "chopin", "targets", "tarchetypes",
      "data.table", "sf", "terra", "exactextractr",
      #"crew", "crew.cluster", 
      "tigris", "dplyr",
      "future.batchtools", "qs",
      "future", "future.apply", "future.callr", "callr",
      #"sftime",
      "stars", "rlang", "foreach", "parallelly"),
  library = "~/r-libs",
  repository = "local",
  error = "continue",
  # controller = 
  #   crew.cluster::crew_controller_slurm(
  #     slurm_log_output = "output/slurm_pipeline_log.out",
  #     slurm_log_error = "output/slurm_pipeline_error.err",
  #     script_directory = "output/slurm_scripts",
  #     workers = 50L,
  #     tasks_max = 50L,
  #     slurm_memory_gigabytes_per_cpu = 12,
  #     slurm_cpus_per_task = 8L,
  #     slurm_time_minutes = NULL,
  #     slurm_partition = "geo"
  #   ),
  resources = tar_resources(
    future = tar_resources_future(
      plan =
        tweak(
          future.batchtools::batchtools_slurm,
          template = "inst/targets/template_slurm.tmpl",
          resources =
            list(
              memory = 10,
              log.file = "slurm_run.log",
              ncpus = 1, partition = "geo", ntasks = 1,
              email = arglist_common$user_email,
              error.file = "slurm_error.log"
            )
        )
    )
  ),
  memory = "persistent",
  format = "qs",
  storage = "main",
  deployment = "worker",
  garbage_collection = TRUE,
  seed = 202401L
)

# should run tar_make_future()

list(
  target_init,
  # targets::tar_target(
  #   int_feat_calc_radii,
  #   command = c(1e3, 1e4, 5e4),
  #   iteration = "vector"
  # ),
  target_download,
  target_calculate_fit#,
  # target_baselearner,
  # target_metalearner,
  # target_calculate_predict,
  # target_predict,
  # # documents and summary statistics
  # targets::tar_target(
  #   summary_urban_rural,
  #   summary_prediction(
  #     grid_filled,
  #     level = "point",
  #     contrast = "urbanrural"))
  # ,
  # targets::tar_target(
  #   summary_state,
  #   summary_prediction(
  #     grid_filled,
  #     level = "point",
  #     contrast = "state"
  #   )
  # )
)

# targets::tar_visnetwork(targets_only = TRUE)
# END OF FILE
