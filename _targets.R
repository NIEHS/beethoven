library(targets)
library(tarchetypes)
library(future)
library(future.batchtools)
library(dplyr)
library(
  beethoven,
  lib.loc = "/ddn/gs1/home/manwareme/R/x86_64-pc-linux-gnu-library/4.3"
)
library(tidymodels)
library(bonsai)
# library(
#   torch,
#   lib.loc = "/ddn/gs1/biotools/R/lib64/R/library"
# )

Sys.setenv("LD_LIBRARY_PATH" = paste("/ddn/gs1/biotools/R/lib64/R/customlib", Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))

# replacing yaml file.
tar_config_set(
  store = "/ddn/gs1/home/manwareme/beethoven/beethoven_targets"
)

# maximum future exportable object size is set 50GB
# TODO: the maximum size error did not appear until recently
#       and suddenly appeared. Need to investigate the cause.
#       Should be removed after the investigation.
# options(future.globals.maxSize = 50 * 2^30)
options(future.globals.maxSize = 60 * 1024^3)  # 60 GiB


generate_list_download <- FALSE

arglist_download <-
  set_args_download(
    char_period = c("2018-01-01", "2022-12-31"),
    char_input_dir = "input",
    nasa_earth_data_token = NULL,#Sys.getenv("NASA_EARTHDATA_TOKEN"),
    mod06_filelist = "inst/targets/mod06_links_2018_2022.csv",
    export = generate_list_download,
    path_export = "inst/targets/download_spec.qs"
  )

generate_list_calc <- FALSE

arglist_common <-
  set_args_calc(
    char_siteid = "site_id",
    char_timeid = "time",
    char_period = c("2018-01-01", "2022-12-31"),
    num_extent = c(-126, -62, 22, 52),
    char_user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
    export = generate_list_calc,
    path_export = "inst/targets/calc_spec.qs",
    char_input_dir = "/ddn/gs1/group/set/Projects/NRT-AP-Model/input"
  )

tar_source("inst/targets/targets_initialize.R")
tar_source("inst/targets/targets_download.R")
tar_source("inst/targets/targets_calculate_fit.R")
tar_source("inst/targets/targets_calculate_predict.R")
tar_source("inst/targets/targets_baselearner.R")
tar_source("inst/targets/targets_metalearner.R")
tar_source("inst/targets/targets_predict.R")


# bypass option
Sys.setenv("BTV_DOWNLOAD_PASS" = "TRUE")

#
# bind custom built GDAL
# Users should export the right path to the GDAL library
# by export LD_LIBRARY_PATH=.... command.

# arglist_common is generated above
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
          email = arglist_common$char_user_email,
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
# For GPU support, users should be aware of setting environment
# variables and GPU versions of the packages.
# TODO: check if the controller and resources setting are required
tar_option_set(
  packages = c(
    "beethoven", "amadeus", "chopin", "targets", "tarchetypes",
    "data.table", "sf", "terra", "exactextractr",
    #"crew", "crew.cluster", 
    "tigris", "dplyr",
    "future.batchtools", "qs", "collapse", "bonsai",
    "tidymodels", "tune", "rsample", "torch", "brulee",
    "glmnet", "xgboost",
    "future", "future.apply", "future.callr", "callr",
    "stars", "rlang", "parallelly"
  ),
  library = c("/ddn/gs1/group/set/isong-archive/r-libs"),
  repository = "local",
  error = "abridge",
  memory = "transient",
  format = "qs",
  storage = "worker",
  deployment = "worker",
  garbage_collection = TRUE,
  seed = 202401L
)

# should run tar_make_future()

list(
  target_init,
  target_download,
  target_calculate_fit,
  target_baselearner,
  target_metalearner,
  target_calculate_predict#,
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
