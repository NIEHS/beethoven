library(targets)
library(tarchetypes)
library(dplyr)
library(crew)
library(future)
library(future.batchtools)
library(dplyr)
library(beethoven, lib.loc = "/ddn/gs1/tools/set/R432/lib64/R/library")
library(tidymodels)
library(bonsai)
# library(torch)

Sys.setenv("LD_LIBRARY_PATH" = paste("/ddn/gs1/set/R432/lib64/R/lib", Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))

# replacing yaml file.
tar_config_set(
  store = "/opt/_targets"
)

# crew contollers
# For now, one is set, but we can explore the use of multiple controllers
# Can also explore making the workers input for bash script or Rscript
geo_controller <- crew_controller_local(
  name = "geo_controller",
  workers = 16L,
  launch_max = 8L,
  seconds_idle = 120
)



# Setting up the NASA Earthdata token inside the container
# This needs to be tested
if (!nzchar(Sys.getenv("NASA_EARTHDATA_TOKEN"))){
  tar_source("/mnt/NASA_token_setup.R")
  file.exists(".netrc")
  file.exists(".urs_cookies")
  file.exists(".dodsrc")
}


arglist_download <-
  set_args_download(
    char_period = c("2018-01-01", "2022-12-31"),
    char_input_dir = "/input",
    nasa_earth_data_token = Sys.getenv("NASA_EARTHDATA_TOKEN"),
    mod06_filelist = "/pipeline/targets/mod06_links_2018_2022.csv",
    export = TRUE,
    path_export = "/pipeline/targets/download_spec.qs"
  )






### NOTE: It is important to source the scipts after the global variables are defined from the set_args functions
 #tar_source("/pipeline/targets/targets_aqs.R")
 tar_source("/pipeline/targets/targets_download.R")

# Toy test files - note we will not have functions defined like this directly in
# the _targets.R file
my_fun_a <- function(n) {
  rnorm(n)
}

my_fun_b <- function(x) {
  x^2
}




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
  library = c("/ddn/gs1/tools/set/R432/lib64/R/library"),
  repository = "local",
  error = "abridge",
  memory = "transient",
  format = "qs",
  storage = "worker",
  deployment = "worker",
  garbage_collection = TRUE,
  seed = 202401L
)

  list(
    tar_target(name = A, command = my_fun_a(100)),
    tar_target(name = B, command = my_fun_b(A), pattern = A),
    tar_target(name = save_input, command = saveRDS(B, "/input/input.rds")),
    tar_target( # Test download data with amadeus
      download_test,
      amadeus::download_narr(
      variables = c("weasd", "omega"),
      year = c(2023, 2023),
      directory_to_save = "/input/narr_monolevel",
      acknowledgement = TRUE,
      download = TRUE, 
      remove_command = TRUE
    )
  ),
   target_download
  )


# Style below that uses sources scripts for targets by pipeline step
# Note that variables created in _targets.R are in the same local
# environment as the sourced scripts

# list(
#   target_init,
#   target_download
  # target_calculate_fit,
  # target_baselearner#,
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
# )

# targets::tar_visnetwork(targets_only = TRUE)
# END OF FILE

# list(
#   target_init,
#   target_download,
#   target_calculate_fit,
#   target_baselearner,
#   target_metalearner,
#   target_calculate_predict#,
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
