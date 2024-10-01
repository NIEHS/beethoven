################################################################################
############################      DEVELOPMENT      #############################
##### Development work to refactor the pipeline for non-injected targets.

################################################################################
##### libraries
library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)
library(beethoven)
library(dplyr)

Sys.setenv(
  "LD_LIBRARY_PATH" = paste(
    "/ddn/gs1/tools/set/R432/lib64/R/lib",
    Sys.getenv("LD_LIBRARY_PATH"),
    sep = ":"
  )
)

################################################################################
##### development functions
reduce_list_dt <- function(df_list) {
  grouped_dfs <- list()
  for (df in df_list) {
    col_names <- paste(sort(colnames(df)), collapse = ",")
    if (!col_names %in% names(grouped_dfs)) {
      grouped_dfs[[col_names]] <- list()
    }
    df <- as.data.table(df)
    grouped_dfs[[col_names]] <- append(grouped_dfs[[col_names]], list(df))
  }
  combined_list <- lapply(grouped_dfs, function(dfs) {
    rbindlist(dfs)  # More memory-efficient rbind for data.tables
  })
  names(combined_list) <- seq_along(combined_list)
  return(combined_list)
}


################################################################################
##### controllers
script_lines <- paste0("
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=manwareme@nih.gov

export PATH=/ddn/gs1/tools/set/R432/bin/R:/ddn/gs1/tools/cuda11.8/bin:$PATH
export LD_LIBRARY_PATH=/ddn/gs1/tools/set/R432/lib64/R/lib:/ddn/gs1/tools/cuda11.8/lib64:$LD_LIBRARY_PATH
export R_LIBS_USER=/ddn/gs1/tools/set/R432/lib64/R/library:$R_LIBS_USER

module load /ddn/gs1/tools/set/R432/bin/R
"
)

default_controller <- crew_controller_slurm(
  name = "default_controller",
  workers = 4,
  seconds_idle = 30,
  slurm_partition = "geo",
  slurm_memory_gigabytes_per_cpu = 4,
  slurm_cpus_per_task = 2,
  script_lines = script_lines
)
calc_controller <- crew_controller_slurm(
  name = "calc_controller",
  workers = 32,
  seconds_idle = 30,
  slurm_partition = "geo",
  slurm_memory_gigabytes_per_cpu = 8,
  slurm_cpus_per_task = 2,
  script_lines = script_lines
)
highmem_controller <- crew_controller_slurm(
  name = "highmem_controller",
  workers = 1,
  seconds_idle = 30,
  slurm_partition = "highmem",
  slurm_memory_gigabytes_per_cpu = 64,
  slurm_cpus_per_task = 2,
  script_lines = script_lines,
  launch_max = 10
)

################################################################################
##### targets options
tar_option_set(
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
    highmem_controller
  ),
  resources = tar_resources(
    crew = tar_resources_crew(
      controller = "default_controller"
    )
  )
)

################################################################################
##### pipeline
list(
  targets::tar_target(
    sf_feat_proc_aqs_sites,
    amadeus::process_aqs(
      export = FALSE,
      path = list.files(
        path = file.path(
          "/ddn/gs1/group/set/Projects/NRT-AP-Model/input",
          "aqs",
          "data_files"
        ),
        pattern = "daily_88101_[0-9]{4}.csv",
        full.names = TRUE
      ),
      date = c("2018-01-01", "2022-12-31"),
      mode = "location",
      return_format = "sf"
    ),
    description = "AQS sites"
  ),
  tar_target(
    sf_locs,
    command = sf::read_sf(
      system.file("shape/nc.shp", package = "sf")
    )
  ),
  tar_target(
    sf_grid,
    command = sf::st_as_sf(
      sf::st_sample(sf_locs, 200000)
    )
  ),
  tar_target(
    sf_sample,
    command = cbind(
      site_id = sprintf(
        "A_%06d",
        seq_len(nrow(sf_grid))
      ),
      sf_grid
    )
  ),
  tar_target(
    list_sample,
    command = base::split(
      sf_sample,
      ceiling(seq_len(nrow(sf_sample)) / 50000)
    )
  ),
  tar_target(
    chr_sample,
    command = names(list_sample)
  ),
  tar_target(
    list_dates,
    command = beethoven::split_dates(
      dates = c("2018-01-01", "2022-12-31"),
      n = 50
    )
  ),
  tar_target(
    chr_dates,
    command = names(list_dates)
  ),
  tar_target(
    list_split_geos,
    command = beethoven::inject_geos(
      locs = list_sample[[chr_sample]],
      injection = list(
        path = paste0(
          "/ddn/gs1/group/set/Projects/NRT-AP-Model/input/geos/",
          "aqc_tavg_1hr_g1440x721_v1"
        ),
        date = fl_dates(list_dates[[chr_dates]]),
        nthreads = 1
      )
    ),
    pattern = cross(chr_sample, chr_dates),
    iteration = "list",
    resources = tar_resources(
      crew = tar_resources_crew(
        controller = "calc_controller"
      )
    )
  ),
  targets::tar_target(
    list_geos,
    command = rbind(list_split_geos),
    resources = tar_resources(
      crew = tar_resources_crew(
        controller = "highmem_controller"
      )
    )
  )
  # targets::tar_target(
  #   dt_geos,
  #   command = reduce_merge(list_geos),
  #   resources = tar_resources(
  #     crew = tar_resources_crew(
  #       controller = "dt_controller"
  #     )
  #   )
  # )
  # tar_target(
  #   chr_narr,
  #   command = c("weasd", "air.sfc")
  # ),
  # tar_target(
  #   list_narr,
  #   command = beethoven::par_narr(
  #     domain = chr_narr,
  #     path = amadeus::download_sanitize_path(
  #       paste0(
  #         "/ddn/gs1/group/set/Projects/NRT-AP-Model/input/narr/",
  #         chr_narr
  #       )
  #     ),
  #     date = fl_dates(list_dates[[chr_dates]]),
  #     locs = sf_locs_scale,
  #     nthreads = 1
  #   ),
  #   pattern = cross(chr_dates, chr_narr),
  #   iteration = "list",
  #   resources = tar_resources(
  #     crew = tar_resources_crew(
  #       controller = "base_controller"
  #     )
  #   )
  # ),
  # targets::tar_target(
  #   dt_narr,
  #   command = reduce_merge(
  #   lapply(
  #     list(list_narr),
  #     function(x) reduce_merge(reduce_list(lapply(x, "[[", 1)))
  #   ),
  #   by = c("site_id", "time")
  # ),
  #   resources = tar_resources(
  #     crew = tar_resources_crew(
  #       controller = "dt_controller"
  #     )
  #   )
  # ),
  # targets::tar_target(
  #   chr_nasa,
  #   command = c(
  #     "mod11"#, "mod06", "mod13", "mcd19_1km", "mcd19_5km", "mod09", "viirs"
  #   )
  # ),
  # targets::tar_target(
  #   list_nasa,
  #   command = beethoven::inject_modis_par(
  #     locs = sf_locs_scale,
  #     injection = loadargs("./calc_spec.qs", chr_nasa)
  #   ),
  #   pattern = map(chr_nasa),
  #   iteration = "list",
  #   resources = tar_resources(
  #     crew = tar_resources_crew(
  #       controller = "nasa_controller"
  #     )
  #   )
  # ),
  # targets::tar_target(
  #   dt_nasa,
  #   command = reduce_merge(reduce_list(list_nasa))
  # ),
  # targets::tar_target(
  #   dt_pred,
  #   command = {
  #     Sys.sleep(60)
  #     Reduce(
  #       post_calc_autojoin,
  #       list(
  #         dt_geos,
  #         dt_narr
  #         # dt_nasa
  #       )
  #     )
  #   },
  #   resources = tar_resources(
  #     crew = tar_resources_crew(
  #       controller = "dt_controller"
  #     )
  #   )
  # )
)
