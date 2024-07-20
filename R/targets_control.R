# nocov start


#' Set resource management for SLURM
#' @keywords Utility
#' @param template_file SLURM job submission shell template path.
#' @param partition character(1). Name of partition. Default is `"geo"`
#' @param ncpus integer(1). Number of CPU cores assigned to each task.
#' @param ntasks integer(1). Number of tasks to submit.
#' @param memory integer(1). Specifically odds to 2*x GB.
#' @param user_email character(1). User email address.
#' @param error_log character(1). Error log file name.
#' @note This function is designed to be used with `tar_resources`.
#' Suggested number of `ncpus` is more than 1 for typical multicore R tasks.
#' @returns A list of resources for `tar_resources`
#' @author Insang Song
#' @importFrom future tweak
#' @importFrom future.batchtools batchtools_slurm
#' @importFrom targets tar_resources
#' @importFrom targets tar_resources_future
#' @export
set_slurm_resource <-
  function(
    template_file = "inst/targets/template_slurm.tmpl",
    partition = "geo",
    ncpus = 2L,
    ntasks = 2L,
    memory = 8,
    user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
    error_log = "slurm_error.log"
  ) {
    targets::tar_resources(
      future = targets::tar_resources_future(
        plan = future::tweak(
          future.batchtools::batchtools_slurm,
          template = template_file,
          resources =
            list(
              partition = partition,
              ntasks = ntasks,
              ncpus = ncpus,
              memory = memory,
              email = user_email,
              error.file = error_log
            )
        )
      )
    )
  }


# nocov end
