# Helper functions for checking SLURM jobs and nodes

# nocov start
job <- function(job_id) {
  system(
    paste0("sacct -j ", job_id, " --format=JobID,Elapsed,TotalCPU,MaxRSS")
  )
}

kb_to_gb <- function(kb) {
  gb <- kb / (1024^2)
  return(gb)
}

geo <- function() {
  system("srun --partition=geo --cpus-per-task=1 --pty top")
}

node <- function(node = "gn040815") {
  system(paste0("scontrol show node ", node))
}

queue <- function() {
  system("squeue -u $USER")
}

#' Cancel current workflow.
#' @param job character(1). SLURM job number or "full", to cancel all jobs
#' running on `$USER` SLURM profile.
#' @return NULL; cancels jobs.
#' @keywords Miscellaneous
#' @export
cancel <- function(job = NULL) {
  stopifnot(!is.null(job))
  if (job == "full") {
    system("scancel -u $USER")
  } else {
    system(paste0("scancel ", job))
  }
}

batch <- function(file = "run.sh") {
  system(paste0("sbatch ", file))
}

clean <- function(pattern = NULL) {
  system(paste0("rm slurm/*"))
}

gpu <- function() {
  system("nvidia-smi")
}
# nocov end

#' Run all tests within a single file from `tests/testthat/` directory
#' with the `container_models.sif` container.
#' @param pattern A regular expression to match the test file name.
#' @return NULL; Prints the output of the `testthat` tests.
#' @seealso [testthat::test_file()]
#' @keywords Miscellaneous
test <- function(pattern = NULL) {
  if (is.null(pattern)) {
    stop()
  }
  system(
    paste(
      c(
        "apptainer exec --nv --bind $PWD:/mnt --bind /tmp:/opt/tmp ",
        "container_models.sif Rscript --no-init-file -e \"",
        ".libPaths(grep(paste0('biotools|', Sys.getenv('USER')), .libPaths(), ",
        "value = TRUE, invert = TRUE)); devtools::load_all('/mnt'); ",
        "library(bonsai); library(dplyr); library(testthat); ",
        "test_file <- list.files('/mnt/tests/testthat', full.names = TRUE, ",
        "pattern = '",
        pattern,
        "'); source_files <- list.files('/mnt/R', ",
        "full.names = TRUE); covr::file_coverage(source_files, test_file)\""
      ),
      collapse = ""
    )
  )
}

#' Calculate code coverage of the `beethoven` package with the
#' `container_models.sif` container.
#' @return NULL; Prints the output of the code coverage.
#' @seealso [covr::package_coverage()]; [covr::coverage_to_list()]
#' @keywords Miscellaneous
cov <- function() {
  system(
    paste(
      c(
        "apptainer exec --nv --bind $PWD:/mnt --bind /tmp:/opt/tmp ",
        "container_models.sif Rscript --no-init-file -e \"",
        ".libPaths(grep(paste0('biotools|', Sys.getenv('USER')), .libPaths(), ",
        "value = TRUE, invert = TRUE)); devtools::load_all('/mnt'); ",
        "library(bonsai); library(dplyr); library(testthat); ",
        "cov <- covr::package_coverage(install_path = '/tmp/cov', ",
        "clean = FALSE); ",
        "saveRDS(cov, '/mnt/cov/cov_",
        format(Sys.time(), "%m%d_%H%M"),
        ".rds'); covr::coverage_to_list(cov)\""
      ),
      collapse = ""
    )
  )
}

#' Open interactive session with `container.sif` container.
#' @param dir character(1). Directory with `interactive.sh` file.
#' @return NULL
#' @keywords Miscellaneous
interactive <- function(dir = "./") {
  file <- file.path(dir, "interactive.sh")
  system(paste0(". ", file))
}
