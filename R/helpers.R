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

cancel <- function() {
  system("scancel -u $USER")
}

batch <- function(file = "run.sh") {
  system(paste0("sbatch ", file))
}

gpu <- function() {
  system("nvidia-smi")
}

#' Run all tests within a single file from `tests/testthat/` directory
#' with the `container_models.sif` container.
#' @param pattern A regular expression to match the test file name.
#' @return NULL; Prints the output of the testthat tests.
#' @seealso [testthat::test_file()]
test <- function(pattern = NULL) {
  if (is.null(pattern)) stop()
  system(
    glue::glue(
      "apptainer exec --nv --bind $PWD:/mnt --bind /tmp:/opt/tmp ",
      "container_models.sif Rscript --no-init-file -e \"",
      ".libPaths(grep(paste0('biotools|', Sys.getenv('USER')), .libPaths(), ",
      "value = TRUE, invert = TRUE)); library(beethoven); library(bonsai); ",
      "library(dplyr); library(testthat); ",
      "test_file <- list.files('/mnt/tests/testthat', full.names = TRUE, ",
      "pattern = '", pattern, "'); source_files <- list.files('/mnt/R', ",
      "full.names = TRUE); testthat::test_file(test_file)\""
    )
  )
}

#' Calculate code coverage of the {beethoven} package with the
#' `container_models.sif` container.
#' @return NULL; Prints the output of the code coverage.
#' @seealso [covr::package_coverage()]; [covr::coverage_to_list()]
cov <- function() {
  system(
    glue::glue(
      "apptainer exec --nv --bind $PWD:/mnt --bind /tmp:/opt/tmp ",
      "container_models.sif Rscript --no-init-file -e \"",
      ".libPaths(grep(paste0('biotools|', Sys.getenv('USER')), .libPaths(), ",
      "value = TRUE, invert = TRUE)); library(beethoven); library(bonsai); ",
      "library(dplyr); library(testthat); ",
      "cov <- covr::package_coverage(install_path = '/tmp/cov', ",
      "clean = FALSE); covr::coverage_to_list(cov)\""
    )
  )
}
# nocov end
