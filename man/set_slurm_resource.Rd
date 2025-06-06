% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/targets_control.R
\name{set_slurm_resource}
\alias{set_slurm_resource}
\title{Set resource management for SLURM}
\usage{
set_slurm_resource(
  template_file = "inst/targets/template_slurm.tmpl",
  partition = "geo",
  ncpus = 2L,
  ntasks = 2L,
  memory = 8,
  user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
  error_log = "slurm_error.log"
)
}
\arguments{
\item{template_file}{SLURM job submission shell template path.}

\item{partition}{character(1). Name of partition. Default is \code{"geo"}}

\item{ncpus}{integer(1). Number of CPU cores assigned to each task.}

\item{ntasks}{integer(1). Number of tasks to submit.}

\item{memory}{integer(1). Specifically odds to 2*x GB.}

\item{user_email}{character(1). User email address.}

\item{error_log}{character(1). Error log file name.}
}
\value{
A list of resources for \code{tar_resources}
}
\description{
This function sets up resources for SLURM job submission.
Note that this function is designed to work with \code{tar_make_future()}
}
\note{
This function is designed to be used with \code{tar_resources}.
Suggested number of \code{ncpus} is more than 1 for typical multicore R tasks.
}
\author{
Insang Song
}
\keyword{Utility}
