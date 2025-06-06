% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_postprocessing.R
\name{append_predecessors}
\alias{append_predecessors}
\title{Append Predecessors}
\usage{
append_predecessors(
  path_qs = "output/qs",
  period_new = NULL,
  input_new = NULL,
  nthreads = 8L
)
}
\arguments{
\item{path_qs}{The path where the predecessors will be stored.}

\item{period_new}{The new period to be appended.}

\item{input_new}{The new input object to be appended.}

\item{nthreads}{The number of threads to be used.}
}
\value{
If no existing predecessors are found, the function saves
the new input object and returns the name of the saved file.
If existing predecessors are found, the function appends
the new input object to the existing ones and returns the combined object.
}
\description{
This function appends predecessors to an existing object or
creates a new object if none exists.
}
\keyword{Post-calculation}
