% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_postprocessing.R
\name{post_calc_cols}
\alias{post_calc_cols}
\title{Post-calculation column renaming}
\usage{
post_calc_cols(data, prefix = NULL, skip = c("site_id", "time"))
}
\arguments{
\item{data}{data.frame(1)}

\item{prefix}{character(1). The prefix to be added to the column names.}

\item{skip}{character. The column names to be skipped from renaming.
Default is \code{c("site_id", "time")}.}
}
\value{
data.frame with renamed columns.
}
\description{
This function renames the columns of the input \code{data} based on
the \code{prefix} and original column names.
}
\keyword{Post-calculation}
