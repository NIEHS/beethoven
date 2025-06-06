% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_postprocessing.R
\name{post_calc_pca}
\alias{post_calc_pca}
\title{Post-calculation Principal Component Analysis}
\usage{
post_calc_pca(
  data,
  locs_id = "site_id",
  time_id = "time",
  yvar = "Arithmetic.Mean",
  coords = c("lon", "lat"),
  num_comp = 5,
  threshold = NA,
  pattern = "FUGITIVE|STACK",
  groups = NULL,
  prefix = "PCA",
  kernel = FALSE
)
}
\arguments{
\item{data}{data.frame or data.table}

\item{locs_id}{The column name in the spatial object that represents the
location identifier.}

\item{time_id}{The column name in the data frame that represents the time
identifier.}

\item{yvar}{The target variable.}

\item{coords}{The column names that represent the XY coordinates. Default
is \code{c("lon", "lat")}.}

\item{num_comp}{integer(1). The number of components to retain as new
predictors.  If \code{threshold} is defined, \code{num_comp} will be overridden.}

\item{threshold}{numeric(1). A fraction of the total variance that should
be covered by the components.}

\item{pattern}{character(1). A regular expression pattern to match the
columns that should be included in the PCA.}

\item{groups}{character. A character vector of groups to perform PCA on.
Each character should be a regular expression pattern to match the columns
that should be included in the PCA. Default is \code{NULL}.}

\item{prefix}{character(1). A prefix to be added to the column names of the
Principal Components. Default is \code{NULL}.}

\item{kernel}{logical(1). Whether to use a kernel PCA with
\code{\link[recipes:step_kpca]{recipes::step_kpca()}}. Default is \code{FALSE}.}
}
\value{
data.table with Principal Components sufficient to satisfy the
\code{threshold}, merged with \verb{*_id} and \code{yvar} columns from original \code{data}.
}
\description{
This function performs PCA on the input data frame to reduce
number of predictors.
}
\note{
If \code{threshold} is defined, \code{num_comp} will be overridden.
}
\seealso{
\code{\link[=reduce_pca]{reduce_pca()}} \code{\link[recipes:step_pca]{recipes::step_pca()}} \code{\link[recipes:step_kpca]{recipes::step_kpca()}}
}
\keyword{Post-calculation}
