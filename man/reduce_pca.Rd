% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_postprocessing.R
\name{reduce_pca}
\alias{reduce_pca}
\title{Perform Principal Component Analysis}
\usage{
reduce_pca(data, num_comp = 5, threshold = NA, kernel = FALSE)
}
\arguments{
\item{data}{data.frame or data.table}

\item{num_comp}{integer(1). The number of components to retain as new
predictors. If \code{threshold} is defined, \code{num_comp} will be overridden.}

\item{threshold}{numeric(1). A fraction of the total variance that should
be covered by the components.}

\item{kernel}{logical(1). Whether to use a kernel PCA with
\code{\link[recipes:step_kpca]{recipes::step_kpca()}}. Default is \code{FALSE}.}
}
\value{
data.table with Principal Components sufficient to satisfy the
\code{threshold}.`
}
\description{
Perform Principal Component Analysis
}
\seealso{
\code{\link[recipes:step_pca]{recipes::step_pca()}} \code{\link[recipes:step_kpca]{recipes::step_kpca()}}
}
\keyword{internal}
