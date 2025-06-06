% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/injection.R
\name{inject_gmted}
\alias{inject_gmted}
\title{Injects GMTED data into specified locations}
\usage{
inject_gmted(locs, variable, radii, injection)
}
\arguments{
\item{locs}{A data frame/sf/SpatVector containing the locations
where GMTED variables needs to be calculated}

\item{variable}{The variable for which GMTED data needs to be calculated.}

\item{radii}{A vector of radii for which GMTED data needs
to be calculated.}

\item{injection}{A list of additional arguments to be passed to
the \code{calc_gmted_direct} function.}
}
\value{
A data frame containing the merged results of GMTED data
for each location within different radii.
}
\description{
This function injects GMTED (Global Multi-resolution Terrain Elevation Data)
into specified locations. It calculates the GMTED values for each
location within different radii and returns the merged results.
}
\keyword{Calculation}
