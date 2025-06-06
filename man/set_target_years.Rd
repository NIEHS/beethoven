% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/injection.R
\name{set_target_years}
\alias{set_target_years}
\title{Set which years to be processed}
\usage{
set_target_years(period = NULL, available = NULL)
}
\arguments{
\item{period}{character(2)/integer(2) of integer/character/Date.}

\item{available}{vector of integer or Date. Available years to be processed.}
}
\value{
A vector of years to be processed.
}
\description{
Set which years to be processed
}
\note{
This function is designed to define the temporal domain
from the calculation period and the available years of raw data.
}
\keyword{Utility}
