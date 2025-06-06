% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_postprocessing.R
\name{post_calc_year_expand}
\alias{post_calc_year_expand}
\title{Map the available raw data years over the given period}
\usage{
post_calc_year_expand(
  time_start = NULL,
  time_end = NULL,
  time_unit = "year",
  time_available = NULL
)
}
\arguments{
\item{time_start}{integer(1). Starting year.}

\item{time_end}{integer(1). Ending year.}

\item{time_unit}{character(1). Time unit. Default is \code{"year"}.}

\item{time_available}{vector. Available years.}
}
\value{
integer vector of length (time_end - time_start + 1).
Each element will get the nearest preceeding available year.
}
\description{
Many raw datasets are periodically updated and the period could
be longer than a year. This function maps the available years
over the given period.
}
\note{
The minimum of \code{time_available} will be filled in front of the first
available year when the minimum of \code{time_available} is greater
than \code{time_start}.
}
\examples{
\dontrun{
process_calc_year_expand(2018, 2022, "year", c(2017, 2020, 2021))
process_calc_year_expand(2018, 2022, "year", c(2020, 2021))
}
}
\keyword{Post-calculation}
