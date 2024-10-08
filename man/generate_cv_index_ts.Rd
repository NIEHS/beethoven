% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_learner.R
\name{generate_cv_index_ts}
\alias{generate_cv_index_ts}
\title{Generate temporal cross-validation index}
\usage{
generate_cv_index_ts(data, time_col = "time", cv_fold = 10L, window = 14L)
}
\arguments{
\item{data}{data.table with X, Y, and time information.}

\item{time_col}{character(1). Field name with time information.}

\item{cv_fold}{integer(1). Number of cross-validation folds.}

\item{window}{integer(1). Window size for each fold.
Simply meaning overlaps between folds. Unit is
the base unit of temporal values stored in \code{time_col}.
Window size is put into \code{as.difftime} function, then the half of it
(if odd, rounded number + 1 is applied) is used for overlaps
in the middle folds.}
}
\value{
List of numeric vector with out-of-sample indices.
}
\description{
Generate temporal cross-validation index
}
\examples{
data <- data.frame(
 time = seq.Date(from = as.Date("2021-01-01"), by = "day", length.out = 100),
 value = rnorm(100)
)
rset_ts <-
  generate_cv_index_ts(data, time_col = "time", cv_fold = 10, window = 14)
}
\keyword{Baselearner}
