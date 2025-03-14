% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_learner.R
\name{assign_learner_cv}
\alias{assign_learner_cv}
\title{Shuffle cross-validation mode for each learner type}
\usage{
assign_learner_cv(
  learner = c("lgb", "mlp", "elnet"),
  cv_mode = c("spatiotemporal", "spatial", "temporal"),
  cv_rep = 100L,
  num_device = ifelse(torch::cuda_device_count() > 1, 2, 1),
  balance = FALSE
)
}
\arguments{
\item{learner}{character(1). The base learner to be used.
Default is "mlp". Available options are "mlp", "lgb", "elnet".}

\item{cv_mode}{character(1). The cross-validation mode to be used.
Default is "spatiotemporal". Available options are "spatiotemporal",
"spatial", "temporal".}

\item{cv_rep}{integer(1). The number of repetitions for each \code{cv_mode}.}

\item{num_device}{integer(1). The number of CUDA devices to be used.
Each device will be assigned to each eligible learner (i.e., lgb, mlp).}

\item{balance}{logical(1). If TRUE, the number of CUDA devices will be
equally distributed based on the number of eligible devices.}
}
\value{
A data frame with three columns: learner, cv_mode, and device.
}
\description{
Shuffle cross-validation mode for each learner type
}
\keyword{Baselearner}
