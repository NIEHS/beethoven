## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(message = FALSE)
library(knitr)

tab_feature <-
  read.csv(
    system.file(
      "extdata",
      "beethoven_covariate_list.csv",
      package = "beethoven"
    )
  )

## -----------------------------------------------------------------------------
knitr::kable(
  tab_feature
)

