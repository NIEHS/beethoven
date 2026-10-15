################################################################################
# Merge all calculated covariates at Uber H3 (resolution 8) locations.

################################################################################
# Load libraries and source local functions.
library(amadeus)
source("R/beethoven.R")

################################################################################
# Import configured variables and inputs.
chr_pred_h3r8 <- qs2::qs_read(snakemake@input[["chr_pred_h3r8"]])
dt_pred_narr_sptmpl <- qs2::qs_read(snakemake@input[["dt_pred_narr_sptmpl"]])
dt_pred_hms_sptmpl <- qs2::qs_read(snakemake@input[["dt_pred_hms_sptmpl"]])

################################################################################
list_pred_merge_sptmpl <- lapply(
  list(
    dt_pred_narr_sptmpl,
    dt_pred_hms_sptmpl
  ),
  \(x) x[, time := as.character(time)]
)

################################################################################
dt_pred_merge_sptmpl <- reduce_merge(
  list_pred_merge_sptmpl,
  by = c(chr_pred_h3r8, "time")
)

################################################################################
qs2::qs_save(dt_pred_merge_sptmpl, snakemake@output[["dt_pred_merge_sptmpl"]])
