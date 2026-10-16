################################################################################
# Merge all calculated covariates at unique AQS locations, fill time.

################################################################################
# Load libraries and source local functions.
library(beethoven)

################################################################################
# Import configured variables and inputs.
dt_feat_aqs_sptmpl <- qs2::qs_read(snakemake@input[["dt_feat_aqs_sptmpl"]])
dt_feat_narr_sptmpl <- qs2::qs_read(snakemake@input[["dt_feat_narr_sptmpl"]])
dt_feat_hms_sptmpl <- qs2::qs_read(snakemake@input[["dt_feat_hms_sptmpl"]])

################################################################################
list_feat_merge_sptmpl <- lapply(
  list(
    dt_feat_aqs_sptmpl,
    dt_feat_narr_sptmpl,
    dt_feat_hms_sptmpl
  ),
  \(x) x[, time := as.character(time)]
)

################################################################################
dt_feat_merge_sptmpl <- beethoven::reduce_merge(
  list_feat_merge_sptmpl,
  by = c("site_id", "time")
)

################################################################################
# Substitute "_" for "." in column names for easier SQL integration.
names(dt_feat_merge_sptmpl) <- gsub("\\.", "_", names(dt_feat_merge_sptmpl))

################################################################################
arrow::write_parquet(
  dt_feat_merge_sptmpl,
  snakemake@output[["dt_feat_merge_sptmpl"]]
)
