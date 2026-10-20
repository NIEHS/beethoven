################################################################################
# Implement spatiotemporal cross validation schema.

################################################################################
# Load libraries and source local functions.
library(beethoven)

################################################################################
# Import configured variables and inputs.
dt_feat_merge_sptmpl <- arrow::read_parquet(snakemake@input[[
  "dt_feat_merge_sptmpl"
]])
sf_feat_aqs_sp <- qs2::qs_read(snakemake@input[["sf_feat_aqs_sp"]])
int_base_iter <- snakemake@params[["int_base_iter"]]

################################################################################
# Define common parameters across all base learner models.
list_base_params_static <- list(
  data = dt_feat_merge_sptmpl,
  yvar = "Arithmetic_Mean",
  xvar = setdiff(
    names(dt_feat_merge_sptmpl),
    c("site_id", "time", "Event_Type", "Arithmetic_Mean")
  ),
  dvar = c("site_id", "time"),
  normalize = TRUE,
  nmodels = int_base_iter,
  metric = "mae",
  v = 5L,
  crs = 5070L,
  cellsize = 100000,
  buffer = 50000
)

################################################################################
# Project AQS site locations to meter-based coordinate reference system.
sf_feat_aqs_5070 <- sf::st_transform(
  sf_feat_aqs_sp,
  crs = list_base_params_static$crs
)

################################################################################
# Generate spatial v-fold rset objects with pre-defined cell sizes and buffers.
rset_base_aqs_sp <- spatialsample::spatial_block_cv(
  sf_feat_aqs_5070,
  v = list_base_params_static$v,
  repeats = list_base_params_static$nmodels,
  cellsize = list_base_params_static$cellsize,
  buffer = list_base_params_static$buffer,
  square = TRUE,
  radius = NULL
)

################################################################################
# Extract site_id per repeat and fold for pass to Spark in model train/test/fit.
list_base_cv_idx <- lapply(
  split(seq_len(nrow(rset_base_aqs_sp)), rset_base_aqs_sp$id),
  function(i) {
    stats::setNames(
      lapply(i, function(j) {
        split_j <- rset_base_aqs_sp$splits[[j]]
        list(
          analysis = rsample::analysis(split_j)$site_id,
          assessment = rsample::assessment(split_j)$site_id
        )
      }),
      rset_base_aqs_sp$id2[i]
    )
  }
)

################################################################################
qs2::qs_save(
  rlang::duplicate(list_base_params_static),
  snakemake@output[["list_base_params_static"]]
)
qs2::qs_save(
  list_base_cv_idx,
  snakemake@output[["list_base_cv_idx"]]
)
