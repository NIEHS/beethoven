################################################################################
# Implement spatiotemporal cross validation schema.

################################################################################
# Load libraries and source local functions.
library(beethoven)

################################################################################
# Import configured variables and inputs.
list_base_params_static <- qs2::qs_read(snakemake@input[[
  "list_base_params_static"
]])
sf_feat_aqs_sp <- qs2::qs_read(snakemake@input[["sf_feat_aqs_sp"]])
int_meta_iter <- snakemake@params[["int_meta_iter"]]

################################################################################
# Project AQS site locations to meter-based coordinate reference system.
sf_feat_aqs_5070 <- sf::st_transform(
  sf_feat_aqs_sp,
  crs = list_base_params_static$crs
)

################################################################################
# Generate spatial v-fold rset objects with pre-defined cell sizes and buffers.
rset_meta_aqs_sp <- spatialsample::spatial_block_cv(
  sf_feat_aqs_5070,
  v = list_base_params_static$v,
  repeats = int_meta_iter,
  cellsize = list_base_params_static$cellsize,
  buffer = list_base_params_static$buffer,
  square = TRUE,
  radius = NULL
)

################################################################################
# Extract site_id per repeat and fold for pass to Spark in model train/test/fit.
list_meta_cv_idx <- lapply(
  split(seq_len(nrow(rset_meta_aqs_sp)), rset_meta_aqs_sp$id),
  function(i) {
    stats::setNames(
      lapply(i, function(j) {
        split_j <- rset_meta_aqs_sp$splits[[j]]
        list(
          analysis = rsample::analysis(split_j)$site_id,
          assessment = rsample::assessment(split_j)$site_id
        )
      }),
      rset_meta_aqs_sp$id2[i]
    )
  }
)

################################################################################
qs2::qs_save(list_meta_cv_idx, snakemake@output[["list_meta_cv_idx"]])
