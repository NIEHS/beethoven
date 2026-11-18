################################################################################
# Implement spatiotemporal cross validation schema.

################################################################################
# Load libraries and source local functions.
library(beethoven)
library(sedonadb)

################################################################################
# Import configured variables and inputs.
db_train_feat_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_train_feat_sptmpl"
]])
sf_train_sites_sp <- qs2::qs_read(snakemake@input[["sf_train_sites_sp"]])
int_base_iter <- snakemake@params[["int_base_iter"]]

################################################################################
# Extract column names from db_train_feat_sptmpl without full read in.
tbl_train_feat_sptmpl <- sedonadb::sd_read_parquet(db_train_feat_sptmpl)
sedonadb::sd_to_view(
  tbl_train_feat_sptmpl,
  "tbl_train_feat_sptmpl",
  overwrite = TRUE
)
chr_train_feat_names <- sedonadb::sd_sql(
  "SELECT * FROM tbl_train_feat_sptmpl LIMIT 0"
) |>
  sedonadb::sd_collect() |>
  names()

################################################################################
# Define dependent variable.
chr_train_yvar <- "Sample_Measurement"

################################################################################
# Define drop/identification variables.
chr_train_dvar <- c("site_id", "time")

################################################################################
# Define common parameters across all base learner models.
list_base_params_static <- list(
  yvar = chr_train_yvar,
  xvar = setdiff(
    chr_train_feat_names,
    c(chr_train_dvar, chr_train_yvar, "level")
  ),
  dvar = chr_train_dvar,
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
sf_train_sites_5070_sp <- sf::st_transform(
  sf_train_sites_sp,
  crs = list_base_params_static$crs
)

################################################################################
# Generate spatial v-fold rset objects with pre-defined cell sizes and buffers.
rset_base_cv_sp <- spatialsample::spatial_block_cv(
  sf_train_sites_5070_sp,
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
  split(seq_len(nrow(rset_base_cv_sp)), rset_base_cv_sp$id),
  function(i) {
    stats::setNames(
      lapply(i, function(j) {
        split_j <- rset_base_cv_sp$splits[[j]]
        list(
          analysis = rsample::analysis(split_j)$site_id,
          assessment = rsample::assessment(split_j)$site_id
        )
      }),
      rset_base_cv_sp$id2[i]
    )
  }
)

################################################################################
qs2::qs_save(
  list_base_params_static,
  snakemake@output[["list_base_params_static"]]
)
qs2::qs_save(list_base_cv_idx, snakemake@output[["list_base_cv_idx"]])
