################################################################################
# Implement spatiotemporal cross validation schema.

################################################################################
# Load libraries and source local functions.
library(mirai)
library(mori)
library(beethoven)

################################################################################
# Import configured variables and inputs.
dt_feat_merge_sptmpl <- arrow::read_parquet(snakemake@input[[
  "dt_feat_merge_sptmpl"
]])

################################################################################
# Define common parameters across all base learner models.
list_base_params_static <- list(
  data = mori::share(dt_feat_merge_sptmpl),
  yvar = "Arithmetic_Mean",
  xvar = setdiff(
    names(dt_feat_merge_sptmpl),
    c("site_id", "time", "Event_Type", "Arithmetic_Mean")
  ),
  dvar = c("site_id", "time"),
  normalize = TRUE,
  n_models = 1L,
  metric = "mae",
  tune = 3L,
  crs = 5070L,
  cellsize = 250000L,
  cvsize = 5L
)
stopifnot(mori::is_shared(list_base_params_static$data))

################################################################################
# Derive sequence for number of models.
int_base_cv_index <- seq_len(list_base_params_static$n_models)

################################################################################
# Create training and testing split indices.
mirai::daemons(length(int_base_cv_index))
mirai_base_cv_index <- mirai::mirai_map(
  int_base_cv_index,
  function(x) {
    message(sprintf("Generating spatiotemporal CV rset %03d", x))
    beethoven::generate_cv_index_spt(
      data = list_base_params_static$data,
      locs_id = "site_id",
      time_id = "time",
      coords = c("lon", "lat"),
      crs = list_base_params_static$crs,
      cellsize = list_base_params_static$cellsize,
      v = list_base_params_static$cvsize
    )
  },
  list_base_params_static = list_base_params_static
)
list_base_cv_index <- mirai::collect_mirai(
  mirai_base_cv_index,
  options = ".stop"
)

################################################################################
# !!! Ensure that training/testing indices are unique for 100 model runs. !!!
list_keys <- lapply(list_base_cv_index, function(x) match(x, unique(x)))
int_duplicates <- vapply(
  seq_along(list_keys),
  function(i) {
    i > 1L &&
      any(
        vapply(
          seq_len(i - 1L),
          function(j) identical(list_keys[[i]], list_keys[[j]]),
          logical(1)
        )
      )
  },
  logical(1)
)
stopifnot(!any(int_duplicates))

################################################################################
# Generate training/testing rset objects from indices after unqiueness check.
mirai_base_cv_rset <- mirai::mirai_map(
  list_base_cv_index,
  function(x) {
    beethoven::convert_cv_index_rset(
      cvindex = x,
      data = list_base_params_static$data,
      cv_mode = "spatiotemporal"
    )
  },
  list_base_params_static = list_base_params_static
)
list_base_cv_rset <- mirai::collect_mirai(
  mirai_base_cv_rset,
  options = ".stop"
)
mirai::daemons(0)

################################################################################
# Store row identifiers used for full training data.
list_base_params_static$data <- list_base_params_static$data |>
  dplyr::mutate(.row = dplyr::row_number())

################################################################################
qs2::qs_save(
  rlang::duplicate(list_base_params_static),
  snakemake@output[["list_base_params_static"]]
)
qs2::qs_save(
  rlang::duplicate(list_base_cv_rset),
  snakemake@output[["list_base_cv_rset"]]
)
