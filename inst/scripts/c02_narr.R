################################################################################
# Calculate NARR covariates at unique AQS locations (spatial only).

################################################################################
# Load libraries and source local functions.
library(amadeus)
source("R/amadeus.R")
source("R/beethoven.R")

################################################################################
# Import configured variables and inputs.
chr_config_dir <- snakemake@params[["chr_config_dir"]]
chr_init_dates <- qs2::qs_read(snakemake@input[["chr_init_dates"]])
chr_iter_narr <- qs2::qs_read(snakemake@input[["chr_iter_narr"]])
sf_feat_aqs_sp <- qs2::qs_read(snakemake@input[["sf_feat_aqs_sp"]])

################################################################################
mirai::daemons(100)
list_feat_narr_sptmpl <- lapply(
  chr_iter_narr,
  function(x) {
    dt_feat_x_sptmpl <- suppressMessages(
      calculate_narr_mirai(
        from = amadeus::process_narr(
          path = file.path(chr_config_dir, "narr", x),
          variable = x,
          date = fl_dates(chr_init_dates)
        ),
        locs = sf_feat_aqs_sp,
        locs_id = "site_id",
        radius = 0,
        fun = "mean",
        geom = FALSE
      )
    )
    if ("level" %in% names(dt_feat_x_sptmpl)) {
      dt_feat_x_sptmpl |>
        dplyr::filter(level == 1000) |>
        dplyr::select(-level)
    } else {
      dt_feat_x_sptmpl
    }
  }
)
mirai::daemons(0)

################################################################################
dt_feat_narr_sptmpl <- reduce_merge(list_feat_narr_sptmpl)

################################################################################
qs2::qs_save(
  data.table::data.table(dt_feat_narr_sptmpl),
  snakemake@output[["dt_feat_narr_sptmpl"]]
)
