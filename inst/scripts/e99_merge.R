################################################################################
# Merge all calculated covariates at Uber H3 locations.

################################################################################
# Load libraries and source local functions.
library(beethoven)
library(amadeusdb)
source("R/merge.R")

################################################################################
# Import configured variables and inputs.
db_grid_index_sptmpl <- snakemake@input[["db_grid_index_sptmpl"]]
db_grid_feat_geos_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_grid_feat_geos_sptmpl"
]])
db_grid_feat_hrrr_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_grid_feat_hrrr_sptmpl"
]])
db_grid_feat_hms_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_grid_feat_hms_sptmpl"
]])
db_grid_feat_spacetime_sptmpl <- qs2::qs_read(snakemake@input[[
  "db_grid_feat_spacetime_sptmpl"
]])

################################################################################
# Group the GEOS paths by the 240-hour directories written by e02_geos.R.
list_grid_feat_geos_batches_sptmpl <- split(
  db_grid_feat_geos_sptmpl,
  basename(dirname(db_grid_feat_geos_sptmpl))
)
chr_grid_merge_batches <- names(list_grid_feat_geos_batches_sptmpl)
stopifnot(
  length(chr_grid_merge_batches) > 0L,
  all(grepl("^[0-9]{12}_[0-9]{12}$", chr_grid_merge_batches))
)

################################################################################
# Normalize GEOS's :30 timestamps to the hourly join keys. Use [start, end)
# windows so the last hour of one batch and first hour of the next occur once.
time_grid_merge_start <- as.POSIXct(
  substr(chr_grid_merge_batches, 1L, 12L),
  format = "%Y%m%d%H%M",
  tz = "UTC"
)
time_grid_merge_end <- as.POSIXct(
  substr(chr_grid_merge_batches, 14L, 25L),
  format = "%Y%m%d%H%M",
  tz = "UTC"
)
time_grid_merge_start <- as.POSIXct(
  floor(as.numeric(time_grid_merge_start) / 3600) * 3600,
  origin = "1970-01-01",
  tz = "UTC"
)
time_grid_merge_end <- as.POSIXct(
  floor(as.numeric(time_grid_merge_end) / 3600) * 3600 + 3600,
  origin = "1970-01-01",
  tz = "UTC"
)
stopifnot(
  !anyNA(time_grid_merge_start),
  !anyNA(time_grid_merge_end),
  all(time_grid_merge_end > time_grid_merge_start),
  all(head(time_grid_merge_end, -1L) <= tail(time_grid_merge_start, -1L))
)

################################################################################
# Configure query memory before the first SedonaDB query. The batch size bounds
# join inputs; this separate limit guards against excessive query allocations.
chr_grid_merge_memory <- Sys.getenv("GRID_MERGE_MEMORY_LIMIT", "64gb")
int_grid_merge_threads <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "4"))
stopifnot(!is.na(int_grid_merge_threads), int_grid_merge_threads > 0L)
sedonadb::sd_connect(
  global = TRUE,
  memory_limit = chr_grid_merge_memory,
  memory_pool_type = "fair",
  temp_dir = tempdir()
)
invisible(sedonadb::sd_collect(sedonadb::sd_sql(sprintf(
  "SET datafusion.execution.target_partitions = %d",
  int_grid_merge_threads
))))
message(
  "Grid merge memory budget: ",
  chr_grid_merge_memory,
  "; execution partitions: ",
  int_grid_merge_threads,
  "."
)

################################################################################
# Merge sequentially, with all sources restricted before each join. GEOS is
# already partitioned; the other sources need temporary slices of the same window.
db_grid_feat_sptmpl <- unlist(
  lapply(seq_along(chr_grid_merge_batches), function(i) {
    chr_batch <- chr_grid_merge_batches[i]
    message(
      "Merging grid batch ",
      i,
      "/",
      length(chr_grid_merge_batches),
      ": ",
      chr_batch,
      "..."
    )
    chr_tmp <- tempfile("grid_merge_inputs_")
    dir.create(chr_tmp)
    on.exit(
      {
        unlink(chr_tmp, recursive = TRUE)
        gc(verbose = FALSE)
      },
      add = TRUE
    )

    list_sources <- list(
      index = db_grid_index_sptmpl,
      hrrr = db_grid_feat_hrrr_sptmpl,
      spacetime = db_grid_feat_spacetime_sptmpl,
      hms = db_grid_feat_hms_sptmpl
    )
    list_slices <- lapply(names(list_sources), function(source) {
      message("Slicing ", source, " for ", chr_batch, "...")
      write_grid_merge_slice(
        input = list_sources[[source]],
        output = file.path(chr_tmp, paste0(source, ".parquet")),
        start = time_grid_merge_start[i],
        end = time_grid_merge_end[i],
        daily = source == "hms"
      )
    })
    names(list_slices) <- names(list_sources)

    amadeusdb::join_covariates(
      input = list_slices$index,
      output = file.path("output/grid/feat/merge", chr_batch),
      hourly = list(
        geos = list_grid_feat_geos_batches_sptmpl[[i]],
        hrrr = list_slices$hrrr,
        spacetime = list_slices$spacetime
      ),
      daily = list(hms = list_slices$hms),
      static = NULL,
      locs_id = "site_id",
      prefix = FALSE
    )
  }),
  use.names = FALSE
)

################################################################################
qs2::qs_save(db_grid_feat_sptmpl, snakemake@output[["db_grid_feat_sptmpl"]])
