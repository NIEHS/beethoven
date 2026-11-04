chr_home <- Sys.getenv("HOME")
sedonadb_libstdcxx <- Sys.getenv(
  "SEDONADB_LIBSTDCXX",
  unset = paste0(
    chr_home,
    "/.conda/envs/sdamadeus/lib/libstdc++.so.6"
  )
)

# The system R executable starts with an older libstdc++ than the GEOS library
# used by this sedonadb installation. When this helper is run directly, restart
# it before loading any packages so the compatible C++ runtime wins symbol
# resolution. The parent XGBoost script already supplies the same preload.
preloaded_libraries <- strsplit(
  Sys.getenv("LD_PRELOAD", unset = ""),
  split = ":",
  fixed = TRUE
)[[1]]

if (!sedonadb_libstdcxx %in% preloaded_libraries) {
  if (!file.exists(sedonadb_libstdcxx)) {
    stop("Compatible SedonaDB C++ runtime not found: ", sedonadb_libstdcxx)
  }
  if (identical(Sys.getenv("SEDONADB_RUNTIME_REEXEC"), "1")) {
    stop("Could not preload the compatible SedonaDB C++ runtime.")
  }

  script_arg <- grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )
  if (length(script_arg) != 1L) {
    stop(
      "This script must be run with Rscript so it can initialize the ",
      "SedonaDB C++ runtime."
    )
  }

  script_path <- sub("^--file=", "", script_arg)
  current_preload <- Sys.getenv("LD_PRELOAD", unset = "")
  preload <- paste(
    c(
      sedonadb_libstdcxx,
      current_preload[nzchar(current_preload)]
    ),
    collapse = ":"
  )
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    args = c(shQuote(script_path), shQuote(commandArgs(trailingOnly = TRUE))),
    env = c(
      paste0("LD_PRELOAD=", preload),
      "SEDONADB_RUNTIME_REEXEC=1"
    )
  )
  quit(save = "no", status = status, runLast = FALSE)
}

message(getwd())
source("R/spark.R")
library(sedonadb)
Sys.setenv("SEDONADB_PARQUET_PATH" = paste0(chr_home, "beethoven/beethoven/"))

main <- function() {
  workspace_dir <- paste0(chr_home, "/beethoven/sedonatest")
  output_path <- Sys.getenv("SEDONADB_PARQUET_PATH", unset = "")
  if (!nzchar(output_path)) {
    stop("SEDONADB_PARQUET_PATH must specify the output Parquet file.")
  }

  overwrite <- env_flag("SEDONADB_OVERWRITE", default = FALSE)
  max_rows <- env_integer("XGB_MAX_ROWS", 0L, minimum = 0L)
  seed <- env_integer("XGB_SEED", 20260904L, minimum = 0L)
  row_group_size <- env_integer("SEDONADB_ROW_GROUP_SIZE", 32768L)
  memory_limit <- Sys.getenv("SEDONADB_MEMORY_LIMIT", unset = "16gb")

  output_dir <- dirname(output_path)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_path <- normalizePath(output_path, mustWork = FALSE)
  temporary_output <- paste0(output_path, ".tmp-", Sys.getpid())
  on.exit(unlink(temporary_output, recursive = TRUE, force = TRUE), add = TRUE)

  if (file.exists(output_path) && !overwrite) {
    stop(
      "Output already exists: ",
      output_path,
      ". Set SEDONADB_OVERWRITE=true to rebuild it."
    )
  }

  spill_root <- Sys.getenv(
    "SEDONADB_TEMP_DIR",
    unset = file.path(workspace_dir, ".sedonadb-tmp")
  )
  dir.create(spill_root, recursive = TRUE, showWarnings = FALSE)
  spill_dir <- tempfile(pattern = "build_", tmpdir = spill_root)
  if (!dir.create(spill_dir, showWarnings = FALSE)) {
    stop("Could not create SedonaDB spill directory: ", spill_dir)
  }
  on.exit(unlink(spill_dir, recursive = TRUE, force = TRUE), add = TRUE)

  read_time <- system.time({
    dt_feat_pm_imputed <- qs2::qs_read(
      "output/dt_feat_merge_sptmpl.qs"
    )
  })

  if (max_rows > 0L && max_rows < nrow(dt_feat_pm_imputed)) {
    set.seed(seed)
    keep <- sample.int(nrow(dt_feat_pm_imputed), max_rows)
    dt_feat_pm_imputed <- dt_feat_pm_imputed[keep, , drop = FALSE]
  }

  cat(
    "Loaded",
    format(nrow(dt_feat_pm_imputed), big.mark = ","),
    "rows and",
    ncol(dt_feat_pm_imputed),
    "columns in",
    unname(read_time[["elapsed"]]),
    "seconds\n"
  )

  ctx <- sd_connect(
    memory_limit = memory_limit,
    temp_dir = spill_dir,
    memory_pool_type = "fair"
  )

  cat("Registering the R data frame as a SedonaDB view...\n")
  as_sedonadb_dataframe(dt_feat_pm_imputed, ctx = ctx) |>
    sd_to_view(
      "dt_feat_pm_imputed",
      overwrite = TRUE,
      ctx = ctx
    )

  cat("Writing the persistent SedonaDB Parquet table...\n")
  write_time <- system.time({
    sd_ctx_sql(ctx, "SELECT * FROM dt_feat_pm_imputed") |>
      sd_write_parquet(
        temporary_output,
        single_file_output = TRUE,
        max_row_group_size = row_group_size,
        compression = "zstd(3)"
      )
  })

  if (!file.exists(temporary_output)) {
    stop("SedonaDB did not create the expected Parquet file.")
  }
  if (file.exists(output_path)) {
    unlink(output_path, force = TRUE)
  }
  if (!file.rename(temporary_output, output_path)) {
    stop("Could not move the completed Parquet file to ", output_path)
  }

  cat(
    "Wrote",
    format(file.info(output_path)$size, big.mark = ","),
    "bytes to",
    output_path,
    "in",
    unname(write_time[["elapsed"]]),
    "seconds\n"
  )
}

main()
