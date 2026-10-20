################################################################################
env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(default)
  }
  if (value %in% c("1", "true", "yes", "y")) {
    return(TRUE)
  }
  if (value %in% c("0", "false", "no", "n")) {
    return(FALSE)
  }
  stop(name, " must be true or false.")
}

################################################################################
sql_identifier <- function(x) {
  paste0("`", gsub("`", "``", x, fixed = TRUE), "`")
}

################################################################################
sql_string <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

################################################################################
env_integer <- function(name, default, minimum = 1L) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    return(as.integer(default))
  }

  value <- suppressWarnings(as.integer(value))
  if (is.na(value) || value < minimum) {
    stop(name, " must be an integer greater than or equal to ", minimum)
  }
  value
}

################################################################################
environment <- function(workspace = Sys.getenv("HOME"), database = "sedonadb") {
  # Detected from Sys.getenv()
  list_environ <- list(
    chr_workdir = workspace,
    int_maxcores = env_integer("SPARK_CORES_MAX", 64L),
    int_executorcores = env_integer("SPARK_EXECUTOR_CORES", 2L),
    int_executormem = Sys.getenv("SPARK_EXECUTOR_MEMORY", unset = "2g"),
    int_partitions = env_integer("SPARK_SHUFFLE_PARTITIONS", 32L),
    int_nround = env_integer("XGB_NUM_ROUND", 50L),
    int_seed = env_integer("XGB_SEED", 20260904L, minimum = 0L),
    int_maxrows = env_integer("XGB_MAX_ROWS", 0L, minimum = 0L),
    int_maxfeatures = env_integer("XGB_MAX_FEATURES", 0L, minimum = 0L),
    chr_database = Sys.getenv("XGB_DATABASE", unset = database),
    chr_xgboost_package = Sys.getenv(
      "XGBOOST_MAVEN_PACKAGE",
      unset = "ml.dmlc:xgboost4j-spark_2.13:3.4.0"
    )
  )

  # Derived from other environmental variables
  list_environ$chr_resultspath <- Sys.getenv(
    "XGB_RESULTS_PATH",
    unset = file.path(list_environ$chr_workdir, "xgboost-results.rds")
  )
  list_environ$int_nworkers <- env_integer(
    "XGB_NUM_WORKERS",
    max(1L, list_environ$int_maxcores %/% list_environ$int_executorcores)
  )
  list_environ$int_nthreads <- env_integer(
    "XGB_NTHREAD",
    list_environ$int_executorcores
  )
  list_environ$chr_warehouse <- Sys.getenv(
    "XGB_WAREHOUSE_DIR",
    unset = file.path(list_environ$chr_workdir, "spark-warehouse")
  )
  chr_databasedir <- file.path(
    list_environ$chr_warehouse,
    paste0(list_environ$chr_database, ".db")
  )

  # Threads and cores check.
  if (list_environ$int_executorcores > list_environ$int_maxcores) {
    stop("SPARK_EXECUTOR_CORES cannot exceed SPARK_CORES_MAX.")
  }

  if (list_environ$int_nthreads > list_environ$int_executorcore) {
    stop("XGB_NTHREAD cannot exceed SPARK_EXECUTOR_CORES.")
  }

  if (
    list_environ$int_nworkers * list_environ$int_nthreads >
      list_environ$int_maxcores
  ) {
    stop("XGB_NUM_WORKERS * XGB_NTHREAD cannot exceed SPARK_CORES_MAX.")
  }

  # Populate parent environment with variables
  list2env(list_environ, envir = parent.frame())

  # Invisible return
  invisible(list_environ)
}
