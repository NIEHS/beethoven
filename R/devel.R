# Join variable-specific Parquet datasets !! from a single data source !!
# into one wider Parquet dataset.
# Each element of input contains all batch files for one variable/table.
# Rows within a group are stacked; groups are full-joined using by.
# Keys must be non-missing, unique within each group, and have compatible types.
# Timestamps are matched exactly and are not shifted, rounded, or averaged.
join_variables <- function(
  input,
  output,
  by = c("site_id", "time"),
  check_keys = TRUE
) {
  # ----------------------------------------------------------------------------
  # Validate the file groups, join columns, and output destination.
  if (!is.list(input) || !length(input)) {
    stop("input must be a non-empty list of Parquet file vectors.")
  }
  if (
    !is.character(by) ||
      !length(by) ||
      anyNA(by) ||
      any(!nzchar(by)) ||
      anyDuplicated(by)
  ) {
    stop("by must contain distinct, non-empty column names.")
  }
  if (
    !is.character(output) ||
      length(output) != 1L ||
      is.na(output) ||
      !nzchar(output)
  ) {
    stop("output must be one directory path.")
  }
  if (
    !is.logical(check_keys) || length(check_keys) != 1L || is.na(check_keys)
  ) {
    stop("check_keys must be TRUE or FALSE.")
  }
  chr_variables <- names(input)
  if (is.null(chr_variables)) {
    chr_variables <- sprintf("input_%03d", seq_along(input))
  }
  for (i in seq_along(input)) {
    chr_paths <- input[[i]]
    if (
      !is.character(chr_paths) ||
        !length(chr_paths) ||
        anyNA(chr_paths) ||
        any(!file.exists(chr_paths)) ||
        any(dir.exists(chr_paths)) ||
        anyDuplicated(chr_paths)
    ) {
      stop("Input group ", i, " must contain existing, distinct files.")
    }
  }

  # ----------------------------------------------------------------------------
  # Quote identifiers, including names containing capitals or punctuation.
  chr_by_sql <- paste0('"', gsub('"', '""', by, fixed = TRUE), '"')
  sql_by <- paste(chr_by_sql, collapse = ", ")

  # ----------------------------------------------------------------------------
  # Generate names specific to this call in the working global context.
  chr_view_prefix <- gsub(
    "[^A-Za-z0-9_]",
    "_",
    basename(tempfile("db_merge_variables_"))
  )
  chr_variable_views <- sprintf(
    "%s_%03d",
    chr_view_prefix,
    seq_along(input)
  )
  chr_merge_views <- character(0)
  chr_data_columns <- character(0)

  # ----------------------------------------------------------------------------
  # Drop only views created by this call, including when an R error occurs.
  on.exit(
    {
      for (h in rev(chr_merge_views)) {
        tryCatch(
          sedonadb::sd_drop_view(h),
          error = function(e) {
            message("Could not remove view ", h, ": ", conditionMessage(e))
          }
        )
      }
    },
    add = TRUE
  )

  # ----------------------------------------------------------------------------
  # Read each group as one logical table and check its schema and join keys.
  for (i in seq_along(input)) {
    message("Registering variable group ", i, "/", length(input), "...")
    db_variable <- sedonadb::sd_read_parquet(input[[i]])
    sedonadb::sd_to_view(db_variable, chr_variable_views[i])
    chr_merge_views <- c(chr_merge_views, chr_variable_views[i])

    # LIMIT 0 collects column names and types without collecting data rows.
    chr_columns <- names(
      sedonadb::sd_sql(
        paste0("SELECT * FROM ", chr_variable_views[i], " LIMIT 0")
      ) |>
        sedonadb::sd_collect()
    )
    if (!all(by %in% chr_columns)) {
      stop("Missing join columns in input group: ", chr_variables[i])
    }
    chr_variable_columns <- setdiff(chr_columns, by)
    if (!length(chr_variable_columns)) {
      stop("No data columns in input group: ", chr_variables[i])
    }
    chr_conflicts <- intersect(chr_variable_columns, chr_data_columns)
    if (length(chr_conflicts)) {
      stop(
        "Data columns occur in more than one group: ",
        paste(chr_conflicts, collapse = ", "),
        ". Rename these columns before merging."
      )
    }
    chr_data_columns <- c(chr_data_columns, chr_variable_columns)

    # This optional scan prevents duplicate keys from multiplying output rows.
    # It also rejects missing keys, which would not match using SQL equality.
    # Set FALSE only when the input key constraints are already established.
    if (check_keys) {
      message("Checking keys for ", chr_variables[i], "...")
      sql_missing <- paste(paste(chr_by_sql, "IS NULL"), collapse = " OR ")
      sql_check <- paste0(
        "SELECT ",
        sql_by,
        " FROM ",
        chr_variable_views[i],
        " GROUP BY ",
        sql_by,
        " HAVING COUNT(*) > 1 OR (",
        sql_missing,
        ") LIMIT 1"
      )
      df_key_check <- sedonadb::sd_sql(sql_check) |>
        sedonadb::sd_collect()
      if (nrow(df_key_check)) {
        stop(
          "Duplicate or missing join keys in input group: ",
          chr_variables[i]
        )
      }
    }
  }

  # ----------------------------------------------------------------------------
  # Build a single query across the variable tables.
  # FULL OUTER JOIN preserves keys present in any variable group.
  # USING produces one shared copy of each key in SELECT *.
  # Missing variable values remain NULL; all data columns are retained.
  # ------------------------------------------------------------------------------
  # Retain keys from either side, including rows absent from earlier variables.
  sql_join_keys <- paste(
    sprintf(
      "COALESCE(l.%s, r.%s) AS %s",
      chr_by_sql,
      chr_by_sql,
      chr_by_sql
    ),
    collapse = ",\n"
  )

  # ------------------------------------------------------------------------------
  # Fully qualify both sides of each join condition.
  sql_join_condition <- paste(
    sprintf("l.%s = r.%s", chr_by_sql, chr_by_sql),
    collapse = " AND "
  )

  # ------------------------------------------------------------------------------
  # Build nested joins with one shared set of keys at every step.
  # This constructs one SQL query; it does not execute joins inside the loop.
  sql_merge <- paste0(
    "SELECT * FROM ",
    chr_variable_views[1L]
  )

  for (i in seq_along(chr_variable_views)[-1L]) {
    sql_merge <- paste0(
      "SELECT\n",
      sql_join_keys,
      ",\n",
      "l.* EXCLUDE (",
      sql_by,
      "),\n",
      "r.* EXCLUDE (",
      sql_by,
      ")\n",
      "FROM (\n",
      sql_merge,
      "\n) AS l\n",
      "FULL OUTER JOIN ",
      chr_variable_views[i],
      " AS r\n",
      "ON ",
      sql_join_condition
    )
  }

  # ----------------------------------------------------------------------------
  # Execute once and write directly to a Parquet dataset directory.
  # No growing materialized stack, global sort, or full R collection is needed.
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  message("Joining variables and writing Parquet dataset...")
  db_extraction <- sedonadb::sd_sql(sql_merge)
  sedonadb::sd_write_parquet(
    db_extraction,
    output,
    single_file_output = FALSE
  )
  list.files(output, recursive = TRUE, full.names = TRUE)
}

# Left-join hourly, daily, and static Parquet covariates onto AQS observations.
#
# input: one AQS Parquet file with location and time columns.
# hourly/daily/static: NULL; a directory, file, or vector of batch files for
# ONE table; or a NAMED LIST of these inputs for multiple separate tables.
# Example: hourly = list(geos = chr_geos_parquet, narr = chr_narr_parquet).
# Combine variable-specific files with join_variables() before using them here.
# output: a NEW directory for the combined Parquet dataset.
#
# All date/time keys must already represent UTC. This function does not convert
# local clock times to UTC. Location identifiers must have compatible types;
# preserve character identifiers with leading zeroes when writing inputs.
#
# For GEOS tavg_1hr data, 00:30 labels the mean over 00:00-01:00 UTC.
# Hourly keys are therefore matched using the start of their calendar hour.
# This rule assumes averages over the same clock-hour intervals; it should not
# be applied to instantaneous records or differently defined averaging windows.
join_covariates <- function(
  input,
  output,
  hourly = NULL,
  daily = NULL,
  static = NULL,
  locs_id = "site_id",
  time_id = "time",
  date_id = "date",
  check_keys = TRUE,
  prefix = FALSE
) {
  # ----------------------------------------------------------------------------
  # Validate the AQS file, output directory, and shared column names.
  if (
    !is.character(input) ||
      length(input) != 1L ||
      is.na(input) ||
      !file.exists(input) ||
      dir.exists(input)
  ) {
    stop("input must identify one existing AQS Parquet file.")
  }
  if (
    !is.character(output) ||
      length(output) != 1L ||
      is.na(output) ||
      !nzchar(output)
  ) {
    stop("output must be one directory path.")
  }
  list_ids <- list(locs_id, time_id, date_id)
  if (
    !all(vapply(
      list_ids,
      function(x) {
        is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
      },
      logical(1)
    )) ||
      anyDuplicated(c(locs_id, time_id, date_id))
  ) {
    stop("locs_id, time_id, and date_id must be distinct column names.")
  }
  if (
    !is.logical(check_keys) || length(check_keys) != 1L || is.na(check_keys)
  ) {
    stop("check_keys must be TRUE or FALSE.")
  }
  if (!is.logical(prefix) || length(prefix) != 1L || is.na(prefix)) {
    stop("prefix must be TRUE or FALSE.")
  }

  # ----------------------------------------------------------------------------
  # Quote SQL identifiers, preserving capitals and punctuation in data columns.
  quote_sql <- function(x) {
    paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
  }
  locs_id_sql <- quote_sql(locs_id)
  time_id_sql <- quote_sql(time_id)
  date_id_sql <- quote_sql(date_id)

  # ----------------------------------------------------------------------------
  # Expand each optional resolution into its separate source tables.
  # A character vector is ONE table's batch files; a named list is MANY tables.
  list_inputs <- list(aqs = input)
  chr_source_types <- c(aqs = "aqs")
  chr_source_labels <- c(aqs = "aqs")
  list_resolutions <- list(hourly = hourly, daily = daily, static = static)

  for (chr_resolution in names(list_resolutions)) {
    list_sources <- list_resolutions[[chr_resolution]]
    if (is.null(list_sources)) {
      next
    }
    if (is.character(list_sources)) {
      list_sources <- setNames(list(list_sources), chr_resolution)
    } else if (!is.list(list_sources)) {
      stop(
        chr_resolution,
        " must be a file vector or a named list of datasets."
      )
    }
    if (!length(list_sources)) {
      next
    }
    chr_names <- names(list_sources)
    if (
      is.null(chr_names) ||
        anyNA(chr_names) ||
        any(!nzchar(chr_names)) ||
        anyDuplicated(chr_names)
    ) {
      stop("Supply distinct, non-empty dataset names in ", chr_resolution, ".")
    }
    for (i in seq_along(list_sources)) {
      # Internal SQL names do not depend on user-supplied dataset labels.
      chr_source <- sprintf("source_%03d", length(list_inputs))
      list_inputs[chr_source] <- list(list_sources[[i]])
      chr_source_types[chr_source] <- chr_resolution
      chr_source_labels[chr_source] <- paste(
        chr_resolution,
        chr_names[i],
        sep = "/"
      )
    }
  }
  chr_sources <- names(list_inputs)

  # ----------------------------------------------------------------------------
  # Use view names specific to this call in the working global context.
  chr_view_prefix <- gsub(
    "[^A-Za-z0-9_]",
    "_",
    basename(tempfile("db_join_covariates_"))
  )
  chr_source_views <- setNames(
    paste0(chr_view_prefix, "_", chr_sources),
    chr_sources
  )
  chr_join_views <- character(0)

  # ----------------------------------------------------------------------------
  # Remove only views registered by this call, on normal return or an R error.
  # Input and output files are not deleted.
  on.exit(
    {
      for (h in rev(chr_join_views)) {
        tryCatch(
          sedonadb::sd_drop_view(h),
          error = function(e) {
            message("Could not remove view ", h, ": ", conditionMessage(e))
          }
        )
      }
    },
    add = TRUE
  )

  # ----------------------------------------------------------------------------
  # Store SQL projections and output fields; source rows stay in SedonaDB.
  list_source_sql <- list()
  chr_output_columns <- character(0)
  chr_select <- "a.*"
  chr_join <- character(0)

  for (chr_source in chr_sources) {
    chr_resolution <- chr_source_types[[chr_source]]
    chr_source_label <- chr_source_labels[[chr_source]]
    # --------------------------------------------------------------------------
    # Resolve a source directory or file vector into its batch files.
    chr_paths <- list_inputs[[chr_source]]
    if (
      !is.character(chr_paths) ||
        !length(chr_paths) ||
        anyNA(chr_paths) ||
        any(!nzchar(chr_paths)) ||
        any(!file.exists(chr_paths))
    ) {
      stop(
        "Source ",
        chr_source_label,
        " must contain existing files or directories."
      )
    }
    chr_paths <- unlist(
      lapply(chr_paths, function(h) {
        if (dir.exists(h)) {
          list.files(
            h,
            pattern = "\\.parquet$",
            full.names = TRUE,
            recursive = TRUE
          )
        } else {
          h
        }
      }),
      use.names = FALSE
    )
    if (!length(chr_paths)) {
      stop("No Parquet files found for source: ", chr_source_label)
    }
    chr_paths <- unique(normalizePath(
      chr_paths,
      winslash = "/",
      mustWork = TRUE
    ))

    # --------------------------------------------------------------------------
    # Register each file group as one logical table without collecting its rows.
    message("Registering ", chr_source_label, " data...")
    db_source <- sedonadb::sd_read_parquet(chr_paths)
    sedonadb::sd_to_view(db_source, chr_source_views[[chr_source]])
    chr_join_views <- c(chr_join_views, chr_source_views[[chr_source]])

    # Retrieve only schema information using a zero-row result.
    chr_columns <- names(
      sedonadb::sd_sql(paste0(
        "SELECT * FROM ",
        chr_source_views[[chr_source]],
        " LIMIT 0"
      )) |>
        sedonadb::sd_collect()
    )
    chr_keys <- switch(
      chr_resolution,
      aqs = c(locs_id, time_id),
      hourly = c(locs_id, time_id),
      daily = c(locs_id, date_id),
      static = locs_id
    )
    if (!all(chr_keys %in% chr_columns)) {
      stop(
        "Missing join columns in ",
        chr_source_label,
        ": ",
        paste(setdiff(chr_keys, chr_columns), collapse = ", ")
      )
    }

    # --------------------------------------------------------------------------
    # Normalize the temporal key inside SQL and retain every other column.
    # Preserve the AQS timestamp; normalize the hourly right-hand key to HH:00.
    chr_projection <- quote_sql(chr_columns)
    if (chr_resolution %in% c("aqs", "hourly")) {
      sql_time <- paste0("CAST(", time_id_sql, " AS TIMESTAMP)")
      if (chr_resolution == "hourly") {
        sql_time <- paste0("DATE_TRUNC('hour', ", sql_time, ")")
      }
      chr_projection[match(time_id, chr_columns)] <- paste0(
        sql_time,
        " AS ",
        time_id_sql
      )
    } else if (chr_resolution == "daily") {
      chr_projection[match(date_id, chr_columns)] <- paste0(
        "CAST(",
        date_id_sql,
        " AS DATE) AS ",
        date_id_sql
      )
    }
    sql_source <- paste0(
      "SELECT ",
      paste(chr_projection, collapse = ", "),
      " FROM ",
      chr_source_views[[chr_source]]
    )
    list_source_sql[[chr_source]] <- paste0(
      chr_source,
      "_data AS (\n",
      sql_source,
      "\n)"
    )

    # --------------------------------------------------------------------------
    # Keep all AQS columns and its existing row multiplicity.
    if (chr_source == "aqs") {
      chr_output_columns <- chr_columns
      next
    }

    # --------------------------------------------------------------------------
    # Retain every right-hand data column, excluding only its join keys.
    # Optionally prefix data columns by resolution and dataset label.
    # Example with prefix = TRUE: hourly/geos + CO -> hourly_geos_CO.
    chr_data_columns <- setdiff(chr_columns, chr_keys)
    chr_data_names <- chr_data_columns
    if (prefix && length(chr_data_columns)) {
      chr_data_names <- paste0(
        gsub("/", "_", chr_source_label, fixed = TRUE),
        "_",
        chr_data_columns
      )
    }
    chr_conflicts <- intersect(chr_data_names, chr_output_columns)
    if (length(chr_conflicts)) {
      stop(
        "Conflicting data columns in ",
        chr_source_label,
        ": ",
        paste(chr_conflicts, collapse = ", "),
        ". Set prefix = TRUE or rename these columns before joining."
      )
    }
    chr_output_columns <- c(chr_output_columns, chr_data_names)

    # --------------------------------------------------------------------------
    # Check right-hand uniqueness AFTER timestamp/date normalization.
    # Duplicate right-hand keys would multiply AQS rows during the left join.
    # Only an at-most-one-row diagnostic is collected into R.
    if (check_keys) {
      message("Checking ", chr_source_label, " join keys...")
      chr_keys_sql <- quote_sql(chr_keys)
      sql_keys <- paste(chr_keys_sql, collapse = ", ")
      sql_missing <- paste(paste(chr_keys_sql, "IS NULL"), collapse = " OR ")
      sql_check <- paste0(
        "SELECT ",
        sql_keys,
        " FROM (",
        sql_source,
        ") AS v",
        " GROUP BY ",
        sql_keys,
        " HAVING COUNT(*) > 1 OR (",
        sql_missing,
        ") LIMIT 1"
      )
      df_key_check <- sedonadb::sd_sql(sql_check) |>
        sedonadb::sd_collect()
      if (nrow(df_key_check)) {
        stop(
          "Duplicate or missing normalized join keys in source: ",
          chr_source_label
        )
      }
    }

    # --------------------------------------------------------------------------
    # Build qualified LEFT JOIN conditions for each temporal resolution.
    chr_alias <- paste0("v", match(chr_source, chr_sources))
    sql_condition <- paste0(
      "a.",
      locs_id_sql,
      " = ",
      chr_alias,
      ".",
      locs_id_sql
    )
    if (chr_resolution == "hourly") {
      sql_condition <- paste0(
        sql_condition,
        " AND DATE_TRUNC('hour', a.",
        time_id_sql,
        ") = ",
        chr_alias,
        ".",
        time_id_sql
      )
    } else if (chr_resolution == "daily") {
      sql_condition <- paste0(
        sql_condition,
        " AND CAST(a.",
        time_id_sql,
        " AS DATE) = ",
        chr_alias,
        ".",
        date_id_sql
      )
    }
    chr_join <- c(
      chr_join,
      paste0(
        "LEFT JOIN ",
        chr_source,
        "_data AS ",
        chr_alias,
        " ON ",
        sql_condition
      )
    )
    if (length(chr_data_columns)) {
      chr_select <- c(
        chr_select,
        paste0(
          chr_alias,
          ".",
          quote_sql(chr_data_columns),
          " AS ",
          quote_sql(chr_data_names)
        )
      )
    }
  }

  # ----------------------------------------------------------------------------
  # Build one query. Daily/static values repeat over existing matching AQS rows.
  # No extra hours are generated, and missing covariates remain NULL.
  sql_merge <- paste0(
    "WITH\n",
    paste(unlist(list_source_sql, use.names = FALSE), collapse = ",\n"),
    "\nSELECT\n",
    paste(chr_select, collapse = ",\n"),
    "\nFROM aqs_data AS a\n",
    paste(chr_join, collapse = "\n")
  )

  # ----------------------------------------------------------------------------
  # Execute and write directly to a Parquet dataset for Spark.
  # No global sort or full-result collection into R is performed.
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  message("Joining covariates and writing Parquet dataset...")
  db_extraction <- sedonadb::sd_sql(sql_merge)
  db_extraction <- sedonadb::sd_select(
    db_extraction,
    tidyselect::all_of(stats::setNames(
      chr_output_columns,
      gsub(".", "_", chr_output_columns, fixed = TRUE)
    ))
  )
  sedonadb::sd_write_parquet(db_extraction, output, single_file_output = FALSE)
  list.files(output, full.names = TRUE, recursive = TRUE)
}
