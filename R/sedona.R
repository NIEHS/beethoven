################################################################################
# Development script for implementing {sedonadb} functionality in {amadeus}.
# Mitchell Manware
# 21 October 2026

################################################################################
# Process and calculate covariates from NOAA HMS data with SedonaDB backend.
sedona_hms <- function(
  date,
  path,
  locs,
  locs_id,
  radius,
  geom,
  batch = 50L
) {
  # ----------------------------------------------------------------------------
  # Validate batch size.
  stopifnot(
    length(batch) == 1L,
    is.numeric(batch),
    !is.na(batch),
    is.finite(batch),
    batch >= 1,
    batch == floor(batch)
  )
  # ----------------------------------------------------------------------------
  # Identify HMS shapefile paths from all files in the path.
  chr_paths <- list.files(
    path,
    pattern = "hms_smoke*.*.shp$",
    full.names = TRUE
  )
  # ----------------------------------------------------------------------------
  # Generate sequence from user supplied dates.
  chr_datesofinterest <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  # ----------------------------------------------------------------------------
  # Select paths which align with the requested date sequence.
  chr_datapaths <- unique(
    grep(
      paste(
        chr_datesofinterest,
        collapse = "|"
      ),
      chr_paths,
      value = TRUE
    )
  )
  # ----------------------------------------------------------------------------
  # Store file path, date from file, and generated database view names.
  df_hmspaths <- data.frame(
    path = chr_datapaths,
    date = hmsdates(chr_datapaths)
  )
  df_hmspaths$view <- sprintf(
    "db_hms_%08s",
    gsub("-", "", df_hmspaths$date)
  )
  # ----------------------------------------------------------------------------
  # Validate dates, paths, and database view names.
  stopifnot(
    all(c("date", "path", "view") %in% names(df_hmspaths)),
    !anyNA(df_hmspaths$date),
    !anyDuplicated(df_hmspaths$date),
    all(file.exists(df_hmspaths$path)),
    !anyNA(df_hmspaths$view),
    !anyDuplicated(df_hmspaths$view)
  )
  # ----------------------------------------------------------------------------
  # Define location identifiers in SQL-friendly format.
  locs_id_sql <- paste0(
    '"',
    gsub('"', '""', locs_id, fixed = TRUE),
    '"'
  )
  # ----------------------------------------------------------------------------
  # Define SQL code for processing HMS data.
  sql_processhms <- glue::glue(
    "
      SELECT
        db_locs.{locs_id_sql} AS {locs_id_sql},
        CAST($1 AS DATE) AS date,

        MAX(CASE
          WHEN LOWER(TRIM(db_hms.\"Density\")) = 'light' THEN 1
          ELSE 0
        END) AS light,

        MAX(CASE
          WHEN LOWER(TRIM(db_hms.\"Density\")) = 'medium' THEN 1
          ELSE 0
        END) AS medium,

        MAX(CASE
          WHEN LOWER(TRIM(db_hms.\"Density\")) = 'heavy' THEN 1
          ELSE 0
        END) AS heavy

      FROM db_locs
      LEFT JOIN db_hms
        ON ST_Intersects(db_locs.geometry, db_hms.wkb_geometry)
      GROUP BY db_locs.{locs_id_sql}
    "
  )
  # ----------------------------------------------------------------------------
  # Convert extraction locations to a SedonaDB data frame.
  db_locs <- sedonadb::as_sedonadb_dataframe(locs)
  # ----------------------------------------------------------------------------
  # Register the extraction point data object in the SedonaDB session ! once !.
  sedonadb::sd_to_view(
    db_locs,
    "db_locs",
    overwrite = TRUE
  )
  # ----------------------------------------------------------------------------
  # Split date row indices into batches.
  list_hms_batch <- split(
    seq_len(nrow(df_hmspaths)),
    ceiling(seq_len(nrow(df_hmspaths)) / batch)
  )
  # ----------------------------------------------------------------------------
  # Generate a database view name for each batch.
  chr_hms_batch <- sprintf(
    "db_batch_%03d",
    seq_along(list_hms_batch)
  )
  # ----------------------------------------------------------------------------
  # Perform daily spatial joins within each batch.
  for (l in seq_along(list_hms_batch)) {
    # --------------------------------------------------------------------------
    # Index data files, dates, and names to active batch.
    df_hms_batch <- df_hmspaths[
      list_hms_batch[[l]],
      ,
      drop = FALSE
    ]
    # --------------------------------------------------------------------------
    # Generate daily view names for active batch.
    chr_hms_daily <- sprintf("db_day_%03d", seq_len(nrow(df_hms_batch)))
    # --------------------------------------------------------------------------
    message("Processing batch ", l, "/", length(list_hms_batch))
    for (i in seq_len(nrow(df_hms_batch))) {
      # ------------------------------------------------------------------------
      # Read HMS data shapefile into the database.
      db_hms <- sedonadb::sd_read_sf(
        df_hms_batch$path[i],
        lazy = FALSE
      )
      # ------------------------------------------------------------------------
      # Register polygons in the database.
      sedonadb::sd_to_view(
        db_hms,
        "db_hms",
        overwrite = TRUE
      )
      # ------------------------------------------------------------------------
      # Compute the spatial join.
      db_date <- sedonadb::sd_sql(
        sql_processhms,
        params = list(as.character(df_hms_batch$date[i]))
      ) |>
        sedonadb::sd_compute()
      # ------------------------------------------------------------------------
      # Register post-spatial join data in the database.
      sedonadb::sd_to_view(
        db_date,
        chr_hms_daily[i],
        overwrite = TRUE
      )
    }
    # --------------------------------------------------------------------------
    # Materialize and union the batch before removing its daily views.
    db_batch <- sedonadb::sd_sql(
      hmsunion(locs_id_sql, chr_hms_daily)
    ) |>
      sedonadb::sd_compute()
    # --------------------------------------------------------------------------
    # Register batch data in the database.
    sedonadb::sd_to_view(
      db_batch,
      chr_hms_batch[l],
      overwrite = TRUE
    )
    # --------------------------------------------------------------------------
    # Remove daily views now represented by the registered batch.
    for (d in chr_hms_daily) {
      sedonadb::sd_drop_view(d)
    }
  }
  # ----------------------------------------------------------------------------
  # Remove batch views when the function exits.
  on.exit(
    for (d in chr_hms_batch) {
      sedonadb::sd_drop_view(d)
    },
    add = TRUE
  )
  # ----------------------------------------------------------------------------
  # Combine and sort registered batches, and return to R.
  message("Combining and collecting ", length(chr_hms_batch), " batches...")
  sedonadb::sd_sql(
    paste0(
      hmsunion(locs_id_sql, chr_hms_batch),
      "\nORDER BY ",
      locs_id_sql,
      ", date"
    )
  ) |>
    sedonadb::sd_collect()
}

################################################################################
# Define SQL code for stacking daily or batch results.
hmsunion <- function(l, v) {
  paste(
    sprintf(
      "SELECT %s, date, light, medium, heavy FROM %s",
      l,
      v
    ),
    collapse = "\nUNION ALL\n"
  )
}

################################################################################
# Function for extracting dates from HMS shapefile file paths.
hmsdates <- function(x) {
  chr_hmssplit <- gsub("\\.shp", "", x)
  chr_hmssub <- substr(
    chr_hmssplit,
    nchar(chr_hmssplit) - 7,
    nchar(chr_hmssplit)
  )
  paste0(
    substr(chr_hmssub, 1, 4),
    "-",
    substr(chr_hmssub, 5, 6),
    "-",
    substr(chr_hmssub, 7, 8)
  )
}

################################################################################
# Process and calculate covariates from NASA GEOS-CF data with SedonaDB backend.
sedona_geos <- function(
  date,
  path,
  variable,
  locs,
  locs_id,
  radius,
  geom,
  fun = "mean",
  touches = TRUE,
  exclude_nodata = TRUE,
  batch = 50L
) {
  # ----------------------------------------------------------------------------
  # Validate batch size.
  stopifnot(
    length(batch) == 1L,
    is.numeric(batch),
    !is.na(batch),
    is.finite(batch),
    batch >= 1,
    batch == floor(batch)
  )
  # ----------------------------------------------------------------------------
  # Check zonal statistic function against SedonaDB supported (as of 10/21/26)
  fun <- match.arg(
    tolower(fun),
    c(
      "mean",
      "sum",
      "min",
      "max",
      "median",
      "count",
      "stddev",
      "variance",
      "mode"
    )
  )
  # ----------------------------------------------------------------------------
  # Derive extraction location geometries.
  chr_locsgeom <- as.character(unique(sf::st_geometry_type(locs)))
  # ----------------------------------------------------------------------------
  # Identify GEOS file paths from all files in the path.
  chr_paths <- list.files(
    path,
    pattern = "GEOS-CF.v01.rpl",
    full.names = TRUE,
    recursive = TRUE
  )
  # ----------------------------------------------------------------------------
  # Generate sequence from user supplied dates.
  chr_datesofinterest <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  # ----------------------------------------------------------------------------
  # Select only those file paths which align with user supplied date sequence.
  chr_datapaths <- unique(
    grep(
      paste(
        chr_datesofinterest,
        collapse = "|"
      ),
      chr_paths,
      value = TRUE
    )
  )
  # ----------------------------------------------------------------------------
  # Store file path, NetCDF variable path, date, and generated db names.
  df_geospaths <- data.frame(
    path = chr_datapaths,
    time = amadeus::process_collection(
      chr_datapaths,
      source = "geos",
      datetime = TRUE
    )
  )
  df_geospaths$date <- as.character(as.POSIXct(
    as.character(df_geospaths$time),
    format = "%Y%m%d%H%M",
    tz = "UTC"
  ))
  df_geospaths$var <- sprintf(
    'NETCDF:"%s":%s',
    normalizePath(df_geospaths$path, winslash = "/", mustWork = TRUE),
    variable
  )
  df_geospaths$view <- sprintf("db_geos_%012s", df_geospaths$time)
  # ----------------------------------------------------------------------------
  # Check dates, paths, and db names are set, and all exist with no NAs/NULLs.
  stopifnot(
    nrow(df_geospaths) > 0L,
    all(c("time", "path", "view") %in% names(df_geospaths)),
    !anyNA(df_geospaths$time),
    !anyDuplicated(df_geospaths$time),
    all(file.exists(df_geospaths$path)),
    !anyNA(df_geospaths$view),
    !anyDuplicated(df_geospaths$view)
  )
  # ----------------------------------------------------------------------------
  # Define location identifiers in SQL-friendly format.
  locs_id_sql <- paste0(
    '"',
    gsub('"', '""', locs_id, fixed = TRUE),
    '"'
  )
  # ----------------------------------------------------------------------------
  # Define variable identifiers in SQL-friendly format.
  variable_sql <- paste0(
    '"',
    gsub('"', '""', variable, fixed = TRUE),
    '"'
  )
  # ----------------------------------------------------------------------------
  # Define SQL code for processing GEOS data for points or polygons.
  sql_extraction <- if (chr_locsgeom == "POLYGON") {
    "
      RS_ZonalStats(
        r.raster,
        db_locs.geometry,
        1,
        $3,
        ($4 = 1),
        ($5 = 1)
      )
    "
  } else {
    "RS_Value(r.raster, db_locs.geometry, 1)"
  }

  sql_processgeos <- glue::glue(
    "
      WITH r AS (
        SELECT
          RS_SetCRS(
            RS_FromPath($1),
            'OGC:CRS84'
          ) AS raster
      )
      SELECT
        db_locs.{locs_id_sql} AS {locs_id_sql},
        CAST($2 AS TEXT) AS time,
        {sql_extraction} AS {variable_sql}
      FROM db_locs
      CROSS JOIN r
    "
  )
  # ----------------------------------------------------------------------------
  # Convert extraction points to SedonaDB data frame.
  db_locs <- sedonadb::as_sedonadb_dataframe(locs)
  # ----------------------------------------------------------------------------
  # Register the extraction point data object in the SedonaDB session ! once !.
  sedonadb::sd_to_view(
    db_locs,
    "db_locs",
    overwrite = TRUE
  )
  # ----------------------------------------------------------------------------
  # Split hourly file row indices into `batch` sized batches.
  list_geos_batch <- split(
    seq_len(nrow(df_geospaths)),
    ceiling(seq_len(nrow(df_geospaths)) / batch)
  )
  # ----------------------------------------------------------------------------
  # Iterative naming for database batch view objects.
  chr_geos_batch <- sprintf(
    "db_batch_%03d",
    seq_along(list_geos_batch)
  )
  # ----------------------------------------------------------------------------
  # Perform spatial extraction within each batch.
  for (l in seq_along(list_geos_batch)) {
    # --------------------------------------------------------------------------
    # Index data files, times, and names to active batch.
    df_geos_batch <- df_geospaths[
      list_geos_batch[[l]],
      ,
      drop = FALSE
    ]
    # --------------------------------------------------------------------------
    # Generate daily view names for active batch.
    chr_geos_hourly <- sprintf(
      "db_time_%03d",
      seq_len(nrow(df_geos_batch))
    )
    message("Processing batch ", l, "/", length(list_geos_batch))
    for (i in seq_len(nrow(df_geos_batch))) {
      # ------------------------------------------------------------------------
      # Time extraction directly from NetCDF not supported (as of 10/21/26)
      list_params <- list(
        as.character(df_geos_batch$var[i]), # $1: raster source
        df_geos_batch$date[i] # $2: timestamp
      )
      # ------------------------------------------------------------------------
      # Include additional arguments for zonal statistics.
      if (chr_locsgeom == "POLYGON") {
        list_params <- c(
          list_params,
          list(
            fun, # $3: statistic
            as.integer(touches), # $4: 0/1
            as.integer(exclude_nodata) # $5: 0/1
          )
        )
      }
      # ------------------------------------------------------------------------
      # Compute and register the hourly extraction.
      db_time <- sedonadb::sd_sql(
        sql_processgeos,
        params = list_params
      ) |>
        sedonadb::sd_compute()

      sedonadb::sd_to_view(
        db_time,
        chr_geos_hourly[i],
        overwrite = TRUE
      )
    }
    # --------------------------------------------------------------------------
    # Materialize and union the batch before removing its daily views.
    db_batch <- sedonadb::sd_sql(
      geosunion(locs_id_sql, variable_sql, chr_geos_hourly)
    ) |>
      sedonadb::sd_compute()
    # --------------------------------------------------------------------------
    # Register batch data in the database.
    sedonadb::sd_to_view(
      db_batch,
      chr_geos_batch[l],
      overwrite = TRUE
    )
    # --------------------------------------------------------------------------
    # Remove daily views now represented by the registered batch.
    for (h in chr_geos_hourly) {
      sedonadb::sd_drop_view(h)
    }
  }
  # ----------------------------------------------------------------------------
  # Remove batch views when the function exits.
  on.exit(
    for (d in chr_geos_batch) {
      sedonadb::sd_drop_view(d)
    },
    add = TRUE
  )
  # ----------------------------------------------------------------------------
  # Combine materialized batches, sort once, and return to R.
  message("Combining and collecting ", length(chr_geos_batch), " batches...")
  sedonadb::sd_sql(
    paste0(
      geosunion(locs_id_sql, variable_sql, chr_geos_batch),
      "\nORDER BY ",
      locs_id_sql,
      ", time"
    )
  ) |>
    sedonadb::sd_collect()
}

################################################################################
# Define SQL code for stacking hourly or batch results.
geosunion <- function(l, var, views) {
  paste(
    sprintf(
      "SELECT %s, time, %s FROM %s",
      l,
      var,
      views
    ),
    collapse = "\nUNION ALL\n"
  )
}
