#' Add Time Column
#'
#' This function adds a time column to a data frame.
#'
#' @keywords Post-calculation
#' @param df The data frame to which the time column will be added.
#' @param time_value The value to be assigned to the time column.
#' @param time_id The name of the time column (default is "time").
#'
#' @return The data frame with the added time column.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' add_time_col(df, "2022-01-01")
#' }
#' @export
add_time_col <- function(df, time_value, time_id = "time") {
  if (!time_id %in% names(df)) {
    df[[time_id]] <- time_value
  }
  return(df)
}


#' Merge input data.frame objects
#' @param by character. Joining keys. See [`merge`] for details.
#' @param time logical(1). Whether or not include time identifier.
#' Set this `TRUE` will supersede `by` value by appending time identifier.
#' @param ... data.frame objects to merge
#' @return data.table
#' @keywords Post-calculation
#' @importFrom data.table as.data.table
#' @importFrom data.table merge.data.table
#' @export
post_calc_merge_features <-
  function(
    by = c("site_id"),
    time = FALSE,
    ...
  ) {
    ellipsis <- list(...)
    if (time) {
      by <- c("site_id", "time")
      ellipsis_clean <-
        lapply(
          ellipsis,
          function(x) {
            x <- data.table::as.data.table(x)
            col_coords <- grep("(lon|lat)", names(x))
            if (length(col_coords) > 0 && !is.null(col_coords)) {
              x <- x[, -col_coords, with = FALSE]
            }
            x$time <- as.character(x$time)
            return(x)
          }
        )
    } else {
      ellipsis_clean <- ellipsis
    }
    joined <-
      Reduce(
        function(x, y) {
          data.table::merge.data.table(
            x,
            y,
            by = by,
            all.x = TRUE,
            suffixes = c("_Ma", "_Mb")
          )
        },
        ellipsis_clean
      )
    return(joined)
  }


#' Change time column name
#' @param df data.frame
#' @param candidates character. Candidate column names.
#' @param replace character. New column name.
#' @return data.frame
#' @keywords Post-calculation
#' @export
post_calc_unify_timecols <-
  function(
    df,
    candidates = c("year"),
    replace = "time"
  ) {
    if (sum(names(df) %in% candidates) > 1) {
      stop("More than a candidate is detected in the input.")
    }
    names(df)[names(df) %in% candidates] <- replace
    return(df)
  }


#' Convert time column to character
#' @keywords Post-calculation
#' @param df data.table
#' @note This function takes preprocessed data.table with
#'   a column named `"time"`.
#' @importFrom data.table as.data.table copy
#' @importFrom data.table `:=`
#' @export
post_calc_convert_time <-
  function(
    df
  ) {
    stopifnot("time" %in% names(df))
    df1 <- data.table::copy(data.table::as.data.table(df))
    df1[, time := as.character(time)]
    return(df1)
  }


#' Join a data.frame with a year-only date column to
#'  that with a full date column
#' @description The full date column will be converted to a year column
#' as a new column, then the data.frame with the year-only column will
#' be joined.
#' @keywords Post-calculation
#' @param df_year data.frame with a year-only date column
#' @param df_date data.frame with a full date column
#' @param field_year character(1). Year column in `df_year`
#' @param field_date character(1). Date column in `df_date`
#' @param spid character(1). Name of the unique location identifier field.
#' @importFrom methods is
#' @importFrom data.table merge.data.table
#' @importFrom data.table `:=`
#' @return data.frame
post_calc_join_yeardate <-
  function(
    df_year,
    df_date,
    field_year = "time",
    field_date = "time",
    spid = "site_id"
  ) {
    if (!inherits(df_year, "data.frame") && !inherits(df_date, "data.frame")) {
      stop("Both inputs should be data.frame.")
    }

    names(df_year)[which(names(df_year) %in% field_year)] <- "year"
    df_year$year <- as.character(unlist(df_year$year))
    df_date$year <- as.character(substr(df_date[[field_date]], 1, 4))

    df_joined <-
      data.table::merge.data.table(
        df_date,
        df_year,
        by = c(spid, "year"),
        all.x = TRUE
      )

    df_joined <- df_joined[, c("year") := NULL]
    return(df_joined)
  }


# 2018~2022, 2017, 2020
# 2017 ... 2020 ...
# 2017
#' Map the available raw data years over the given period
#' @description
#' Many raw datasets are periodically updated and the period could
#' be longer than a year. This function maps the available years
#' over the given period.
#' @keywords Post-calculation
#' @param time_start integer(1). Starting year.
#' @param time_end integer(1). Ending year.
#' @param time_unit character(1). Time unit. Default is `"year"`.
#' @param time_available vector. Available years.
#' @return integer vector of length (time_end - time_start + 1).
#' Each element will get the nearest preceeding available year.
#' @note
#' The minimum of `time_available` will be filled in front of the first
#' available year when the minimum of `time_available` is greater
#' than `time_start`.
#' @examples
#' \dontrun{
#' process_calc_year_expand(2018, 2022, "year", c(2017, 2020, 2021))
#' process_calc_year_expand(2018, 2022, "year", c(2020, 2021))
#' }
#' @export
post_calc_year_expand <-
  function(
    time_start = NULL,
    time_end = NULL,
    time_unit = "year",
    time_available = NULL
  ) {
    time_seq <- seq(time_start, time_end)
    time_target_seq <- findInterval(time_seq, time_available)
    time_target_seq <- time_available[time_target_seq]
    if (min(time_available) > time_start) {
      time_target_seq <-
        c(
          rep(min(time_available), min(time_available) - time_start),
          time_target_seq
        )
    }
    return(time_target_seq)
  }


#' Expand a data frame by year
#'
#' This function expands a data frame by year, creating multiple rows
#'  for each year based on the time period specified.
#' @keywords Post-calculation
#' @param df The input data frame. The data frame should have the same
#' number of rows per year, meaning that it assumes this argument is
#' a spatial-only feature data.frame.
#' @param locs_id The column name of the location identifier in the data frame.
#' @param time_field The column name of the time field in the data frame.
#' @param time_start The start of the time period.
#' @param time_end The end of the time period.
#' @param time_unit The unit of time to expand the data frame. Only for record.
#' @param time_available A vector of available time periods.
#' @param ... Placeholders.
#' @note Year expansion rule is to assign the nearest past year
#' in the available years; if there is no past year in the available years,
#' the first available year is rolled back to the start of the time period.
#' @return The expanded data frame with multiple rows for each year.
#' @seealso [`post_calc_year_expand()`]
#' @examples
#' \dontrun{
#' df <- data.frame(year = c(2010, 2010, 2011, 2012),
#'                  value = c(1, 2, 3, 4))
#' df_expanded <-
#'   post_calc_df_year_expand(df, locs_id = "site_id", time_field = "year",
#'                            time_start = 2011, time_end = 2012,
#'                            time_unit = "year")
#' print(df_expanded)
#' }
#' @importFrom stats sd
#' @export
post_calc_df_year_expand <- function(
  df,
  locs_id = "site_id",
  time_field = "time",
  time_start = NULL,
  time_end = NULL,
  time_unit = "year",
  time_available = NULL,
  ...
) {
  time_summary <- table(unlist(df[[time_field]]))
  if (length(time_summary) != 1) {
    if (stats::sd(time_summary) != 0) {
      stop("df should be a data frame with the same number of rows per year")
    }
  }
  # assume that df is the row-bound data frame
  if (is.character(df[[time_field]])) {
    df[[time_field]] <- as.integer(df[[time_field]])
  }
  df_years <- unique(df[[time_field]])
  nlocs <- length(unique(df[[locs_id]]))
  year_period <- seq(time_start, time_end)
  # assign the time period to the available years
  year_assigned <-
    post_calc_year_expand(time_start, time_end, time_unit, df_years)
  df_years_repeats <- table(year_assigned)

  # repeat data frames
  df_expanded <- Map(
    function(y) {
      df_sub <- df[df[[time_field]] == df_years[y], ]
      df_sub <- df_sub[rep(seq_len(nrow(df_sub)), df_years_repeats[y]), ]
      return(df_sub)
    },
    seq_along(year_assigned)
  )
  df_expanded <- do.call(rbind, df_expanded)
  df_expanded[[time_field]] <- rep(year_period, each = nlocs)
  return(df_expanded)
}


#' Merge spatial and spatiotemporal covariate data
#' @keywords Post-calculation
#' @param locs Location. e.g., AQS sites.
#' @param locs_id character(1). Location identifier.
#' @param time_id character(1). Location identifier.
#' @param target_years integer. Used to dummify nominal year.
#' @param df_sp data.frame. Spatial-only covariates.
#' @param df_spt data.frame. Spatiotemporal covariates.
#' @note This version assumes the time_id contains Date-like strings.
#' @return data.frame
#' @importFrom data.table merge.data.table
#' @importFrom amadeus calculate_temporal_dummies
#' @export
post_calc_merge_all <-
  function(
    locs,
    locs_id,
    time_id,
    target_years = seq(2018, 2022),
    df_sp,
    df_spt
  ) {
    if (methods::is(locs, "sf")) {
      locs <- sf::st_drop_geometry(locs)
    }
    locs$time <- as.character(locs$time)
    locs <- data.table::as.data.table(locs)
    locs_merged <-
      data.table::merge.data.table(
        locs,
        df_sp,
        by = c(locs_id)
      )
    locs_merged <-
      data.table::merge.data.table(
        locs_merged,
        df_spt,
        by = c(locs_id, time_id)
      )
    # need POSIXt class for amadeus function
    locs_merged[[time_id]] <- as.POSIXct(locs_merged[[time_id]])
    locs_merged <-
      amadeus::calculate_temporal_dummies(
        locs = locs_merged,
        locs_id = locs_id,
        year = target_years
      )
    # reset time as character
    locs_merged[[time_id]] <- as.character(locs_merged[[time_id]])
    return(locs_merged)
  }


#' Remove columns from a data frame based on regular expression patterns.
#' @keywords Post-calculation
#'
#' This function removes columns from a data frame that match
#'  any of the specified
#' regular expression patterns. By default, it removes columns with names that
#' match the patterns "^lon$|^lat$|geoid|year$|description".
#'
#' @param df The input data frame.
#' @param candidates A character vector of regular expression patterns
#'   to match against column names. Columns that match any of the patterns
#'   will be removed. The default value is
#'   "^lon$|^lat$|geoid|year$|description".
#' @param strict logical(1). If `TRUE`,
#'   only `c("site_id", "time")` will be kept.
#' @return The modified data frame with the specified columns removed.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(lon = 1:5, lat = 6:10, geoid = 11:15, year = 2010:2014,
#'                  description = letters[1:5], other = 16:20)
#' post_calc_drop_cols(df)
#' }
#' @export
post_calc_drop_cols <-
  function(
    df,
    candidates = "(^lon$|^lat$|geoid|year$|description|geometry)",
    strict = FALSE
  ) {
    idx_remove <-
      if (!strict) {
        grep(candidates, names(df), value = TRUE)
      } else {
        grep("site_id|time", names(df), value = TRUE, invert = TRUE)
      }
    df <- df[, -idx_remove, with = FALSE]
    return(df)
  }


#' Automatic joining by the time and spatial identifiers
#' @description The key assumption is that all data frames will have
#' time field and spatial field and the data should have one of date or year.
#' Whether the input time unit is year or date
#' is determined by the coercion of the **first row value** of the time field
#' into a character with `as.Date()`. This function will fail if it
#' gets year-like string with length 4.
#'
#' @param df_fine The fine-grained data frame.
#' @param df_coarse The coarse-grained data frame.
#' @param field_sp The name of the spatial field in the data frames.
#' @param field_t The name of the time field in the data frames.
#' @param year_start The starting year of the time period.
#' @param year_end The ending year of the time period.
#' @keywords Post-calculation
#' @return A merged data table.
#' @examples
# nolint start
#' \dontrun{
#' df_fine0 <- data.frame(site_id = c("A", "B", "B", "C"),
#'                       time = as.Date(c("2022-01-01", "2022-01-02", "2021-12-31", "2021-01-03")),
#'                       value = c(1, 2, 3, 5))
#' df_coarse0 <- data.frame(site_id = c("A", "B", "C"),
#'                         time = c("2022", "2022", "2021"),
#'                         other_value = c(10, 20, 30))
#' jdf <- post_calc_autojoin(df_fine0, df_coarse0)
#' print(jdf)
#' }
# nolint end
#' @importFrom data.table merge.data.table
#' @importFrom rlang as_name sym
#' @export
post_calc_autojoin <-
  function(
    df_fine,
    df_coarse,
    field_sp = "site_id",
    field_t = "time",
    year_start = 2018L,
    year_end = 2022L
  ) {
    if (any(grepl("population", names(df_coarse)))) {
      df_coarse <- df_coarse[, -c("time"), with = FALSE]
    }
    common_field <- intersect(names(df_fine), names(df_coarse))
    df_fine <- data.table::as.data.table(df_fine)
    df_coarse <- data.table::as.data.table(df_coarse)
    df_fine <- post_calc_drop_cols(df_fine)
    df_coarse <- post_calc_drop_cols(df_coarse)
    # if (length(common_field) > 2) {
    #   message("The data frames have more than two common fields.")
    #   message("Trying to remove the redundant common fields...")
    #   common_field <- intersect(names(df_fine), names(df_coarse))
    #   print(common_field)
    #   common_field <-
    #     common_field[-which(!common_field %in% c(field_sp, field_t))]
    # }
    if (length(common_field) == 1) {
      print(common_field)
      if (common_field == field_sp) {
        joined <- data.table::merge.data.table(
          df_fine,
          df_coarse,
          by = field_sp,
          all.x = TRUE
        )
      }
    }
    if (length(common_field) == 2) {
      if (all(common_field %in% c(field_sp, field_t))) {
        # t_fine <- try(as.Date(df_fine[[field_t]][1]))
        df_fine[[field_t]] <- as.character(df_fine[[field_t]])
        df_coarse[[field_t]] <- as.character(df_coarse[[field_t]])
        t_coarse <- try(as.Date(df_coarse[[field_t]][1]))
        if (inherits(t_coarse, "try-error")) {
          message(
            "The time field includes years. Trying different join strategy."
          )
          # derive the available years from the coarsely resolved data
          coarse_years <- sort(unique(unlist(as.integer(df_coarse[[field_t]]))))
          df_coarse2 <- post_calc_df_year_expand(
            df_coarse,
            time_start = year_start,
            time_end = year_end,
            time_available = coarse_years
          )
          joined <-
            post_calc_join_yeardate(df_coarse2, df_fine, field_t, field_t)
        } else {
          joined <- data.table::merge.data.table(
            df_fine,
            df_coarse,
            by = c(field_sp, field_t),
            all.x = TRUE
          )
        }
      }
    }
    return(joined)
  }


#' Impute missing values and attach lagged features
#' @keywords Post-calculation
#' @note
#' This function performs imputation on a given data table
#'   by replacing missing values with imputed values.
#'   It follows a series of steps including data cleaning, name cleaning,
#'   geoscf column renaming, NDVI 16-day backward filling,
#'   zero-variance exclusion, excessive "true zeros" exclusion,
#'   and imputation using missRanger.
#' A few points should be discussed to sophisticate the imputation
#' process: exclusion threshold for rates of zero observations,
#' which might lead to significant improvement in the imputation
#' process especially in terms of speed and accuracy.
#' @param dt The input data table to be imputed.
#' @param period The period for lagged features.
#' @param nthreads_dt The number of threads to be used for
#'   data.table operations.
#' @param nthreads_collapse The number of threads to be used for
#'   collapse operations.
#' @param nthreads_imputation The number of threads to be used for
#'   the imputation process.
#'
#' @return The imputed data table with lagged features.
#'
#' @importFrom collapse set_collapse replace_inf replace_na fvar fnth
#' @importFrom data.table setDTthreads setnafill
#' @importFrom qs qread
#' @importFrom stats setNames
#' @importFrom stringi stri_replace_all_regex
#' @importFrom missRanger missRanger
#' @export
impute_all <-
  function(
    dt,
    period,
    nthreads_dt = 32L,
    nthreads_collapse = 32L,
    nthreads_imputation = 32L
  ) {
    data.table::setDTthreads(nthreads_dt)
    if (is.character(dt)) {
      if (!endsWith(dt, ".qs")) {
        stop(
          paste0(
            "If `dt` points to a file, provide full path to .qs file.\n"
          )
        )
      }
      dt <- qs::qread(file.path(dt))
    }
    dt$time <- as.POSIXct(dt$time)
    # remove unnecessary columns
    query <- "^(site_id|time)\\.[0-9]+"
    dt <- dt[, !grepl(query, names(dt)), with = FALSE]

    # name cleaning
    dt <- stats::setNames(
      dt,
      sub("light_00000", "OTH_HMSWL_0_00000", names(dt))
    )
    dt <- stats::setNames(
      dt,
      sub("medium_00000", "OTH_HMSWM_0_00000", names(dt))
    )
    dt <- stats::setNames(
      dt,
      sub("heavy_00000", "OTH_HMSWH_0_00000", names(dt))
    )
    dt <- stats::setNames(
      dt,
      sub("population_", "POP_SEDAC_0_", names(dt))
    )

    geoscn <-
      "ACET\tGEO_ACETO_0_00000
    ALD2\tGEO_ACETA_0_00000
    ALK4\tGEO_CALKA_0_00000
    BCPI\tGEO_HIBCA_0_00000
    BCPO\tGEO_HOBCA_0_00000
    BENZ\tGEO_BENZE_0_00000
    C2H6\tGEO_ETHTE_0_00000
    C3H8\tGEO_PROPA_0_00000
    CH4\tGEO_METHA_0_00000
    CO\tGEO_CMONO_0_00000
    DST1\tGEO_DUST1_0_00000
    DST2\tGEO_DUST2_0_00000
    DST3\tGEO_DUST3_0_00000
    DST4\tGEO_DUST4_0_00000
    EOH\tGEO_ETHOL_0_00000
    H2O2\tGEO_HYPER_0_00000
    HCHO\tGEO_FORMA_0_00000
    HNO3\tGEO_NITAC_0_00000
    HNO4\tGEO_PERAC_0_00000
    ISOP\tGEO_ISOPR_0_00000
    MACR\tGEO_METHC_0_00000
    MEK\tGEO_MEKET_0_00000
    MVK\tGEO_MVKET_0_00000
    N2O5\tGEO_DIPEN_0_00000
    NH3\tGEO_AMNIA_0_00000
    NH4\tGEO_AMNUM_0_00000
    NIT\tGEO_INNIT_0_00000
    NO\tGEO_NIOXI_0_00000
    NO2\tGEO_NIDIO_0_00000
    NOy\tGEO_NITRO_0_00000
    OCPI\tGEO_HIORG_0_00000
    OCPO\tGEO_HOORG_0_00000
    PAN\tGEO_PERNI_0_00000
    PM25_RH35_GCC\tGEO_PM25X_0_00000
    PM25_RH35_GOCART\tGEO_PM25R_0_00000
    PM25bc_RH35_GCC\tGEO_BLCPM_0_00000
    PM25du_RH35_GCC\tGEO_DUSPM_0_00000
    PM25ni_RH35_GCC\tGEO_NITPM_0_00000
    PM25oc_RH35_GCC\tGEO_ORCPM_0_00000
    PM25soa_RH35_GCC\tGEO_SORPM_0_00000
    PM25ss_RH35_GCC\tGEO_SEAPM_0_00000
    PM25su_RH35_GCC\tGEO_SULPM_0_00000
    PRPE\tGEO_CALKE_0_00000
    RCHO\tGEO_CALDH_0_00000
    SALA\tGEO_FSEAS_0_00000
    SALC\tGEO_CSEAS_0_00000
    SO2\tGEO_SULDI_0_00000
    SOAP\tGEO_SOAPR_0_00000
    SOAS\tGEO_SOASI_0_00000
    TOLU\tGEO_TOLUE_0_00000
    XYLE\tGEO_XYLEN_0_00000
    CO_y\tGEO_COVMR_0_00000
    NO2_y\tGEO_NOVMR_0_00000
    O3\tGEO_OZVMR_0_00000
    SO2_y\tGEO_SOVMR_0_00000"

    geoscn <- strsplit(geoscn, "\n")
    geoscn <- unlist(geoscn)
    geoscn <- strsplit(geoscn, "\t")
    geoscn <- do.call(rbind, geoscn)
    geoscndf <- as.data.frame(geoscn, stringsAsFactors = FALSE)
    colnames(geoscndf) <- c("variable", "code")
    geoscndf$variable <- trimws(geoscndf$variable)

    for (i in seq_len(nrow(geoscndf))) {
      dt <-
        setNames(
          dt,
          stringi::stri_replace_all_regex(
            names(dt),
            sprintf("%s$", geoscndf$variable[i]),
            geoscndf$code[i]
          )
        )
    }
    site_id <- NULL
    # NDVI 16-day
    # For each site_id, backward filling for 16-day NDVI
    # Last Observation Carried Forward is the method used;
    # it assumes that the rows are ordered by date
    dt <- dt[order(site_id, time), ]
    col_ndviv <- grep("MOD_NDVIV_", names(dt))
    dtndviv <-
      data.table::setnafill(
        dt,
        type = "nocb",
        nan = NA,
        cols = col_ndviv
      )

    collapse::set_collapse(mask = "manip", nthreads = nthreads_collapse)

    target_replace <- grep("^MOD_", names(dt), invert = TRUE)
    dt <- collapse::replace_inf(dtndviv, value = NA, replace.nan = TRUE)
    dt <- collapse::replace_na(dt, value = 0, cols = target_replace)

    # zero-variance exclusion
    dt_colvars <- collapse::fvar(dt[, 5:ncol(dt), with = FALSE])
    zero_var_fields <- names(dt_colvars[dt_colvars == 0])

    # Exclude fields with zero variance using data.table
    dt <- dt[, (zero_var_fields) := NULL]

    # Store the name of zero variance fields as an attribute of the input object
    attr(dt, "zero_var_fields") <- zero_var_fields

    # excluding columns with excessive "true zeros"
    # we should have a threshold for the zero rate
    # exc_zero <- collapse::fnth(dt[, 5:ncol(dt), with = FALSE], n = 0.9)
    # exc_zero <- unname(which(exc_zero == 0)) + 5L
    # dt <- dt[, (exc_zero) := NULL]

    # Q: Do we use all other features to impute? -- Yes.
    # 32-thread, 10% for tree building, 200 trees, 4 rounds: 11 hours
    imputed <-
      missRanger::missRanger(
        data = dt,
        maxiter = 10L,
        num.trees = 300L,
        num.threads = nthreads_imputation,
        mtry = 100L,
        sample.fraction = 0.1
      )

    imputed <- amadeus::calculate_temporal_dummies(imputed, "time")
    return(imputed)
    # lagged features: changing period (period[1] + 1 day)
    # period <- as.Date(period)
    # period[1] <- period[1] + as.difftime(1, units = "days")
    # period <- as.character(period)
    # index_lag <-
    #   sprintf("MET_%s", c("ATSFC", "ACPRC", "PRSFC", "SPHUM", "WNDSP"))
    # index_lag <- grep(paste(index_lag, collapse = "|"), names(dt))
    # target_lag <- imputed[, index_lag, with = FALSE]

    # output <- amadeus::calculate_lagged(target_lag, period, 1, "site_id")
    # return(output)
  }


#' Append Predecessors
#'
#' This function appends predecessors to an existing object or
#'  creates a new object if none exists.
#'
#' @keywords Post-calculation
#' @param path_qs The path where the predecessors will be stored.
#' @param period_new The new period to be appended.
#' @param input_new The new input object to be appended.
#' @param nthreads The number of threads to be used.
#'
#' @return If no existing predecessors are found, the function saves
#'   the new input object and returns the name of the saved file.
#'   If existing predecessors are found, the function appends
#'   the new input object to the existing ones and returns the combined object.
#' @export
append_predecessors <-
  function(
    path_qs = "output/qs",
    period_new = NULL,
    input_new = NULL,
    nthreads = 8L
  ) {
    if (is.null(input_new)) {
      stop("Please provide a valid object.")
    }
    if (!dir.exists(path_qs)) {
      dir.create(path_qs, recursive = TRUE)
    }
    input_old <- list.files(path_qs, "*.*.qs$", full.names = TRUE)

    # validate input_old with period_new
    # if (length(input_old) > 0) {
    # periods_old <- do.call(rbind, strsplit(input_old, "_"))
    # periods_old <- periods_old[, 4:5]
    # periods_old_check <- vapply(
    #   seq(1, nrow(periods_old)),
    #   function(i) {
    #     period_old <- periods_old[i, ]
    #     period_old <- as.Date(period_old, format = "%Y-%m-%d")
    #     period_new <- as.Date(period_new, format = "%Y-%m-%d")
    #     if (period_new[1] < period_old[1] | period_new[2] < period_old[2]) {
    #       return(FALSE)
    #     } else {
    #       return(TRUE)
    #     }
    #   },
    #   logical(1)
    # )
    # if (!all(periods_old_check)) {
    #   stop("Results have an overlap period. Please provide a valid period.")
    # }
    # }
    period_new <- sapply(period_new, as.character)
    time_create <- gsub("[[:punct:]]|[[:blank:]]", "", Sys.time())
    name_qs <-
      sprintf(
        "dt_feat_pm25_%s_%s_%s.qs",
        period_new[1],
        period_new[2],
        time_create
      )
    if (length(input_old) == 0) {
      qs::qsave(input_new, file = file.path(path_qs, name_qs))
      return(name_qs)
    } else {
      # vv <- list()
      qs::qsave(input_new, file = file.path(path_qs, name_qs))
      input_update <- list.files(path_qs, "*.*.qs$", full.names = TRUE)
      bound_large <-
        Reduce(
          function(x, y) {
            if (inherits(x, "data.frame")) {
              bound <- rbind(x, qs::qread(y))
            } else {
              bound <- rbind(qs::qread(x), qs::qread(y))
            }
            return(bound)
          },
          input_update
        )
      return(bound_large)
    }
  }


#' Perform Principal Component Analysis
#' @keywords internal
#' @param data data.frame or data.table
#' @param num_comp integer(1). The number of components to retain as new
#' predictors. If `threshold` is defined, `num_comp` will be overridden.
#' @param threshold numeric(1). A fraction of the total variance that should
#' be covered by the components.
#' @param kernel logical(1). Whether to use a kernel PCA with
#' [`recipes::step_kpca()`]. Default is `FALSE`.
#' @seealso [`recipes::step_pca()`] [`recipes::step_kpca()`]
#' @importFrom recipes recipe bake prep step_normalize step_pca step_kpca
#' @importFrom magrittr %>%
#' @return data.table with Principal Components sufficient to satisfy the
#' `threshold`.`
#' @export
reduce_pca <- function(
  data,
  num_comp = 5,
  threshold = NA,
  kernel = FALSE
) {
  stopifnot(inherits(data, "data.frame"))
  stopifnot(is.numeric(num_comp))

  data_rec <- recipes::recipe(~., data = data)
  data_pca <- data_rec %>%
    recipes::step_normalize(recipes::all_numeric())

  if (kernel) {
    data_pca <- data_pca %>%
      recipes::step_kpca(
        recipes::all_numeric(),
        num_comp = num_comp
      )
  } else {
    data_pca <- data_pca %>%
      recipes::step_pca(
        recipes::all_numeric(),
        threshold = threshold,
        num_comp = num_comp
      )
  }

  data_prep <- recipes::prep(data_pca, data = data)
  data_pca <- recipes::bake(data_prep, new_data = data)

  return(data_pca)
}

#' Post-calculation Principal Component Analysis
#' @keywords Post-calculation
#' @description This function performs PCA on the input data frame to reduce
#' number of predictors.
#' @param data data.frame or data.table
#' @param locs_id The column name in the spatial object that represents the
#'   location identifier.
#' @param time_id The column name in the data frame that represents the time
#'   identifier.
#' @param yvar The target variable.
#' @param coords The column names that represent the XY coordinates. Default
#' is `c("lon", "lat")`.
#' @param num_comp integer(1). The number of components to retain as new
#' predictors.  If `threshold` is defined, `num_comp` will be overridden.
#' @param threshold numeric(1). A fraction of the total variance that should
#' be covered by the components.
#' @param pattern character(1). A regular expression pattern to match the
#' columns that should be included in the PCA.
#' @param groups character. A character vector of groups to perform PCA on.
#' Each character should be a regular expression pattern to match the columns
#' that should be included in the PCA. Default is `NULL`.
#' @param prefix character(1). A prefix to be added to the column names of the
#' Principal Components. Default is `NULL`.
#' @param kernel logical(1). Whether to use a kernel PCA with
#' [`recipes::step_kpca()`]. Default is `FALSE`.
#' @note  If `threshold` is defined, `num_comp` will be overridden.
#' @seealso [`reduce_pca()`] [`recipes::step_pca()`] [`recipes::step_kpca()`]
#' @importFrom data.table data.table
#' @return data.table with Principal Components sufficient to satisfy the
#' `threshold`, merged with `*_id` and `yvar` columns from original `data`.
#' @export
post_calc_pca <- function(
  data,
  locs_id = "site_id",
  time_id = "time",
  yvar = "Arithmetic.Mean",
  coords = c("lon", "lat"),
  num_comp = 5,
  threshold = NA,
  pattern = "FUGITIVE|STACK",
  groups = NULL,
  prefix = "PCA",
  kernel = FALSE
) {
  data <- data.table::data.table(data)
  chr_retaincols <- c(locs_id, time_id, yvar, coords)
  data_trim <- data[, chr_retaincols, with = FALSE]

  data_pca <- data[,
    grep(pattern, names(data)),
    with = FALSE
  ]

  if (is.null(groups)) {
    return_pca <- beethoven::reduce_pca(
      data = data_pca,
      threshold = threshold,
      num_comp = num_comp,
      kernel = kernel
    )
    names(return_pca) <- paste0(prefix, "_", names(return_pca))
  } else {
    list_pca <- list()
    for (g in seq_along(groups)) {
      data_group <- data_pca[,
        grep(groups[g], names(data_pca)),
        with = FALSE
      ]
      group_pca <- beethoven::reduce_pca(
        data = data_group,
        threshold = threshold,
        num_comp = num_comp,
        kernel = kernel
      )
      names(group_pca) <- paste0(
        prefix,
        "_",
        names(group_pca),
        "_",
        groups[g]
      )
      list_pca <- c(list_pca, group_pca)
    }
    return_pca <- do.call(cbind, list_pca)
  }

  stopifnot(nrow(data_trim) == nrow(return_pca))
  data_return <- data.table::data.table(cbind(data_trim, return_pca))

  return(data_return)
}

#' Post-calculation column renaming
#' @keywords Post-calculation
#' @description This function renames the columns of the input `data` based on
#' the `prefix` and original column names.
#' @param data data.frame(1)
#' @param prefix character(1). The prefix to be added to the column names.
#' @param skip character. The column names to be skipped from renaming.
#' Default is `c("site_id", "time")`.
#' @return data.frame with renamed columns.
#' @export
post_calc_cols <- function(
  data,
  prefix = NULL,
  skip = c("site_id", "time")
) {
  stopifnot(inherits(data, "data.frame"))

  chr_names <- names(data)
  chr_edit <- chr_names[!chr_names %in% skip]

  list_split <- strsplit(chr_edit, "_")
  list_update <- lapply(
    list_split,
    function(x) {
      paste0(
        prefix,
        toupper(x[[1]]),
        "_",
        sprintf("%05d", as.integer(x[[length(x)]]))
      )
    }
  )
  chr_update <- unlist(list_update)
  names(data)[match(chr_edit, chr_names)] <- chr_update
  return(data)
}
