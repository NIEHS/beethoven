#' Combine dynamically branched sublists based on common column names
#' @description
#' This function combines the sublists of dynamically branched targets object
#' as a preprocessor for `reduce_merge` function.
#' @keywords prediction
#' @param df_list list(1). List of data frames with common column names.
#' Typically the output of a dynamically branched target.
#' @return a list object, with data frames combined by common column names.
#' @keywords Utility
#' @export
reduce_list <- function(df_list) {
  # Create a named list to group data frames by their column names
  grouped_dfs <- list()

  # Iterate over each data frame in the list
  for (df in df_list) {
    # Get the column names of the current data frame
    col_names <- paste(sort(colnames(df)), collapse = ",")

    # Check if this set of column names already exists in the grouped list
    if (!col_names %in% names(grouped_dfs)) {
      grouped_dfs[[col_names]] <- list()
    }

    # Append the current data frame to the list for its column names
    grouped_dfs[[col_names]] <- append(grouped_dfs[[col_names]], list(df))
  }

  # Combine data frames with the same column names
  combined_list <- lapply(grouped_dfs, function(dfs) {
    do.call(rbind, dfs)
  })

  # Assign numeric names to the combined list
  names(combined_list) <- seq_along(combined_list)

  return(combined_list)
}

#' Split a date range into subranges
#' @description
#' Split a date range into subranges of equal length as a list.
#' @param dates character(2). date to query. `"YYYY-MM-DD"` format.
#' @param n integer(1). Number of dates in each subrange.
#' @param year logical(1). If `TRUE`, sublists will contain only one year. This
#' may result in sublists with fewer than `n` dates.
#' @param julian logical(1). If `TRUE`, dates are in Julian format.
#' @return a list object, with date ranges split into subranges.
#' @keywords Utility
#' @export
split_dates <- function(
  dates,
  n,
  year = TRUE,
  julian = FALSE
) {
  ##### full list of dates
  dates_full <- amadeus::generate_date_sequence(
    dates[1],
    dates[2],
    sub_hyphen = FALSE
  )

  ##### include only dates from the same year in each sublist item
  if (year) {
    u_years <- unique(substr(dates_full, 1, 4))
    list_split <- lapply(
      u_years,
      function(year) {
        dates_year <- grep(year, dates_full, value = TRUE)
        base::split(
          dates_year,
          ceiling(seq_along(dates_year) / n)
        )
      }
    )
    dates_split <- do.call(c, list_split)
    names(dates_split) <- seq(1, length(dates_split), 1)
  } else {
    dates_split <- base::split(
      dates_full,
      ceiling(seq_along(dates_full) / n)
    )
  }

  ##### convert to julian date format
  if (julian) {
    dates_julian <- lapply(dates_split, function(x) format(as.Date(x), "%Y%j"))
  } else {
    dates_julian <- dates_split
  }

  return(dates_julian)
}

#' Extract the first and last elements of a list
#' @param dates list. A list of dates.
#' @return a character vector with the first and last dates from the list.
#' @export
#' @keywords Utility
fl_dates <- function(
  dates
) {
  first <- dates[1]
  last <- dates[length(dates)]
  return(c(first, last))
}
