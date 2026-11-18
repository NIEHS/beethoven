################################################################################
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

################################################################################
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

################################################################################
