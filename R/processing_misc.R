
#' Load county sf object
#' @keywords Calculation
#' @param year integer(1). Year of the county shapefile.
#' @param exclude character. State FIPS codes to exclude.
#' Default is c("02", "15", "60", "66", "68", "69", "72", "78").
#' @return sf object
#' @importFrom tigris counties
process_counties <-
  function(
    year = 2020,
    exclude = c("02", "15", "60", "66", "68", "69", "72", "78")
  ) {
    options(tigris_use_cache = TRUE)
    cnty <- tigris::counties(year = year)
    cnty <-
      cnty[!cnty$STATEFP %in%
           c("02", "15", "60", "66", "68", "69", "72", "78"), ]
    return(cnty)
  }


#' Get Divisors
#' @keywords Miscellaneous
#' @param x integer(1). A positive integer.
#' @return A vector of divisors of x.
divisor <-
  function(x) {
    xv <- seq_len(x)
    xv[which(x %% xv == 0)]
  }
