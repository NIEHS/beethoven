source("./input/Rinput/processing_functions/load_packages.R")
source("./input/Rinput/processing_functions/filter_unique_sites.R")

# filter unique sites
sites <- filter_unique_sites()
sites_st <- filter_unique_sites(include_time = TRUE)


#' Calculate temporal dummy variables
#' @input sites data.frame with a temporal field named "date"
#' @return data.frame with year, month, and weekday indicators.
#' @author Insang Song
#' @importFrom methods is
#' @importFrom data.table year
#' @importFrom data.table month
#' @importFrom data.table as.data.table
#' @export
calc_temporal_dummies <-
  function(
    sites
  ) {

  if (!methods::is(sites, "data.frame")) {
    stop("Argument sites is not a data.frame.\n")
  }
  if (!"date" %in% names(sites)) {
    stop("A mandatory field 'date' does not exist in sites.\n")
  }
  dummify <- function(vec) {
    vec_unique <- sort(unique(vec))
    vec_split <- split(vec_unique, vec_unique)
    vec_assigned <-
      lapply(vec_split,
            function(x) {
              as.integer(vec == x)
            })
    dt_dum <- Reduce(cbind, vec_assigned)
    dt_dum <- data.table::as.data.table(dt_dum)
    return(dt_dum)
  }

  # year
  vec_year <- data.table::year(sites$date)
  dt_year_dum <- dummify(vec_year)
  colnames(dt_year_dum) <-
    sprintf("DUM_Y%d_0_00000", seq(2018, 2022))


  # month
  vec_month <- data.table::month(sites$date)
  dt_month_dum <- dummify(vec_month)
  shortmn <-
    c("JANUA", "FEBRU", "MARCH", "APRIL",
      "MAYMA", "JUNEJ", "JULYJ", "AUGUS",
      "SEPTE", "OCTOB", "NOVEM", "DECEM")
  colnames(dt_month_dum) <-
    sprintf("DUM_%s_0_00000", shortmn)

  # weekday (starts from 1-Monday)
  vec_wday <- as.POSIXlt(sites$date)$wday
  dt_wday_dum <- dummify(vec_wday)
  colnames(dt_wday_dum) <-
    sprintf("DUM_WKDY%d_0_00000", seq(1, 7))


  # column binding
  sites_dums <-
    cbind(
      sites,
      dt_year_dum,
      dt_month_dum,
      dt_wday_dum
    )

  return(sites_dums)
  }

dummify <- function(vec) {
  vec_unique <- sort(unique(vec))
  vec_split <- split(vec_unique, vec_unique)
  vec_assigned <-
    lapply(vec_split,
           function(x) {
             as.integer(vec == x)
           })
  dt_dum <- Reduce(cbind, vec_assigned)
  dt_dum <- as.data.table(dt_dum)
  return(dt_dum)
}

# year
vec_year <- year(sites_st$date)
dt_year_dum <- dummify(vec_year)
colnames(dt_year_dum) <-
  sprintf("DUM_Y%d_0_00000", seq(2018, 2022))


# month
vec_month <- month(sites_st$date)
dt_month_dum <- dummify(vec_month)
shortmn <-
  c("JANUA", "FEBRU", "MARCH", "APRIL",
    "MAYMA", "JUNEJ", "JULYJ", "AUGUS",
    "SEPTE", "OCTOB", "NOVEM", "DECEM")
colnames(dt_month_dum) <-
  sprintf("DUM_%s_0_00000", shortmn)

# weekday (starts from 1-Monday)
vec_wday <- as.POSIXlt(sites_st$date)$wday
dt_wday_dum <- dummify(vec_wday)
colnames(dt_wday_dum) <-
  sprintf("DUM_WKDY%d_0_00000", seq(1, 7))


# column binding
sites_dums <-
  cbind(
    sites_st,
    dt_year_dum,
    dt_month_dum,
    dt_wday_dum
  )

saveRDS(sites_dums,
        file = "./output/NRTAP_Covars_Timedummies.rds",
        compress = "xz")
