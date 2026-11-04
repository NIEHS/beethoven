################################################################################
# Derive all time-related objects from initial date range definition.
# --1 chr_init_dates
# --2 int_init_years
# --3 list_init_dates
# --4 list_init_datesj

################################################################################
# Load libraries and source local functions.
library(amadeus)

################################################################################
# Import configured variables and inputs.
chr_datestart <- snakemake@params[["start"]]
chr_dateend <- snakemake@params[["end"]]

################################################################################
# Full date range as dates.
chr_init_dates <- amadeus::generate_date_sequence(
  date_start = chr_datestart,
  date_end = chr_dateend,
  sub_hyphen = FALSE
)

################################################################################
# All included years as integers.
int_init_years <- unique(lubridate::year(chr_init_dates))

################################################################################
# Chunks of 100 sequential dates for parallelization. Chunks include only dates
# within the same year to adhere to {amadeus::download_modis} requirements.
list_init_dates <- beethoven::split_dates(
  dates = c(chr_init_dates[1], chr_init_dates[length(chr_init_dates)]),
  n = 100,
  year = TRUE
)

################################################################################
# Chunks of 100 sequential dates in Julian format (YYYYJJJ)
list_init_datesj <- lapply(list_init_dates, function(x) {
  format(as.Date(x), "%Y%j")
})

################################################################################
qs2::qs_save(chr_init_dates, snakemake@output[["chr_init_dates"]])
qs2::qs_save(int_init_years, snakemake@output[["int_init_years"]])
qs2::qs_save(list_init_dates, snakemake@output[["list_init_dates"]])
qs2::qs_save(list_init_datesj, snakemake@output[["list_init_datesj"]])
