# Unmarshal functions

this function is developed to avoid random errors in compressing and
decompressing R function objects with
[`qs2::qs_save`](https://rdrr.io/pkg/qs2/man/qs_save.html) and
[`qs2::qs_read`](https://rdrr.io/pkg/qs2/man/qs_read.html). If you
encounter such errors, please use this function with function name
strings to save and load the function objects.

## Usage

``` r
unmarshal_function(pkg_func_str)
```

## Arguments

- pkg_func_str:

  Character string specifying the package and function.

## Value

Function object.

## Note

The function name string must include two colons `::`. Also, the package
preceding the two colons should be loaded in the current environment.

## Examples

``` r
unmarshal_function("amadeus::process_aqs")
#> function (path = NULL, date = c("2018-01-01", "2022-12-31"), 
#>     mode = c("date-location", "available-data", "location"), 
#>     data_field = "Arithmetic.Mean", return_format = c("terra", 
#>         "sf", "data.table"), extent = NULL, resolution_temporal = c("daily", 
#>         "hourly"), tz = c("local", "gmt"), ...) 
#> {
#>     mode <- match.arg(mode)
#>     return_format <- match.arg(return_format)
#>     resolution_temporal <- match.arg(resolution_temporal)
#>     if (missing(data_field)) {
#>         data_field <- if (resolution_temporal == "hourly") {
#>             "Sample.Measurement"
#>         }
#>         else {
#>             "Arithmetic.Mean"
#>         }
#>     }
#>     tz <- match.arg(tz)
#>     date_tz <- ifelse(tz == "local", "Date.Local", "Date.GMT")
#>     if (resolution_temporal == "daily" & tz == "gmt") {
#>         message("GMT time zone not applicable for daily data; using `Date.Local`.")
#>         date_tz <- "Date.Local"
#>     }
#>     time_tz <- ifelse(tz == "local", "Time.Local", "Time.GMT")
#>     if (!is.null(date)) {
#>         date <- try(as.Date(date), silent = TRUE)
#>         if (inherits(date, "try-error")) {
#>             stop("date has invalid format(s). Please check the values.")
#>         }
#>         if (length(date) != 2) {
#>             if (length(date) == 1) {
#>                 date <- c(date, date)
#>             }
#>             else {
#>                 stop("date should be a character vector of length 1 or 2.")
#>             }
#>         }
#>     }
#>     else {
#>         stop("date should be defined.")
#>     }
#>     if (length(path) == 1 && dir.exists(path)) {
#>         path <- list.files(path = path, pattern = "*.csv$", full.names = TRUE)
#>     }
#>     if (length(path) == 0) {
#>         stop("path does not contain csv files.")
#>     }
#>     pathfiles <- lapply(path, read.csv)
#>     sites <- data.table::rbindlist(pathfiles, fill = TRUE)
#>     sites$site_id <- sprintf("%02d%03d%04d%05d", as.integer(sites$State.Code), 
#>         as.integer(sites$County.Code), as.integer(sites$Site.Num), 
#>         as.integer(sites$Parameter.Code))
#>     site_id <- NULL
#>     Datum <- NULL
#>     POC <- NULL
#>     Date.Local <- NULL
#>     Sample.Duration <- NULL
#>     Observation.Count <- NULL
#>     date_start <- as.Date(date[1])
#>     date_end <- as.Date(date[2])
#>     date_sequence <- seq(date_start, date_end, "day")
#>     time_sequence <- sprintf("%02d:00", 0:23)
#>     parsed_dates <- as.Date(rep(NA_character_, nrow(sites)))
#>     raw_dates <- as.character(sites[[date_tz]])
#>     slash_idx <- grepl("/", raw_dates, fixed = TRUE)
#>     dash_idx <- grepl("-", raw_dates, fixed = TRUE)
#>     parsed_dates[slash_idx] <- as.Date(raw_dates[slash_idx], 
#>         format = "%m/%d/%Y")
#>     parsed_dates[dash_idx] <- as.Date(raw_dates[dash_idx], format = "%Y-%m-%d")
#>     sites[[date_tz]] <- parsed_dates
#>     duration_keep <- startsWith(as.character(sites$Sample.Duration), 
#>         "24")
#>     if (resolution_temporal == "hourly") {
#>         duration_keep <- rep(TRUE, nrow(sites))
#>     }
#>     else if ("Observation.Count" %in% names(sites)) {
#>         duration_keep <- duration_keep | (!is.na(sites$Observation.Count) & 
#>             sites$Observation.Count == 24)
#>     }
#>     sites$duration_keep <- duration_keep
#>     chr_time <- if (resolution_temporal == "daily") {
#>         as.character(sites[[date_tz]])
#>     }
#>     else {
#>         as.character(paste0(sites[[date_tz]], " ", sites[[time_tz]]))
#>     }
#>     sites <- dplyr::ungroup(dplyr::filter(dplyr::group_by(dplyr::filter(dplyr::filter(dplyr::mutate(dplyr::as_tibble(sites), 
#>         time = chr_time), .data[[date_tz]] %in% date_sequence), 
#>         duration_keep), site_id), POC == min(POC)))
#>     col_sel <- c("site_id", "Longitude", "Latitude", "Datum")
#>     if (mode != "available-data") {
#>         sites_v <- unique(sites[, col_sel])
#>     }
#>     else {
#>         col_sel <- unique(c(col_sel, if (resolution_temporal == 
#>             "daily") "Event.Type", "time", data_field))
#>         sites_v <- dplyr::distinct(dplyr::select(sites, dplyr::all_of(col_sel)))
#>         if ("Event.Type" %in% col_sel) {
#>             sites_vdup <- dplyr::ungroup(dplyr::filter(dplyr::filter(dplyr::group_by(sites_v, 
#>                 site_id, time), dplyr::n() > 1), !!dplyr::sym("Event.Type") == 
#>                 "Excluded"))
#>             sites_v <- dplyr::anti_join(sites_v, sites_vdup, 
#>                 by = c("site_id", "time", "Event.Type"))
#>         }
#>     }
#>     names(sites_v)[2:3] <- c("lon", "lat")
#>     sites_v <- data.table::as.data.table(sites_v)
#>     sites_v <- sites_v[!grepl("^(02|15|72|78|6|80)", site_id), 
#>         ]
#>     sites_v_nad <- sites_v[sites_v$Datum == "NAD83", ]
#>     if (nrow(sites_v_nad) > 0) {
#>         sites_v_nad <- sf::st_as_sf(sites_v_nad, remove = FALSE, 
#>             coords = c("lon", "lat"), crs = "EPSG:4269")
#>         sites_v_nad <- sf::st_transform(sites_v_nad, "EPSG:4326")
#>         sites_v_nad$lon <- sf::st_coordinates(sites_v_nad)[, 
#>             1]
#>         sites_v_nad$lat <- sf::st_coordinates(sites_v_nad)[, 
#>             2]
#>         sites_v_nad <- sf::st_drop_geometry(sites_v_nad)
#>         sites_v_nad <- data.table::as.data.table(sites_v_nad)
#>     }
#>     sites_v_wgs <- sites_v[sites_v$Datum == "WGS84"]
#>     final_sites <- data.table::rbindlist(list(sites_v_wgs, sites_v_nad), 
#>         fill = TRUE)
#>     final_sites <- final_sites[, grep("Datum", names(final_sites), 
#>         invert = TRUE), with = FALSE]
#>     if (mode == "date-location") {
#>         dl_sequence <- if (resolution_temporal == "daily") {
#>             date_sequence
#>         }
#>         else {
#>             df_sequence <- expand.grid(date_sequence, time_sequence)
#>             paste0(df_sequence[, 1], " ", df_sequence[, 2])
#>         }
#>         final_sites <- lapply(split(as.character(dl_sequence), 
#>             as.character(dl_sequence)), function(x) {
#>             fs_time <- final_sites
#>             fs_time$time <- x
#>             return(fs_time)
#>         })
#>         final_sites <- data.table::rbindlist(final_sites, fill = TRUE)
#>     }
#>     if (mode == "available-data") {
#>         final_sites <- unique(final_sites)
#>     }
#>     final_sites <- switch(return_format, terra = terra::vect(final_sites, 
#>         keepgeom = TRUE, crs = "EPSG:4326"), sf = sf::st_as_sf(final_sites, 
#>         remove = FALSE, dim = "XY", coords = c("lon", "lat"), 
#>         crs = "EPSG:4326"), data.table = final_sites)
#>     if (!is.null(extent)) {
#>         if (return_format == "data.table") {
#>             warning("Extent is not applicable for data.table. Returning data.table...\n")
#>             return(final_sites)
#>         }
#>         final_sites <- apply_extent(final_sites, extent)
#>     }
#>     return(final_sites)
#> }
#> <bytecode: 0x56b4db7a44f0>
#> <environment: namespace:amadeus>
```
