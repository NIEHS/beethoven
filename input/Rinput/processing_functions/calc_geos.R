################################################################################
# Date created: 2023-11-27
# Packages required: stringr
################################################################################

################################################################################
#' calc_geos: Calculate atmospheric chemistry covariates from GEOS-CF.
#' @param date_start character(1). length of 10. Format YYYY-MM-DD
#' (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. Format YYYY-MM-DD
#' (ex. September 1, 2023 = "2023-09-01").
#' @param collection character(1). GEOS-CF data collection file name.
#' @param variable character(1).
#' @param sites
#' @param identifiier character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param crs integer(1). Coordinate reference system.
#' @param fun character(1). Function used to summarize hourly or three hourly
#' observations into daily statistic. (Default = `mean`).
#' @param directory_with_data character(1). Directory with downloaded GEOS-CF
#' netCDF files.
#' @param directory_to_save character(1). Directory to save processed data.
#' @author Mitchell Manware
#' @return a data.frame object;
#' @importFrom stringr str_sub
#' @importFrom stringr str_pad
#' @importFrom terra vect
#' @importFrom terra rast
#' @importFrom terra as.data.frame
#' @importFrom terra project
#' @importFrom terra extract
#' @export
calc_geos <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-01",
    collection = NULL,
    variable = NULL,
    sites,
    identifier = NULL,
    crs = 4326,
    fun = "mean",
    directory_with_data = "../../data/covariates/geos_cf/") {
  #### 1. directory setup
  directory_with_data <- download_sanitize_path(directory_with_data)
  #### 2. check for variable
  check_for_null_parameters(mget(ls()))
  #### 6. check if collection is valid
  collections <- c(
    "htf_inst_15mn_g1440x721_x1", "aqc_tavg_1hr_g1440x721_v1",
    "chm_tavg_1hr_g1440x721_v1", "met_tavg_1hr_g1440x721_x1",
    "xgc_tavg_1hr_g1440x721_x1", "chm_inst_1hr_g1440x721_p23",
    "met_inst_1hr_g1440x721_p23"
  )
  if (!(collection %in% collections)) {
    stop(paste0("Requested collection is not recognized.\n"))
  }
  #### 8. assign variable code
  codes <- cbind(
    unlist(strsplit(paste0("ACET ALD2 ALK4 BCPI BCPO BENZ C2H6 C3H8 CH4 CO ",
                           "DST1 DST2 DST3 DST4 EOH H2O2 HCHO HNO3 HNO4 ISOP ",
                           "MACR MEK MVK N2O5 NH3 NH4 NIT NO NO2 NOy OCPI ",
                           "OCPO PAN PM25_RH35_GCC PM25_RH35_GOCART ",
                           "PM25bc_RH35_GCC PM25du_RH35_GCC PM25ni_RH35_GCC ",
                           "PM25oc_RH35_GCC PM25soa_RH35_GCC PM25ss_RH35_GCC ",
                           "PM25su_RH35_GCC PRPE RCHO SALA SALC SO2 SOAP SOAS ",
                           "TOLU XYLE CO NO2 O3 SO2"), " ")),
    unlist(strsplit(paste0("ACETO ACETA CALKA HIBCA HOBCA BENZE ETHTE PROPA ",
                           "METHA CMONO DUST1 DUST2 DUST3 DUST4 ETHOL HYPER ",
                           "FORMA NITAC PERAC ISOPR METHC MEKET MVKET DIPEN ",
                           "AMNIA AMNUM INNIT NIOXI NIDIO NITRO HIORG HOORG ",
                           "PERNI PM25X PM25R BLCPM DUSPM NITPM ORCPM SORPM ",
                           "SEAPM SULPM CALKE CALDH FSEAS CSEAS SULDI SOAPR ",
                           "SOASI TOLUE XYLEN COVMR NOVMR OZVMR SOVMR"), " ")),
    c(rep("chm_tavg_1hr_g1440x721_v1", 51), rep("aqc_tavg_1hr_g1440x721_v1", 4))
  )
  collection_codes <- subset(codes, subset = codes[, 3] == collection)
  code <- collection_codes[collection_codes[, 1] == variable, ][2]
  #### 7. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 8. define time sequence
  if (stringr::str_sub(collection, -1, -1) == "1") {
    time_sequence <- as.character(seq(from = 30, to = 2330, by = 100))
    time_sequence <- stringr::str_pad(time_sequence,
      pad = "0",
      width = 4,
      side = "left"
    )
  } else if (stringr::str_sub(collection, -1, -1) == "3") {
    time_sequence <- as.character(seq(from = 100, to = 2400, by = 100))
    time_sequence <- stringr::str_pad(time_sequence,
      pad = "0",
      width = 4,
      side = "left"
    )
  }
  #### 10. location SpatVector
  sites_v <- terra::vect(sites,
    geom = c("lon", "lat"),
    crs = paste0(
      "EPSG:",
      crs
    )
  )
  #### 11. site identifiers
  sites_id <- terra::as.data.frame(sites_v)
  #### 12. empty location data.frame
  sites_extracted_df <- NULL
  for (d in seq_along(date_sequence)) {
    date <- date_sequence[d]
    data_date <- terra::rast()
    for (t in seq_along(time_sequence)) {
      #### 13. define file paths
      path <- paste0(
        directory_with_data,
        "/GEOS-CF.v01.rpl.",
        collection,
        ".",
        date,
        "_",
        time_sequence[t],
        "z.nc4"
      )
      #### 14. import SpatRaster data
      data <- terra::rast(path)
      data_time <- terra::subset(
        data,
        subset = grep(
          variable,
          names(data)
        )
      )
      # data_time <- terra::rast(path,
      #   subds = variable
      # )
      #### 15. combine SpatRaster with same date
      data_date <- c(data_date, data_time, warn = FALSE)
    }
    #### 16. calculate daily mean
    data_fun <- do.call(fun, list(data_date, na.rm = TRUE))
    #### 17. define varname based on variable and date
    names(data_fun) <- paste0(variable)
    #### 18. set coordinate reference system
    terra::crs(data_fun) <- paste0("EPSG:", crs)
    #### 19. project to coordinate reference system
    data_fun <- terra::project(
      data_fun,
      paste0("EPSG:", crs)
    )
    #### 20. extract raster values at locations
    sites_extracted_date <- terra::extract(data_fun,
      sites_v,
      method = "simple",
      ID = FALSE,
      bind = FALSE
    )
    #### 21. combine with locations data.frame base
    sites_extracted_date <- data.frame(cbind(
      sites_id,
      as.Date(date,
        format = "%Y%m%d"
      ),
      sites_extracted_date
    ))
    #### 22. define column names
    colnames(sites_extracted_date) <- c(
      identifier,
      "date",
      paste0(
        "GEO_",
        code,
        "_0_00000"
      )
    )
    #### 23. combine with locations data.frame
    sites_extracted_df <- rbind(sites_extracted_df, sites_extracted_date)
    cat(paste0("Calculated ", variable, " for date ", date, "\n" ))
  }
  #### 24. export data
  return(sites_extracted_df)
}
