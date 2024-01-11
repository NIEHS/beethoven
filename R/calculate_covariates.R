#' Calculate covariates

# nocov start

# to look at the path and package settings,
# consult "load_packages.r"
# run variable is to pass lintr test
run <- FALSE
if (run) {
  source("./input/Rinput/processing_functions/load_packages.R")
}


#' Calculate Koeppen-Geiger climate zone binary variables
#' @note the same function is run with an alias
#' \code{calc_koeppen_geiger}.
#' @param path_koppen character(1). Path to Koppen-Geiger
#'  climate zone raster file
#' @param sites sf/SpatVector. Unique sites. Should include
#'  a unique identifier field named \code{id_col}
#' @param id_col character(1). Name of unique identifier.
#' @returns a data.frame object
#' @author Insang Song
#' @importFrom terra vect
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra extract
#' @importFrom terra coltab
#' @importFrom terra merge
#' @importFrom methods is
#' @export
calc_koppen_geiger <-
  function(
      path_koppen = "./input/koppen_geiger/raw/Beck_KG_V1_present_0p0083.tif",
      sites,
      id_col = "site_id") {
    ## You will get "sites" in memory after sourcing the file above
    kg_rast <- terra::rast(path_koppen)
    sites_tr <- sites

    if (!methods::is(sites, "SpatVector")) {
      sites_tr <- terra::vect(sites)
    }
    sites_kg <- terra::project(sites_tr, terra::crs(kg_rast))
    sites_kg_extract <- terra::extract(kg_rast, sites_kg)

    # The starting value is NA as the color table has 0 value in it
    kg_class <-
      c(
        NA, "Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", "Csa", "Csb",
        "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc",
        "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
        "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF"
      )
    kg_coltab <- terra::coltab(kg_rast)
    kg_coltab <- kg_coltab[[1]][seq(1, 31), ]
    kg_colclass <- data.frame(
      value = kg_coltab$value,
      class_kg = kg_class
    )

    sites_kg_extract[[id_col]] <- unlist(sites_kg[[id_col]])
    colnames(sites_kg_extract)[2] <- "value"
    sites_kg_extract_e <- merge(sites_kg_extract, kg_colclass, by = "value")

    # "Dfa": 25
    # "BSh": 6
    # "Dfb": 26
    id_search <- unlist(sites_kg_extract_e[[id_col]])
    # errorfix: how to generalize and auto-fix it?
    sites_kg_extract_e[
      which(id_search == "44009000788101"),
      "class_kg"
    ] <- "Dfa"
    sites_kg_extract_e[
      which(id_search == "48061200488101"),
      "class_kg"
    ] <- "BSh"
    sites_kg_extract_e[
      which(id_search == "33015001488101"),
      "class_kg"
    ] <- "Dfb"

    sites_kg_extract_e$class_kg <-
      as.factor(substr(sites_kg_extract_e$class_kg, 1, 1))
    # currently there are no "E" region in sites.
    # however, E is filled with all zeros at the moment.
    aelabels <- LETTERS[1:5]
    df_ae_separated <-
      split(aelabels, aelabels) |>
      lapply(function(x) {
        as.integer(sites_kg_extract_e$class_kg == x)
      }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_ae_separated) <- sprintf("DUM_CLRG%s_0_00000", aelabels)

    kg_extracted <-
      cbind(
        site_id = unlist(sites_kg_extract_e[[id_col]]),
        df_ae_separated
      )
    return(kg_extracted)
  }


calc_koeppen_geiger <- calc_koppen_geiger
if (run) {
  saveRDS(kg_extracted, file = "~/NRTAP_Covars_Koppen_Geiger_AE_binary.rds")
}

#' calc_geos: Calculate atmospheric chemistry covariates from GEOS-CF.
#' @description
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

#' calc_hms: Calculate wildfire smoke plume binary covariates from NOAA HMS
#' Smoke Product.
#' @param date_start character(1). length of 10. Start date of downloaded data. 
#' Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date of downloaded data. 
#' Format YYYY-MM-DD (ex. September 10, 2023 = "2023-09-10").
#' @param data SpatVector(1). SpatVector object produced by `process_hms`
#' containing density level-specific polygons.
#' @param density_level character(1). "Light", "Medium", or "Heavy".
#' @param sites
#' @param identifiier character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @author Mitchell Manware.
#' @return a data.frame object;
#' @importFrom terra vect
#' @importFrom terra project
#' @importFrom terra extract
#' @importFrom terra as.data.frame
#' @export
calc_hms <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-02",
    data,
    density_level = c("Light", "Medium", "Heavy"),
    sites,
    identifier = "site_id"
) {
  #### 2. check for null parameters
  check_for_null_parameters(mget(ls()))
  #### 3. sites as SpatVector
  sites_v <- terra::vect(
    sites,
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )
  sites_v <- terra::project(sites_v, "EPSG:4326")
  #### 4. site identifiers
  sites_id <- terra::as.data.frame(sites_v)
  #### 5. define date sequence
  date_start_date_format <- as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format <- as.Date(date_end, format = "%Y-%m-%d")
  date_sequence <- seq(date_start_date_format, date_end_date_format, "day")
  date_sequence <- gsub("-", "", as.character(date_sequence))
  #### 6. empty data.frame for extracted values
  sites_extracted <- NULL
  for (d in seq_along(date_sequence)) {
    cat(paste0(
      "Extracting ",
      density_level,
      " smoke data for date: ",
      date_sequence[d],
      "\n"))
    #### 7. subset to date
    data_date <- data[data$Date == date_sequence[d],]
    #### 8. check for polygons
    if (length(data_date$Density) == 0) {
      cat(paste0(
        "No ",
        density_level,
        " smoke plume polygons available for ",
        date_sequence[d],
        ". Will return 0 (NO SMOKE PRESENT).\n"))
    }
    #### 9. extract data at sites
    sites_extracted_date <- terra::extract(
      data_date,
      sites_v
    )
    #### 10. binary identifier (0 = NO SMOKE PRESENT; 1 = SMOKE PRESENT)
    sites_extracted_date$binary12 <- match(
      as.character(sites_extracted_date$Density),
      c(NA, density_level)
    )
    sites_extracted_date$binary01 <- sub(1, 0, sites_extracted_date$binary12)
    sites_extracted_date$binary01 <- sub(2, 1, sites_extracted_date$binary01)
    #### 11. combine with site identifier
    sites_extracted_date_id <- data.frame(cbind(
      sites_id,
      paste0(as.Date(
        date_sequence[d],
        format = "%Y%m%d")),
      as.numeric(sites_extracted_date$binary01)
    ))
    #### 12. set column names
    colnames(sites_extracted_date_id) <- c(
      identifier,
      "date",
      paste0(
        "OTH_HMSW",
        substr(density_level, 1, 1),
        "_0_00000")
    )
    sites_extracted <- rbind(sites_extracted, sites_extracted_date_id)
  }
  return(sites_extracted)
}

#' calc_narr: Calculate meteorological covariates from NOAA NCEP North American
#' Regional Reanalysis.
#' @param year_start integer(1). length of 4. Start of year range for
#' downloading data.
#' @param year_end integer(1). length of 4. End of year range for downloading
#' data.
#' @param month_start integer(1). 1 = January; 12 = December
#' @param month_end integer(1). 1 = January; 12 = December
#' @param variable character(1).
#' @param sites
#' @param identifiier character(1). Column within `locations` CSV file
#' containing identifier for each unique coordinate location.
#' @param level integer(1). Pressure level of data.
#' @param directory_with_data character(1). Directory with downloaded NARR
#' monolevel netCDF files.
#' @author Mitchell Manware
#' @return a data.frame object
#' @export
calc_narr <- function(
    year_start = 2018,
    year_end = 2022,
    month_start = NULL,
    month_end = NULL,
    variable = NULL,
    sites,
    identifier = "site_id",
    level = NULL,
    directory_with_data = "../../data/covariates/narr/") {
  #### 1. directory setup
  chars_dir_data <- nchar(directory_with_data)
  if (substr(directory_with_data, chars_dir_data, chars_dir_data) != "/") {
    directory_with_data <- paste0(directory_with_data, "/", sep = "")
  }
  #### 2. assign variable code
  code <- gsub("\\.", "", variable)
  code <- gsub("_", "", code)
  code <- substr(toupper(paste0(code, code)), 1, 5)
  #### 3. NARR-specific coordinate reference system
  crs_narr <- paste0("PROJCRS[\"unnamed\",\n
                     BASEGEOGCRS[\"WGS 84\",\n
                     DATUM[\"World Geodetic System 1984\",\n
                     ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n
                     LENGTHUNIT[\"metre\",1]]],\n
                     PRIMEM[\"Greenwich\",0,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433]],\n
                     ID[\"EPSG\",4326]],\n
                     CONVERSION[\"unnamed\",\n
                     METHOD[\"Lambert Conic Conformal (2SP)\",\n
                     ID[\"EPSG\",9802]],\n
                     PARAMETER[\"Latitude of false origin\",50,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433],\n
                     ID[\"EPSG\",8821]],\n
                     PARAMETER[\"Longitude of false origin\",-107,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433],\n
                     ID[\"EPSG\",8822]],\n
                     PARAMETER[\"Latitude of 1st standard parallel\",50,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433],\n
                     ID[\"EPSG\",8823]],\n
                     PARAMETER[\"Latitude of 2nd standard parallel\",50,\n
                     ANGLEUNIT[\"degree\",0.0174532925199433],\n
                     ID[\"EPSG\",8824]],\n
                     PARAMETER[\"Easting at false origin\",5632642.22547,\n
                     LENGTHUNIT[\"metre\",1],\n
                     ID[\"EPSG\",8826]],\n
                     PARAMETER[\"Northing at false origin\",4612545.65137,\n
                     LENGTHUNIT[\"metre\",1],\n
                     ID[\"EPSG\",8827]]],\n
                     CS[Cartesian,2],\n
                     AXIS[\"easting\",east,\n
                     ORDER[1],\n
                     LENGTHUNIT[\"metre\",1,\n
                     ID[\"EPSG\",9001]]],\n
                     AXIS[\"northing\",north,\n
                     ORDER[2],\n
                     LENGTHUNIT[\"metre\",1,\n
                     ID[\"EPSG\",9001]]]]")
  #### 4. sites as SpatVector
  sites_v <- terra::vect(sites,
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )
  sites_v <- terra::project(sites_v, crs_narr)
  #### 5. site identifiers
  sites_id <- terra::as.data.frame(sites_v)
  #### 6. define year or year+month sequence
  sequence <- NULL
  years <- seq(year_start, year_end, 1)
  if (!is.null(c(month_start, month_end))) {
    months <- seq(month_start, month_end, 1)
    for (y in seq_along(years)) {
      year <- years[y]
      for (m in seq_along(months)) {
        year_month <- paste0(
          year,
          stringr::str_pad(months[m],
            width = 2,
            pad = "0",
            side = "left"
          )
        )
        sequence <- c(sequence, year_month)
      }
    }
  } else {
    sequence <- c(sequence, years)
  }
  #### 7. define file paths
  paths <- paste0(
    directory_with_data,
    variable,
    "/",
    variable,
    ".",
    sequence,
    ".nc"
  )
  #### 8. empty data.frame for extracted values
  sites_extracted_df <- NULL
  #### 9. import SpatRaster data
  for (p in seq_along(paths)) {
    data_ym <- terra::rast(paths[p])
    #### 10. subset to pressure level if not null
    if (!is.null(level)) {
      data_ym <- terra::subset(data_ym,
        subset = grep(
          paste0(
            "level=",
            level
          ),
          names(data_ym)
        )
      )
    }
    #### 11. define varname based on variable and date
    names(data_ym) <- paste0(
      variable,
      "_",
      gsub(
        "-",
        "",
        as.Date(terra::time(data_ym),
          format = "%Y%d%m"
        )
      )
    )
    for (l in seq_len(terra::nlyr(data_ym))) {
      #### 12. store data date
      date <- as.Date(terra::time(data_ym[[l]]),
        format = "%Y%m%d"
      )
      cat(paste0("Extracting ", variable, " data for date: ", date, "\n"))
      #### 13. extract data
      sites_extracted_date <- terra::extract(data_ym[[l]],
        sites_v,
        method = "simple",
        ID = FALSE,
        bind = FALSE
      )
      #### 14. combine with identifier
      sites_extracted_date <- data.frame(cbind(
        sites_id,
        paste0(date),
        sites_extracted_date
      ))
      colnames(sites_extracted_date) <- c(
        identifier,
        "date",
        paste0(
          "MET_",
          code,
          "_0_00000"
        )
      )
      #### 15. replace missing data flag with NA
      sites_extracted_date[, 3] <-
        replace(
          sites_extracted_date[, 3],
          sites_extracted_date[, 3] == 9.96921e+36,
          NA
        )
      #### 16. combine with locations data.frame
      sites_extracted_df <- rbind(sites_extracted_df, sites_extracted_date)
    }
  }
  #### 17. return data frame with extracted values
  return(sites_extracted_df)
}
