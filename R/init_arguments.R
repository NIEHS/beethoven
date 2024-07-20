
# nocov start

#' Set arguments for the calculation process
#'
#' This function sets the arguments for the calculation process.
#' It takes several parameters including site ID, time ID, time period,
#' extent, user email, export path, and input path.
#' @keywords Utility
#' @param char_siteid Character string specifying the site ID.
#'   Default is "site_id".
#' @param char_timeid Character string specifying the time ID.
#'   Default is "time".
#' @param char_period Character vector specifying the time period.
#'   Default is c("2018-01-01", "2022-10-31").
#' @param num_extent Numeric vector specifying the extent.
#'   Default is c(-126, -62, 22, 52).
#' @param char_user_email Character string specifying the user email.
#'   Default is the current user's email with nih.gov domain.
#' @param export logical(1). If TRUE, the list for the calculation process
#'   is exported to `path_export`. Default is FALSE.
#' @param path_export Character string specifying the export path.
#'   Default is "inst/targets/punchcard_calc.qs".
#' @param char_input_dir Character string specifying the input path.
#'    Default is "input".
#' @param nthreads_nasa integer(1). Number of threads for NASA data.
#'    Default is 14L.
#' @param nthreads_tri integer(1). Number of threads for TRI data.
#'   Default is 5L.
#' @param nthreads_geoscf integer(1). Number of threads for GEOSCF data.
#'   Default is 10L.
#' @param nthreads_gmted integer(1). Number of threads for GMTED data.
#'   Default is 4L.
#' @param nthreads_narr integer(1). Number of threads for NARR data.
#'   Default is 24L.
#' @param nthreads_groads integer(1). Number of threads for GROADS data.
#'   Default is 3L.
#' @param nthreads_population integer(1). Number of threads for population data.
#'   Default is 3L.
#' @param nthreads_append integer(1). Number of threads for appending data.
#'   Default is 8L.
#' @param nthreads_impute integer(1). Number of threads for imputing data.
#'   Default is 64L.
#'
#' @note
#' The number of threads used is fixed as 1L
#' otherwise specified in `nthreads_*` arguments.
#' path_input should contain the following subdirectories:
#' - modis/raw/61/MOD11A1
#' - modis/raw/61/MOD06_L2
#' - modis/raw/61/MOD09GA
#' - modis/raw/61/MCD19A2
#' - modis/raw/61/MOD13A2
#' - modis/raw/5000/VNP46A2
#' - aqs
#' - nlcd
#' - geos/aqc_tavg_1hr_g1440x721_v1
#' - geos/chm_tavg_1hr_g1440x721_v1
#' - HMS_Smoke/data
#' - gmted
#' - nei
#' - narr
#' - HMS_Smoke
#' - koppen_geiger
#' - ecoregions
#' - sedac_groads
#' - sedac_population
#'
#' @returns A list of arguments for common use
#'   in the calculation process.
#' * char_siteid: Character string specifying the site ID.
#' * char_timeid: Character string specifying the time ID.
#' * char_period: Character vector specifying the time period.
#' * num_extent: Numeric vector specifying the extent.
#' * char_user_email: Character string specifying the user email.
#' * char_input_dir: Character string specifying the input path.
#' * nthreads_nasa: Number of threads for NASA data.
#' * nthreads_tri: Number of threads for TRI data.
#' * nthreads_geoscf: Number of threads for GEOS-CF data.
#' * nthreads_gmted: Number of threads for GMTED data.
#' * nthreads_narr: Number of threads for NARR data.
#' * nthreads_groads: Number of threads for SEDAC Groads data.
#' * nthreads_population: Number of threads for population data.
#' * nthreads_append: Number of threads for appending data.
#' * nthreads_impute: Number of threads for imputing data.
#' @author Insang Song
#' @importFrom qs qsave
#' @export
# nolint start
set_args_calc <-
  function(
    char_siteid = "site_id",
    char_timeid = "time",
    char_period = c("2018-01-01", "2022-10-31"),
    num_extent = c(-126, -62, 22, 52),
    char_user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
    export = FALSE,
    path_export = "inst/targets/punchcard_calc.qs",
    char_input_dir = "input",
    nthreads_nasa = 14L,
    nthreads_tri = 5L,
    nthreads_geoscf = 10L,
    nthreads_gmted = 4L,
    nthreads_narr = 24L,
    nthreads_groads = 3L,
    nthreads_population = 3L,
    nthreads_append = 8L,
    nthreads_impute = 64L
  ) {
    list_common <-
      list(
        char_siteid = char_siteid,
        char_timeid = char_timeid,
        char_period = char_period,
        extent = num_extent,
        char_user_email = char_user_email,
        char_input_dir = char_input_dir,
        nthreads_nasa = nthreads_nasa,
        nthreads_tri = nthreads_tri,
        nthreads_geoscf = nthreads_geoscf,
        nthreads_gmted = nthreads_gmted,
        nthreads_narr = nthreads_narr,
        nthreads_groads = nthreads_groads,
        nthreads_population = nthreads_population,
        nthreads_append = nthreads_append,
        nthreads_impute = nthreads_impute
      )
    ain <- function(x, append = FALSE) {
      if (append) {
        file.path(char_input_dir, x, "data_files")
      } else {
        file.path(char_input_dir, x)
      }
    }
    if (export) {
      list_paths <-
        list(
          mod11 = load_modis_files(ain("modis/raw/61/MOD11A1"), date = list_common$char_period),
          mod06 = load_modis_files(ain("modis/raw/61/MOD06_L2"), date = list_common$char_period),
          mod09 = load_modis_files(ain("modis/raw/61/MOD09GA"), date = list_common$char_period),
          mcd19 = load_modis_files(ain("modis/raw/61/MCD19A2"), date = list_common$char_period),
          mod13 = load_modis_files(ain("modis/raw/61/MOD13A2"), date = list_common$char_period),
          viirs = load_modis_files(ain("modis/raw/5000/VNP46A2"), "h5$", date = list_common$char_period)
        )

      list_proccalc <-
        list(
          aqs = list(path = ain("aqs", TRUE),
                     date = list_common$char_period),
          mod11 = list(from = list_paths$mod11,
                      name_covariates = sprintf("MOD_SFCT%s_0_", c("D", "N")),
                      subdataset = "^LST_",
                      nthreads = nthreads_nasa,
                      radius = c(1e3, 1e4, 5e4)),
          mod06 = list(from = list_paths$mod06,
                      name_covariates = sprintf("MOD_CLCV%s_0_", c("D", "N")),
                      subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
                      nthreads = nthreads_nasa,
                      preprocess = amadeus::process_modis_swath,
                      radius = c(1e3, 1e4, 5e4)),
          mod09 = list(from = list_paths$mod09,
                      name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
                      subdataset = "^sur_refl_",
                      nthreads = nthreads_nasa,
                      radius = c(1e3, 1e4, 5e4)),
          mcd19_1km = list(from = list_paths$mcd19,
                          name_covariates = sprintf("MOD_AD%dTA_0_", c(4, 5)),
                          subdataset = "^Optical_Depth",
                          nthreads = nthreads_nasa,
                          radius = c(1e3, 1e4, 5e4)),
          mcd19_5km = list(from = list_paths$mcd19,
                          name_covariates = sprintf("MOD_%sAN_0_", c("CSZ", "CVZ", "RAZ", "SCT", "GLN")),
                          subdataset = "cos|RelAZ|Angle",
                          nthreads = nthreads_nasa,
                          radius = c(1e3, 1e4, 5e4)),
          mod13 = list(from = list_paths$mod13,
                      name_covariates = "MOD_NDVIV_0_",
                      subdataset = "(NDVI)",
                      nthreads = nthreads_nasa,
                      radius = c(1e3, 1e4, 5e4)),
          viirs = list(from = list_paths$viirs,
                      name_covariates = "MOD_LGHTN_0_",
                      subdataset = 3,
                      nthreads = nthreads_nasa,
                      preprocess = amadeus::process_blackmarble,
                      radius = c(1e3, 1e4, 5e4)),
          geoscf_aqc = list(date = list_common$char_period,
                            path = ain("geos/aqc_tavg_1hr_g1440x721_v1"),
                            nthreads = nthreads_geoscf),
          geoscf_chm = list(date = list_common$char_period,
                            path = ain("geos/chm_tavg_1hr_g1440x721_v1"),
                            nthreads = nthreads_geoscf),
          # base class covariates start here
          hms = list(path = ain("HMS_Smoke", TRUE),
                    date = list_common$char_period,
                    covariate = "hms"
          ),
          gmted = list(
            path = ain("gmted", TRUE),
            covariate = "gmted"
          ),
          nei = list(
            domain = c(2017, 2020),
            domain_name = "year",
            path = ain("nei", TRUE),
            covariate = "nei"
          ),
          tri = list(
            domain = seq(2018, 2022),
            domain_name = "year",
            path = ain("tri"),
            radius = c(1e3, 1e4, 5e4),
            covariate = "tri",
            nthreads = nthreads_tri
          ),
          nlcd = list(
            domain = c(2019, 2021),
            domain_name = "year",
            path = ain("nlcd", TRUE),
            covariate = "nlcd",
            mode = "exact",
            extent = NULL,
            radius = c(1e3, 1e4, 5e4),
            max_cells = 1e8
          ),
          koppen = list(path = ain("koppen_geiger/data_files/Beck_KG_V1_present_0p0083.tif"), 
                        covariate = "koppen",
                        nthreads = 1L),
          ecoregions = list(path = ain("ecoregions/data_files/us_eco_l3_state_boundaries.shp"),
                            covariate = "ecoregions",
                            nthreads = 1L),
          narr = list(
            path = ain("narr"),
            covariate = "narr",
            domain_reduced = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
                          "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
                          "pres.sfc", "shtfl", "snowc", "soilm",    
                          "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd"),
            domain_appt = c("prate", "shum"),
            domain = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
                          "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
                          "prate", "pres.sfc", "shtfl", "shum", "snowc", "soilm",    
                          "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd"),
            domain_name = "variable",
            date = list_common$char_period,
            process_function = beethoven::process_narr2,
            calc_function = beethoven::calc_narr2,
            nthreads = nthreads_narr
          ),
          groads = list(
                        path = ain("sedac_groads/data_files/gROADS-v1-americas.gdb"),
                        covariate = "groads",
                        radius = c(1e3, 1e4, 5e4),
                        nthreads = nthreads_groads),
          population = list(
            path = ain("sedac_population/data_files/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif"),
            covariate = "population", fun = "mean",
            radius = c(1e3, 1e4, 5e4),
            nthreads = nthreads_population
          )
        )

      attr(list_proccalc, "description") <-
        tibble::tribble(
          ~dataset, ~description,
          "mod11", "MODIS Land Surface Temperature Day/Night",
          "mod06", "MODIS Cloud Fraction Day/Night",
          "mod09", "MODIS Surface Reflectance",
          "mcd19_1km", "MCD19A2 1km",
          "mcd19_5km", "MCD19A2 5km",
          "mod13", "MODIS Normalized Difference Vegetation Indexß",
          "viirs", "VIIRS Nighttime Lights",
          "hms", "NOAA Hazard Mapping System Smoke",
          "geoscf_aqc", "GEOS-CF AQC",
          "geoscf_chm", "GEOS-CF CHM",
          "gmted", "GMTED elevation",
          "nei", "National Emission Inventory",
          "tri", "Toxic Release Inventory",
          "nlcd", "National Land Cover Database",
          "koppen", "Koppen-Geiger Climate Classification",
          "ecoregions", "EPA Ecoregions",
          "narr", "NARR",
          "groads", "SEDAC Global Roads",
          "population", "SEDAC Population Density"
        )
      if (is.null(path_export)) {
        assign("arglist_proccalc", list_proccalc, envir = .GlobalEnv)
        return(list_common)
      } else {
        if (endsWith(path_export, "qs")) {
          qs::qsave(list_proccalc, path_export)
        } else if (endsWith(path_export, "rds")) {
          saveRDS(list_proccalc, path_export)
        } else {
          stop("Please provide a valid file extension: qs or rds.")
        }

        return(list_common)
      }
    }
    return(list_common)
  }


#' Generate argument list for raw data download
#' @keywords Utility
#' @param char_period Character(2) vector specifying the time period.
#'  Default is c("2018-01-01", "2022-10-31").
#' @param char_input_dir Character string specifying the input path.
#' Default is "input".
#' @param nasa_earth_data_token Character string specifying the NASA Earth Data token.
#' @param year_nlcd numeric(2). Numeric vector specifying the NLCD years.
#' Default is c(2019, 2021).
#' @param export logical(1). If TRUE, the list is saved to `path_export`.
#' Default is `TRUE`.
#' @param path_export Character string specifying the export path.
#' Default is "inst/targets/download_spec.qs".
#' @export
set_args_download <-
  function(
    char_period = c("2018-01-01", "2022-10-31"),
    char_input_dir = "input",
    nasa_earth_data_token = NULL,
    year_nlcd = c(2019, 2021),
    export = FALSE,
    path_export = "inst/targets/download_spec.qs"
  ) {
    ain <- function(x, append = FALSE) {
      if (append) {
        file.path(char_input_dir, x, "data_files")
      } else {
        file.path(char_input_dir, x)
      }
    }

    time_periods <- as.numeric(substr(char_period, 1, 4))
    year_nei <- seq(2017, time_periods[2], 3)
    gmted_vars <-
      c("Breakline Emphasis", "Systematic Subsample", "Median Statistic",
        "Minimum Statistic", "Mean Statistic", "Maximum Statistic",
        "Standard Deviation Statistic"
      )
    narr_variables_mono <-
      c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
        "hpbl", "lcdc", "lhtfl", "mcdc", "pr_wtr",
        "prate", "pres.sfc", "shtfl", "snowc", "soilm",    
        "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd")
    narr_variables_plevels <-
      c("omega", "shum")

    list_download_config <-
      list(
        aqs = list(dataset_name = "aqs", directory_to_save = ain("aqs", TRUE),
                   year_start = time_periods[1], year_end = time_periods[2],
                   unzip = TRUE, remove_zip = TRUE),
        mod11 = list(dataset_name = "modis", directory_to_save = ain("modis/raw"),
                     product = "MOD11A1", date_start = char_period[1], date_end = char_period[2],
                     nasa_earth_data_token = nasa_earth_data_token),
        mod06 = list(dataset_name = "modis", directory_to_save = ain("modis/raw"),
                     product = "MOD06_L2", date_start = char_period[1], date_end = char_period[2],
                     nasa_earth_data_token = nasa_earth_data_token),
        mod09 = list(dataset_name = "modis", directory_to_save = ain("modis/raw"),
                     product = "MOD09GA", date_start = char_period[1], date_end = char_period[2],
                     nasa_earth_data_token = nasa_earth_data_token),
        mcd19 = list(dataset_name = "modis", directory_to_save = ain("modis/raw"),
                     product = "MCD19A2", date_start = char_period[1], date_end = char_period[2],
                     nasa_earth_data_token = nasa_earth_data_token),
        mod13 = list(dataset_name = "modis", directory_to_save = ain("modis/raw"),
                     product = "MOD13A2", date_start = char_period[1], date_end = char_period[2],
                     nasa_earth_data_token = nasa_earth_data_token),
        viirs = list(dataset_name = "modis", directory_to_save = ain("modis/raw"),
                     product = "VNP46A2", date_start = char_period[1], date_end = char_period[2],
                     version = "5000",
                     nasa_earth_data_token = nasa_earth_data_token),
        geoscf_aqc = list(dataset_name = "geos", directory_to_save = ain("geos"),
                          collection = "aqc_tavg_1hr_g1440x721_v1",
                          date_start = char_period[1], date_end = char_period[2]),
        geoscf_chm = list(dataset_name = "geos", directory_to_save = ain("geos"),
                          collection = "chm_tavg_1hr_g1440x721_v1",
                          date_start = char_period[1], date_end = char_period[2]),
        hms = list(dataset_name = "smoke", directory_to_save = ain("HMS_Smoke"),
                   data_format = "Shapefile",
                   date_start = char_period[1], date_end = char_period[2],
                   unzip = TRUE, remove_zip = TRUE),
        gmted = lapply(gmted_vars,
          function(v) {
            list(dataset_name = "gmted", directory_to_save = ain("gmted", TRUE),
                 static = v, resolution = "7.5 arc-seconds",
                 unzip = TRUE, remove_zip = TRUE)
          }),
        nei = lapply(year_nei,
          function(y) {
          list(dataset_name = "nei", directory_to_save = ain("nei", TRUE),
                   year_target = y, unzip = TRUE)
          }),
        tri = list(dataset_name = "tri", directory_to_save = ain("tri"),
                   year_start = time_periods[1], year_end = time_periods[2]),
        nlcd = lapply(year_nlcd,
          function(y) {
            list(dataset_name = "nlcd", directory_to_save = ain("nlcd", TRUE),
                    year = y,
                    unzip = TRUE, remove_zip = TRUE)
          }),
        koppen = list(dataset_name = "koppen", directory_to_save = ain("koppen_geiger", TRUE),
                      data_resolution = "0.0083", time_period = "Present", unzip = TRUE, remove_zip = TRUE),
        ecoregions = list(dataset_name = "koppen", directory_to_save = ain("ecoregions", TRUE),
                          unzip = TRUE, remove_zip = TRUE),
        narr_monolevel = lapply(narr_variables_mono,
          function(v) {
            list(dataset_name = "narr_monolevel", directory_to_save = ain("narr"),
                 variables = v, year_start = char_period[1], year_end = char_period[2])
          }),
        narr_p_levels = lapply(narr_variables_plevels,
          function(v) {
            list(dataset_name = "narr_p_levels", directory_to_save = ain("narr"),
                 variables = v, year_start = char_period[1], year_end = char_period[2])
          })
        ,
        groads = list(dataset_name = "sedac_groads", directory_to_save = ain("sedac_groads", TRUE),
                      data_region = "Americas", data_format = "Geodatabase",
                      unzip = TRUE, remove_zip = TRUE),
        population = list(dataset_name = "sedac_population", directory_to_save = ain("sedac_population", TRUE),
                          data_resolution = "30 second", data_format = "GeoTIFF", year = "2020", unzip = TRUE, remove_zip = TRUE)
      )

    if (export) {
        if (endsWith(path_export, "qs")) {
          qs::qsave(list_proccalc, path_export)
          message("Download configuration is saved to ", path_export)
        } else if (endsWith(path_export, "rds")) {
          saveRDS(list_proccalc, path_export)
          message("Download configuration is saved to ", path_export)
        } else {
          stop("Please provide a valid file extension: qs or rds.")
        }
    }
    return(list_download_config)
  }

# nolint end

# nocov end