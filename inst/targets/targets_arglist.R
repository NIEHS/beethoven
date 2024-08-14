library(dplyr)
library(qs)
# basic idea: a list that provides
# anything needed for the pipeline
# ultimately the name "dataset" is a key for iteration to make
# the outermost pipeline look as simple as possible

# for reference, full list of parameters in amadeus::process_*
# and amadeus::calc_*.
# This list will be useful for entering parameters by rlang::inject.
# amadeusArgs <- list(
#   path = NULL,
#   date = NULL,
#   variable = NULL,
#   year = NULL,
#   county = NULL,
#   variables = NULL,
#   from = NULL,
#   locs = NULL,
#   locs_id = NULL,
#   radius = NULL,
#   fun = NULL,
#   name_extracted = NULL,
#   fun_summary = NULL,
#   max_cells = NULL,
#   preprocess = NULL,
#   name_covariates = NULL,
#   subdataset = NULL,
#   nthreads = NULL,
#   package_list_add = NULL,
#   export_list_add = NULL,
#   sedc_bandwidth = NULL,
#   target_fields = NULL
# )

library(beethoven)

## This file is to define required parameters that are reused
## throughout the pipeline.
## First, let's start with the list of objects generated in this code.
## arglist_common: common information for running the pipeline
##   site identifiers, time identifier, time period of computation,
##   spatial extent of computation, and an email address (for receiving
##   computing node status).


#' Set arguments for the calculation process
#'
#' This function sets the arguments for the calculation process. It takes several parameters
#' including site ID, time ID, time period, extent, user email, export path, and input path.
#' It returns a list of arguments for the calculation process.
#'
#' @param char_siteid Character string specifying the site ID.
#'   Default is "site_id".
#' @param char_timeid Character string specifying the time ID.
#'   Default is "time".
#' @param char_period Character vector specifying the time period.
#'   Default is c("2018-01-01", "2022-10-31").
#' @param extent Numeric vector specifying the extent.
#'   Default is c(-126, -62, 22, 52).
#' @param user_email Character string specifying the user email.
#'   Default is the current user's email.
#' @param path_export Character string specifying the export path.
#'   Default is "inst/targets/punchcard_calc.qs".
#'   If `NULL`, a list object "arglist_common" is exported to the global
#'   environment and returns a list of arguments for the calculation process.
#' @param path_input Character string specifying the input path.
#'   Default is "input".
#'
#' @returns A list of arguments for the calculation process.
#' @importFrom qs qsave
#' @export
set_args_calc <-
  function(
    char_siteid = "site_id",
    char_timeid = "time",
    char_period = c("2018-01-01", "2022-10-31"),
    extent = c(-126, -62, 22, 52),
    user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
    path_export = "inst/targets/punchcard_calc.qs",
    path_input = "input"
  ) {
    list_common <-
      list(
        char_siteid = char_siteid,
        char_timeid = char_timeid,
        char_period = char_period,
        extent = num_extent,
        user_email = char_user_email
      )
    ain <- function(x) file.path(path_input, x)
    list_paths <-
      list(
        mod11 = load_modis_files(ain("modis/raw/61/MOD11A1"), date = arglist_common$char_period),
        mod06 = load_modis_files(ain("modis/raw/61/MOD06_L2"), date = arglist_common$char_period),
        mod09 = load_modis_files(ain("modis/raw/61/MOD09GA"), date = arglist_common$char_period),
        mcd19 = load_modis_files(ain("modis/raw/61/MCD19A2"), date = arglist_common$char_period),
        mod13 = load_modis_files(ain("modis/raw/61/MOD13A2"), date = arglist_common$char_period),
        viirs = load_modis_files(ain("modis/raw/5000/VNP46A2"), "h5$", date = arglist_common$char_period)
      )

    list_proccalc <-
      list(
        aqs = list(path = ain("aqs")),
        mod11 = list(from = list_paths$mod11,
                    name_covariates = sprintf("MOD_SFCT%s_0_", c("D", "N")),
                    subdataset = "^LST_",
                    nthreads = 14L,
                    radius = c(1e3, 1e4, 5e4)),
        mod06 = list(from = list_paths$mod06,
                    name_covariates = sprintf("MOD_CLCV%s_0_", c("D", "N")),
                    subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
                    nthreads = 14L,
                    preprocess = amadeus::process_modis_swath,
                    radius = c(1e3, 1e4, 5e4)),
        mod09 = list(from = list_paths$mod09,
                    name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
                    subdataset = "^sur_refl_",
                    nthreads = 14L,
                    radius = c(1e3, 1e4, 5e4)),
        mcd19_1km = list(from = list_paths$mcd19,
                        name_covariates = sprintf("MOD_AD%dTA_0_", c(4, 5)),
                        subdataset = "^Optical_Depth",
                        nthreads = 14L,
                        radius = c(1e3, 1e4, 5e4)),
        mcd19_5km = list(from = list_paths$mcd19,
                        name_covariates = sprintf("MOD_%sAN_0_", c("CSZ", "CVZ", "RAZ", "SCT", "GLN")),
                        subdataset = "cos|RelAZ|Angle",
                        nthreads = 14L,
                        radius = c(1e3, 1e4, 5e4)),
        mod13 = list(from = list_paths$mod13,
                    name_covariates = "MOD_NDVIV_0_",
                    subdataset = "(NDVI)",
                    nthreads = 14L,
                    radius = c(1e3, 1e4, 5e4)),
        viirs = list(from = list_paths$viirs,
                    name_covariates = "MOD_LGHTN_0_",
                    subdataset = 3,
                    nthreads = 14L,
                    preprocess = amadeus::process_bluemarble,
                    radius = c(1e3, 1e4, 5e4)),
        geoscf_aqc = list(date = list_common$char_period,
                          path = ain("geos/aqc_tavg_1hr_g1440x721_v1")),
        geoscf_chm = list(date = list_common$char_period,
                          path = ain("geos/chm_tavg_1hr_g1440x721_v1")),
        # base class covariates start here
        hms = list(path = ain("HMS_Smoke/data"),
                  date = list_common$char_period,
                  covariate = "hms", 
                  domain = c("Light", "Medium", "Heavy"),
                  nthreads = 3L,
                  domain_name = "variable"),
        gmted = list(
          path = ain("gmted"),
          covariate = "gmted"
        ),
        nei = list(
          domain = c(2017, 2020),
          domain_name = "year",
          path = ain("nei"),
          covariate = "nei"
        ),
        tri = list(
          domain = seq(2018, 2022),
          domain_name = "year",
          path = ain("tri"),
          radius = c(1e3, 1e4, 5e4),
          covariate = "tri",
          nthreads = 5L
        ),
        nlcd = list(
          domain = c(2019, 2021),
          domain_name = "year",
          path = ain("nlcd/raw"),
          covariate = "nlcd",
          radius = c(1e3, 1e4, 5e4),
          nthreads = 2L,
          max_cells = 1e8
        ),
        koppen = list(path = ain("koppen_geiger/raw/Beck_KG_V1_present_0p0083.tif"), 
                      covariate = "koppen",
                      nthreads = 1L),
        ecoregions = list(path = ain("ecoregions/raw/us_eco_l3_state_boundaries.shp"),
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
          process_function = process_narr2,
          calc_function = calc_narr2,
          nthreads = 24L
        ),
        groads = list(
                      path = ain("sedac_groads/groads-v1-americas-gdb/gROADS-v1-americas.gdb"),
                      covariate = "groads",
                      radius = c(1e3, 1e4, 5e4),
                      nthreads = 3L),
        population = list(
          path = ain("sedac_population/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif"),
          covariate = "population", fun = "mean",
          radius = c(1e3, 1e4, 5e4),
          nthreads = 3L
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
      assign("arglist_common", list_common, envir = .GlobalEnv)
      return(list_proccalc)
    } else {
      qs::qsave(
        list_proccalc, 
        path_export
      )
      return(list_common)
    }
  }


arglist_common <-
  list(
    char_siteid = "site_id",
    char_timeid = "time",
    char_period = c("2018-01-01", "2022-10-31"),
    extent = c(-126, -62, 22, 52),
    user_email = Sys.getenv("USER_EMAIL")
  )

## arglist_paths will include large lists of MODIS/VIIRS product files
## we keep the list of files since the functions for the products
## operate at file paths to automate preprocessing and calculation.
## by using load_modis_files macro, users can predefine the scope of
## file lists, which will result in reducing the number of function calls
## in the main pipeline run.
# arglist_paths <-
#   list(
#     mod11 = load_modis_files("input/modis/raw/61/MOD11A1", date = arglist_common$char_period),
#     mod06 = load_modis_files("input/modis/raw/61/MOD06_L2", date = arglist_common$char_period),
#     mod09 = load_modis_files("input/modis/raw/61/MOD09GA", date = arglist_common$char_period),
#     mcd19 = load_modis_files("input/modis/raw/61/MCD19A2", date = arglist_common$char_period),
#     mod13 = load_modis_files("input/modis/raw/61/MOD13A2", date = arglist_common$char_period),
#     viirs = load_modis_files("input/modis/raw/5000/VNP46A2", "h5$", date = arglist_common$char_period)
#   )

# arglist_proccalc: calculation parameters by raw dataset
# This named and nested list object includes raw data names
# with lists inside. The inner list will be passed to rlang::inject
# to fit in the generalized workflow for most covariate groups.
# Names in the inner lists are usually present in amadeus::process_*
# or amadeus::calc_* functions. Otherwise, some modified functions
# will have their own arguments. Users should know what arguments
# are used in each function in the pipeline and make sure that the
# arguments are available in the target functions to which they are
# passed. Most amadeus functions have the same arguments, but
# a potential issue is that __all__ functions have ellipsis (...) arguments.
# This means that the functions can take any arguments, but the functions
# in the pipeline may not be able to handle them or it could entail
# unexpected consequences or errors. Always consult the original
# function documentation to know what arguments are available and
# the correct format of arguments.
# arglist_proccalc <-
#   list(
#     aqs = list(path = "input/aqs"),
#     mod11 = list(from = arglist_paths$mod11,
#                  name_covariates = sprintf("MOD_SFCT%s_0_", c("D", "N")),
#                  subdataset = "^LST_",
#                  nthreads = 14L,
#                  radius = c(1e3, 1e4, 5e4)),
#     mod06 = list(from = arglist_paths$mod06,
#                  name_covariates = sprintf("MOD_CLCV%s_0_", c("D", "N")),
#                  subdataset = c("Cloud_Fraction_Day", "Cloud_Fraction_Night"),
#                  nthreads = 14L,
#                  preprocess = amadeus::process_modis_swath,
#                  radius = c(1e3, 1e4, 5e4)),
#     mod09 = list(from = arglist_paths$mod09,
#                  name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
#                  subdataset = "^sur_refl_",
#                  nthreads = 14L,
#                  radius = c(1e3, 1e4, 5e4)),
#     mcd19_1km = list(from = arglist_paths$mcd19,
#                      name_covariates = sprintf("MOD_AD%dTA_0_", c(4, 5)),
#                      subdataset = "^Optical_Depth",
#                      nthreads = 14L,
#                      radius = c(1e3, 1e4, 5e4)),
#     mcd19_5km = list(from = arglist_paths$mcd19,
#                      name_covariates = sprintf("MOD_%sAN_0_", c("CSZ", "CVZ", "RAZ", "SCT", "GLN")),
#                      subdataset = "cos|RelAZ|Angle",
#                      nthreads = 14L,
#                      radius = c(1e3, 1e4, 5e4)),
#     mod13 = list(from = arglist_paths$mod13,
#                  name_covariates = "MOD_NDVIV_0_",
#                  subdataset = "(NDVI)",
#                  nthreads = 14L,
#                  radius = c(1e3, 1e4, 5e4)),
#     viirs = list(from = arglist_paths$viirs,
#                  name_covariates = "MOD_LGHTN_0_",
#                  subdataset = 3,
#                  nthreads = 14L,
#                  preprocess = amadeus::process_bluemarble,
#                  radius = c(1e3, 1e4, 5e4)),
#     geoscf_aqc = list(date = arglist_common$char_period,
#                       path = "input/geos/aqc_tavg_1hr_g1440x721_v1"),
#     geoscf_chm = list(date = arglist_common$char_period,
#                       path = "input/geos/chm_tavg_1hr_g1440x721_v1"),
#     # base class covariates start here
#     hms = list(path = "input/HMS_Smoke/data",
#                date = arglist_common$char_period,
#                covariate = "hms", 
#                domain = c("Light", "Medium", "Heavy"),
#                nthreads = 3L,
#                domain_name = "variable"),
#     gmted = list(
#       path = "input/gmted",
#       covariate = "gmted"#,
#       # domain =
#       # expand.grid(
#       #   variables = c(
#       #     "Breakline Emphasis", "Systematic Subsample",
#       #     "Median Statistic", "Minimum Statistic",
#       #     "Mean Statistic", "Maximum Statistic",
#       #     "Standard Deviation Statistic"
#       #   ),
#       #   resolution = sprintf("%s arc-seconds", c("7.5"))
#       # ) %>%
#       #   split(., seq_len(nrow(.))) %>%
#       #   lapply(unlist) %>%
#       #   lapply(unname) %>%
#       #   lapply(as.character),
#       # domain_name = "variable"
#     ),
#     nei = list(
#       domain = c(2017, 2020),
#       domain_name = "year",
#       path = "input/nei",
#       covariate = "nei"
#     ),
#     tri = list(
#       domain = seq(2018, 2022),
#       domain_name = "year",
#       path = "input/tri",
#       radius = c(1e3, 1e4, 5e4),
#       covariate = "tri",
#       nthreads = 5L
#       #year = seq(2018, 2022)
#     ),
#     nlcd = list(
#       domain = c(2019, 2021), # rep(_,c(3,2)) -- how to parametrize?
#       domain_name = "year",
#       path = "input/nlcd/raw",
#       covariate = "nlcd",
#       radius = c(1e3, 1e4, 5e4),
#       nthreads = 2L,
#       max_cells = 1e8
#       #years = c(2019, 2021)
#     ),
#     koppen = list(path = "input/koppen_geiger/raw/Beck_KG_V1_present_0p0083.tif", 
#                   covariate = "koppen",
#                   nthreads = 1L),
#     ecoregions = list(path = "input/ecoregions/raw/us_eco_l3_state_boundaries.shp",
#                       covariate = "ecoregions",
#                       nthreads = 1L),
#     narr = list(
#       path = "input/narr",
#       covariate = "narr",
#       domain_reduced = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
#                     "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
#                     "pres.sfc", "shtfl", "snowc", "soilm",    
#                     "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd"),
#       domain_appt = c("prate", "shum"),
#       domain = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
#                     "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
#                     "prate", "pres.sfc", "shtfl", "shum", "snowc", "soilm",    
#                     "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd"),
#       domain_name = "variable",
#       date = arglist_common$char_period,
#       process_function = process_narr2,
#       calc_function = calc_narr2,
#       nthreads = 24L
#     ),
#     groads = list(
#                   path = "input/sedac_groads/groads-v1-americas-gdb/gROADS-v1-americas.gdb",
#                   covariate = "groads",
#                   radius = c(1e3, 1e4, 5e4),
#                   nthreads = 3L),
#     population = list(
#       path = "input/sedac_population/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif",
#       covariate = "population", fun = "mean",
#       radius = c(1e3, 1e4, 5e4),
#       nthreads = 3L
#     )
#   )

# attr(arglist_proccalc, "description") <-
#   tibble::tribble(
#     ~dataset, ~description,
#     "mod11", "MODIS Land Surface Temperature Day/Night",
#     "mod06", "MODIS Cloud Fraction Day/Night",
#     "mod09", "MODIS Surface Reflectance",
#     "mcd19_1km", "MCD19A2 1km",
#     "mcd19_5km", "MCD19A2 5km",
#     "mod13", "MODIS Normalized Difference Vegetation Indexß",
#     "viirs", "VIIRS Nighttime Lights",
#     "hms", "NOAA Hazard Mapping System Smoke",
#     "geoscf_aqc", "GEOS-CF AQC",
#     "geoscf_chm", "GEOS-CF CHM",
#     "gmted", "GMTED elevation",
#     "nei", "National Emission Inventory",
#     "tri", "Toxic Release Inventory",
#     "nlcd", "National Land Cover Database",
#     "koppen", "Koppen-Geiger Climate Classification",
#     "ecoregions", "EPA Ecoregions",
#     "narr", "NARR",
#     "groads", "SEDAC Global Roads",
#     "population", "SEDAC Population Density"
#   )



# arglist_download <-
#   list(

#     mod11 = list(name_covariates = sprintf("MOD_SFCT%s_0_", c("D", "N")),
#                  subdataset = sprintf("LST_%s_", c("Day", "Night"))),
#     mod06 = list(name_covariates = sprintf("MOD_CLCV%s_0_", c("Day", "Night")),
#                  subdataset = sprintf("Cloud_Fraction_%s", c("Day", "Night"))),
#     mod09 = list(name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
#                  subdataset = seq(2, 8)),
#     mcd19 = list(name_covariates =
#                  list(
#                    res1km = sprintf("MOD_AD%dTA_0_", c(4, 5)),
#                    res5km = sprintf("MOD_%sAN_0_", c("CSZ", "CVZ", "RAZ", "SCT", "GLN"))
#                  ),
#                  subdataset =
#                  list(
#                    res1km = c("Optical_Depth"),
#                    res5km = c("cos|RelAZ|Angle")
#                  )
#     ),
#     mod13 = list(name_covariates = "MOD_NDVIV_0_",
#                  subdataset = "(NDVI)"),
#     viirs = list(name_covariates = "MOD_LGHTN_0_",
#                  subdataset = 3),
#     hms = list(levels = c("Light", "Medium", "Heavy")),
#     geoscf = list(element = list(aqc = "aqc", chm = "chm")),
#     gmted = list(
#       variables = c(
#         "Breakline Emphasis", "Systematic Subsample",
#         "Median Statistic", "Minimum Statistic",
#         "Mean Statistic", "Maximum Statistic",
#         "Standard Deviation Statistic"
#       ),
#       resolution = sprintf("%s arc-seconds", c("7.5", "15", "30"))
#     ),
#     nei = list(
#       years = c(2017, 2020),
#       county = process_counties(year = 2020)
#     ),
#     tri = list(
#       years = seq(2018, 2022)
#     ),
#     nlcd = list(
#       years = c(2019, 2021)
#     ),
#     koppen = list(),
#     ecoregions = list(),
#     narr = list(
#       variables = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
#                     "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
#                     "prate", "pres.sfc", "shtfl", "shum", "snowc", "soilm",    
#                     "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd")
#     ),
#     groads = list(),
#     population = list(fun = "mean")
#   )


# export
# time_create <- gsub("[[:punct:]]|[[:blank:]]", "", Sys.time())

# Generated argument list for the pipeline is stored to
# the desired location. The current pipeline setting accepts
# the argument list in RDS format.
# qs::qsave(
#   arglist_proccalc, 
#   "./inst/targets/punchcard_calc.qs"
# )
