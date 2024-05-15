library(dplyr)
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

# blueprint <-
#     tribble(
#         ~dataset,   ~buffers, ~input,
#         "mod11",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD11A1",
#         "mod13",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD13A2",
#         "mcd19",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MCD19A2",
#         "mod06",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD06_L2",
#         "mod09",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD09GA",
#         "viirs",    c(1e3, 1e4, 5e4),    "input/modis/raw/5000/VNP46A2",
#         "ecoregions",    c(-1),    "input/ecoregions",
#         "tri",  c(1e3, 1e4, 5e4),    "input/tri",
#         "nei",  c(-1),   "input/nei",
#         "hms",  c(-1),    "input/hms",
#         "koppen_geiger",   c(0),    "input/koppen_geiger",
#         "groads",   c(1e3, 1e4, 5e4),    "input/sedac_groads/gROADS-v1-americas.gdb",
#         "population",  c(1e3, 1e4, 5e4),    "input/sedac_population/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif",
#         "nlcd", c(1e3, 1e4, 5e4),    "input/nlcd",
#         "geoscf", c(1e3, 1e4, 5e4),    "input/geos",
#         "gmted",  c(0),    "input/gmted",
#         "narr", c(0),    "input/narr"
#     )
source("inst/targets/pipeline_base_functions.R")

## This file is to define required parameters that are reused
## throughout the pipeline.
## First, let's start with the list of objects generated in this code.
## arglist_common: common information for running the pipeline
##   site identifiers, time identifier, time period of computation,
##   spatial extent of computation, and an email address (for receiving
##   computing node status).
arglist_common <-
  list(
    char_siteid = "site_id",
    char_timeid = "time",
    char_period = c("2022-01-01", "2022-10-31"),
    extent = c(-126, -62, 22, 52),
    user_email = "songi2@nih.gov"
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
# saveRDS(arglist_proccalc, 
#         "./inst/targets/punchcard_calc.rds",
#         compress = "xz")
