library(dplyr)
# basic idea: a list that provides
# anything needed for the pipeline
# ultimately the name "dataset" is a key for iteration to make
# the outermost pipeline look as simple as possible
blueprint <-
    tribble(
        ~dataset,   ~buffers, ~input,
        "mod11",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD11A1",
        "mod13",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD13A2",
        "mcd19",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MCD19A2",
        "mod06",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD06_L2",
        "mod09",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD09GA",
        "viirs",    c(1e3, 1e4, 5e4),    "input/modis/raw/5000/VNP46A2",
        "ecoregions",    c(-1),    "input/ecoregions",
        "tri",  c(1e3, 1e4, 5e4),    "input/tri",
        "nei",  c(-1),   "input/nei",
        "hms",  c(-1),    "input/hms",
        "koppen_geiger",   c(0),    "input/koppen_geiger",
        "groads",   c(1e3, 1e4, 5e4),    "input/sedac_groads/gROADS-v1-americas.gdb",
        "population",  c(1e3, 1e4, 5e4),    "input/sedac_population/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif",
        "nlcd", c(1e3, 1e4, 5e4),    "input/nlcd",
        "geoscf", c(1e3, 1e4, 5e4),    "input/geos",
        "gmted",  c(0),    "input/gmted",
        "narr", c(0),    "input/narr"
    )


arglist_common <-
  list(
    common = list(
      char_siteid = "site_id",
      char_timeid = "time",
      char_period = c("2018-01-01", "2022-12-31"),
      extent = c(-126, -62, 22, 52)
    ),
    path_files = list(
      mod11 = load_modis_files("./input/modis/raw/61/MOD11A1"),
      mod06 = load_modis_files("./input/modis/raw/61/MOD06_L2"),
      mod09 = load_modis_files("./input/modis/raw/61/MOD09GA"),
      mcd19 = load_modis_files("./input/modis/raw/61/MCD19A2"),
      mod13 = load_modis_files("./input/modis/raw/61/MOD13A2"),
      viirs = load_modis_files("./input/modis/raw/5000/VNP46A2", "h5$")
    )
  )

arglist_proccalc <-
  list(
    mod11 = list(from = arglist_common$path_files$mod11,
                 name_covariates = sprintf("MOD_SFCT%s_0_", c("D", "N")),
                 subdataset = sprintf("LST_%s_", c("Day", "Night")),
                 nthreads = 20L),
    mod06 = list(from = arglist_common$path_files$mod06,
                 name_covariates = sprintf("MOD_CLCV%s_0_", c("Day", "Night")),
                 subdataset = sprintf("Cloud_Fraction_%s", c("Day", "Night")),
                 nthreads = 20L,
                 preprocess = amadeus::process_modis_swath),
    mod09 = list(from = arglist_common$path_files$mod09,
                 name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
                 subdataset = seq(2, 8),
                 nthreads = 20L),
    mcd19_1km = list(from = arglist_common$path_files$mcd19,
                     name_covariates = sprintf("MOD_AD%dTA_0_", c(4, 5)),
                     subdataseet = "Optical_Depth",
                     nthreads = 20L),
    mcd19_5km = list(from = arglist_common$path_files$mcd19,
                     name_covariates = sprintf("MOD_%sAN_0_", c("CSZ", "CVZ", "RAZ", "SCT", "GLN")),
                     subdataset = c("cos|RelAZ|Angle"),
                     nthreads = 20L),
    mod13 = list(from = arglist_common$path_files$mod13,
                 name_covariates = "MOD_NDVIV_0_",
                 subdataset = "(NDVI)",
                 nthreads = 20L),
    viirs = list(from = arglist_common$path_files$viirs,
                 name_covariates = "MOD_LGHTN_0_",
                 subdataset = 3,
                 nthreads = 20L,
                 preprocess = amadeus::process_bluemarble),
    hms = list(from = "./input/HMS_Smoke/data",
               covariate = "hms", levels = c("Light", "Medium", "Heavy")),
    geoscf_aqc = list(date = arglist_common$common$char_period,
                      from = "./input/geos/aqc_tavg_1hr_g1440x721_v1",
                      covariate = "geoscf", element = "aqc"),
    geoscf_chm = list(date = arglist_common$common$char_period,
                      from = "./input/geos/chm_tavg_1hr_g1440x721_v1",
                      covariate = "geoscf", element = "chm"),
    gmted = list(
      from = "./input/gmted",
      covariate = "gmted",
      variables = c(
        "Breakline Emphasis", "Systematic Subsample",
        "Median Statistic", "Minimum Statistic",
        "Mean Statistic", "Maximum Statistic",
        "Standard Deviation Statistic"
      ),
      resolution = sprintf("%s arc-seconds", c("7.5", "15", "30"))
    ),
    nei = list(
      domain = c(2017, 2017, 2020, 2020, 2020),
      from = "./input/nei",
      covariate = "nei",
      years = c(2017, 2020),
      county = process_counties(year = 2020)
    ),
    tri = list(
      domain = seq(2018, 2022),
      from = "./input/tri",
      covariate = "tri",
      years = seq(2018, 2022)
    ),
    nlcd = list(
      domain = c(2019, 2019, 2019, 2021, 2021),
      from = "./input/nlcd/raw",
      covariate = "nlcd",
      years = c(2019, 2021)
    ),
    koppen_geiger = list(from = "./input/koppen_geiger/raw",
                         covariate = "koppen_geiger"),
    ecoregions = list(from = "./input/ecoregions/raw",
                      covariate = "ecoregions"),
    narr = list(
      from = "./input/narr",
      covariate = "narr",
      variables = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
                    "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
                    "prate", "pres.sfc", "shtfl", "shum", "snowc", "soilm",    
                    "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd")
    ),
    groads = list(from = "./input/sedac_groads/gROADS-v1-americas.gdb",
                  covariate = "groads"),
    population = list(
      domain = rep(2020, 5),
      from = "./input/sedac_population//ddn/gs1/home/songi2/projects/beethoven/input/sedac_population/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif",
      covariate = "population", years = c(2020), fun = "mean"
    )
  )

attr(arglist_proccalc, "description") <-
  tribble(
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
    "koppen_geiger", "Koppen-Geiger Climate Classification",
    "ecoregions", "EPA Ecoregions",
    "narr", "NARR",
    "groads", "SEDAC Global Roads",
    "population", "SEDAC Population Density"
  )



arglist_download <-
  list(

    mod11 = list(name_covariates = sprintf("MOD_SFCT%s_0_", c("D", "N")),
                 subdataset = sprintf("LST_%s_", c("Day", "Night"))),
    mod06 = list(name_covariates = sprintf("MOD_CLCV%s_0_", c("Day", "Night")),
                 subdataset = sprintf("Cloud_Fraction_%s", c("Day", "Night"))),
    mod09 = list(name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
                 subdataset = seq(2, 8)),
    mcd19 = list(name_covariates =
                 list(
                   res1km = sprintf("MOD_AD%dTA_0_", c(4, 5)),
                   res5km = sprintf("MOD_%sAN_0_", c("CSZ", "CVZ", "RAZ", "SCT", "GLN"))
                 ),
                 subdataset =
                 list(
                   res1km = c("Optical_Depth"),
                   res5km = c("cos|RelAZ|Angle")
                 )
    ),
    mod13 = list(name_covariates = "MOD_NDVIV_0_",
                 subdataset = "(NDVI)"),
    viirs = list(name_covariates = "MOD_LGHTN_0_",
                 subdataset = 3),
    hms = list(levels = c("Light", "Medium", "Heavy")),
    geoscf = list(element = list(aqc = "aqc", chm = "chm")),
    gmted = list(
      variables = c(
        "Breakline Emphasis", "Systematic Subsample",
        "Median Statistic", "Minimum Statistic",
        "Mean Statistic", "Maximum Statistic",
        "Standard Deviation Statistic"
      ),
      resolution = sprintf("%s arc-seconds", c("7.5", "15", "30"))
    ),
    nei = list(
      years = c(2017, 2020),
      county = process_counties(year = 2020)
    ),
    tri = list(
      years = seq(2018, 2022)
    ),
    nlcd = list(
      years = c(2019, 2021)
    ),
    koppen_geiger = list(),
    ecoregions = list(),
    narr = list(
      variables = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
                    "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
                    "prate", "pres.sfc", "shtfl", "shum", "snowc", "soilm",    
                    "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd")
    ),
    groads = list(),
    population = list(fun = "mean")
  )


# export
# saveRDS(arglist_proccalc, "./inst/targets/punchcard_calc.rds", compress = "xz")
