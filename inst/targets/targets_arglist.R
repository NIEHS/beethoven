
arglist_proccalc <-
  list(
    common = list(
      char_siteid = "site_id",
      char_timeid = "time",
      char_period = c("2018-01-01", "2022-12-31"),
      extent = c(-126, -62, 22, 52)
    ),
    mod11 = list(name_covariates = sprintf("MOD_SFCT%s_0_", c("D", "N")),
                 subdataset = sprintf("LST_%s_", c("Day", "Night")),
                 nthreads = 20L),
    mod06 = list(name_covariates = sprintf("MOD_CLCV%s_0_", c("Day", "Night")),
                 subdataset = sprintf("Cloud_Fraction_%s", c("Day", "Night")),
                 nthreads = 20L,
                 preprocess = amadeus::process_modis_swath),
    mod09 = list(name_covariates = sprintf("MOD_SFCRF_%d_", seq(1, 7)),
                 subdataset = seq(2, 8),
                 nthreads = 20L),
    mcd19_1km = list(name_covariates = sprintf("MOD_AD%dTA_0_", c(4, 5)),
                     subdataseet = "Optical_Depth",
                     nthreads = 20L),
    mcd19_5km = list(name_covariates = sprintf("MOD_%sAN_0_", c("CSZ", "CVZ", "RAZ", "SCT", "GLN")),
                     subdataset = c("cos|RelAZ|Angle"),
                     nthreads = 20L)
    ,
    mod13 = list(name_covariates = "MOD_NDVIV_0_",
                 subdataset = "(NDVI)",
                 nthreads = 20L),
    viirs = list(name_covariates = "MOD_LGHTN_0_",
                 subdataset = 3,
                 nthreads = 20L,
                 preprocess = amadeus::process_bluemarble),
    hms = list(covariate = "hms", levels = c("Light", "Medium", "Heavy")),
    geoscf_aqc = list(covariate = "geoscf", element = "aqc"),
    geoscf_chm = list(covariate = "geoscf", element = "chm"),
    gmted = list(
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
      covariate = "nei",
      years = c(2017, 2020),
      county = process_counties(year = 2020)
    ),
    tri = list(
      covariate = "tri",
      years = seq(2018, 2022)
    ),
    nlcd = list(
      covariate = "nlcd",
      years = c(2019, 2021)
    ),
    koppen_geiger = list(covariate = "koppen_geiger"),
    ecoregions = list(covariate = "ecoregions"),
    narr = list(
      covariate = "narr",
      variables = c("air.sfc", "albedo", "apcp", "dswrf", "evap", "hcdc",
                    "hpbl", "lcdc", "lhtfl", "mcdc", "omega", "pr_wtr",
                    "prate", "pres.sfc", "shtfl", "shum", "snowc", "soilm",    
                    "tcdc", "ulwrf.sfc", "uwnd.10m", "vis", "vwnd.10m", "weasd")
    ),
    groads = list(covariate = "groads"),
    population = list(covariate = "population", years = c(2020), fun = "mean")
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
#saveRDS(arglist_proccalc, "inst/targets/punchcard_calc.rds", compress = "xz")
