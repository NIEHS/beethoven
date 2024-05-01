
system.time(
    rlang::inject(
    calc_geos_strict(
        locs = sf::st_as_sf(data.frame(x = -88.9, y = 34, site_id = "1", time = "2021-01-01"), coords = 1:2, crs = 4326),
        locs_id = "site_id",
        win = c(-126, -66, 23, 50),
        snap = "out",
        !!!loadargs("inst/targets/punchcard_calc.rds", "geoscf_chm")
    ))
)


loadargs("inst/targets/punchcard_calc.rds", "geoscf_chm")$path |>
    list.files("*.nc", full.names = T) |>
    _[[1]]
terra::describe(
    "input/geos/aqc_tavg_1hr_g1440x721_v1/GEOS-CF.v01.rpl.aqc_tavg_1hr_g1440x721_v1.20180101_0030z.nc4",
    sds = TRUE)$var

amadeus::process_gmted(path = "input/gmted", variable = x)
amadeus::generate_date_sequence(
    "2021-01-01",
    "2021-01-02",
    sub_hyphen = F
)

source("inst/targets/pipeline_base_functions.R")
varn <- "hms"
system.time(
    jj <-
    inject_calculate(
        covariate = varn,
        locs = tar_read(sf_feat_proc_aqs_sites)[1:50,],
        #nthreads = 1L,
        injection = loadargs("inst/targets/punchcard_calc.rds", varn))
    )
jj

calc_covariates(
    covariate = "nlcd",
    from = process_covariates(covariate = "nlcd", path = "input/nlcd/raw", year = 2019L),
    locs = tar_read(sf_feat_proc_aqs_sites),
    locs_id = "site_id",
    radius = 1000
)

system.time(
    jk <-
    calc_gmted_direct(
        locs = tar_read(sf_feat_proc_aqs_sites)[1,],
        locs_id = "site_id",
        path = "input/gmted",
        radius = 0,
        variable = c("Breakline Emphasis", "7.5 arc-seconds")
    )
)
head(jk)
jj

kl <- terra::rast("input/narr/omega/omega.202101.nc")
kk <-
process_narr(path = "input/narr", variable = c("shum"), date = c("2021-01-31", "2021-02-02"))

kke <- (calc_narr(from = kk, locs = targets::tar_read(sf_feat_proc_aqs_sites), locs_id = "site_id"))

xj <- terra::extract(kk, targets::tar_read(sf_feat_proc_aqs_sites) |> terra::vect(), bind = TRUE)
xjj <- exactextractr::exact_extract(kk, targets::tar_read(sf_feat_proc_aqs_sites) |> sf::st_buffer(0.000001), stack_apply =TRUE, fun = "mean", force_df = TRUE) 
kk[targets::tar_read(sf_feat_proc_aqs_sites)[1,] |> terra::vect()]
targets::tar_read(sf_feat_proc_aqs_sites)[1,] |> terra::vect() -> vc1
vc1 <- terra::project(vc1, terra::crs(kk))
xj <- terra::extract(kk, vc1, bind = TRUE)

system.time(
    jk <-
    inject_gmted(
        locs = tar_read(sf_feat_proc_aqs_sites)[1:10,],
        #locs_id = "site_id",
        #path = "input/gmted",
        radii = c(0,1000),
        variable = c("Breakline Emphasis"),
        injection = loadargs("inst/targets/punchcard_calc.rds", "gmted")
    )
)


system.time(
    hx <-
      amadeus::process_hms(date = c("2020-01-01", "2020-04-30"),
        variable = "Medium", path = "input/HMS_Smoke/data")
)
system.time(
    hxe <-
      amadeus::calc_hms(hx, targets::tar_read(sf_feat_proc_aqs_sites), locs_id = "site_id")
)


amadeus::process_gmted(path = "input/gmted/", variable = c("Breakline Emphasis", "7.5 arc-seconds"))
terra::rast("input/gmted/be75_grd")


amadeus::process_gmted


process_covariates(covariate = "hms")
calculate(
    covariate = "hms",
    path = "input/HMS_Smoke/data",
    locs = tar_read(sf_feat_proc_aqs_sites),
    variable = "Medium",
    domain_name = "whatever"

)

calculate(
    locs = targets::tar_read(sf_feat_proc_aqs_sites),
    locs_id = "site_id",
    domain = rep(2020),
    path = "input/sedac_groads/groads-v1-americas-gdb/gROADS-v1-americas.gdb",
    covariate = "groads",
    domain_name = "year",
    radius = c(1e3, 1e4, 5e4),
    nthreads = 3L
)


calculate(
        population = list(
      domain = rep(2020, 5),
      domain_name = "year",
      path = "input/sedac_population/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif",
      covariate = "population", fun = "mean",
      radius = c(1e3, 1e4, 5e4),
      nthreads = 3L
    )

)


narrd <-
calculate(
    covariate = "narr",
    path = "input/narr",
    locs = targets::tar_read(sf_feat_proc_aqs_sites),
    locs_id = "site_id",
    domain = c("omega"),
    domain_name = "variable",
    date = c("2020-01-30", "2020-02-05"),
    process_function = process_narr2,
    calc_function = calc_narr2,
    nthreads = 1L
)

kk <-
process_narr(path = "input/narr", variable = c("omega"), date = c("2021-01-31", "2021-02-05"))

kke <- (calc_narr(from = kk, locs = targets::tar_read(sf_feat_proc_aqs_sites), locs_id = "site_id"))


kk <- amadeus::process_ecoregion(path = "input/ecoregions/raw")
kx <- amadeus::calc_ecoregion(from = kk, locs = tar_read(sf_feat_proc_aqs_sites) |> terra::vect())

kl <- amadeus::process_covariates("ecoregions", path = "input/ecoregions/raw")
kz <- amadeus::calc_covariates("ecoregions", from = kl, locs = tar_read(sf_feat_proc_aqs_sites) |> terra::vect())
as.data.table(kz)
