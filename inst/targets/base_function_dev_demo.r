
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

profvis::profvis(
kx <- amadeus::calc_covariates(
    covariate = "nlcd",
    from = amadeus::process_covariates(covariate = "nlcd", path = "input/nlcd/raw", year = 2019L),
    locs = tar_read(sf_feat_proc_aqs_sites)[1:200,],
    locs_id = "site_id",
    radius = 50000,
    max_cells = 1e8
)
)

system.time(
    jk <-
    calc_gmted_direct(
        locs = tar_read(sf_feat_proc_aqs_sites)[1:100,],
        locs_id = "site_id",
        path = "input/gmted",
        radius = 1000,
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



## R code example for temporal join utilizing join_by and between in dplyr package
library(dplyr)

# Create two data frames for the example
df1 <- data.frame(
    id = 1:3,
    start_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    end_date = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31"))
)

df2 <- data.frame(
    id = c(1, 2, 2, 3, 3),
    date = as.Date(c("2020-01-15", "2020-02-15", "2020-02-20", "2020-03-15", "2020-03-20"))
)

btw <- function(x, from, to) (x >= from & x <= to)
# Perform the temporal join
result <- df2 %>%
    left_join(df1, by = join_by(id == id, !!rlang::sym("date") <= end_date))# %>%
    #filter(between(date, start_date, end_date))

result


post_calc_join_yeardate <-
  function(
    df_year,
    df_date,
    field_year = "time",
    field_date = "time",
    spid = "site_id"
  ) {
    if (!inherits(df_year, "data.frame") && !inherits(df_date, "data.frame")) {
      stop("Both inputs should be data.frame.")
    }
    df_date_joined <-
    df_date[df_year,
            on = .(spid == spid, field_year == as.POSIXlt(field_date)$year)
            #union(names(df_year), names(df_date)),
            #with = FALSE
           ]

    # names(df_year)[names(df_year) %in% field_year] <- "year"
    # df_date$year <- as.integer(substr(df_date[[field_date]], 1, 4))
    # #as.integer(format(as.Date(df_date[[field_date]]), "%Y"))
    # df_joined <-
    #   data.table::merge.data.table(
    #     df_date, df_year,
    #     by = c(spid, "year"),
    #     all.x = TRUE
    #   )

    # df_joined <- df_joined[, c("year") := NULL]
    return(df_date_joined)
  }

jj <- \(x) eval(x);quote(x)
jj("joy")
eval("joy")
quote("joy")
  library(data.table)

  # Generate sample data
  df_year <- data.table(
    site_id = rep(1:10, each = 10),
    time = rep(2000:2009, times = 10),
    x = rnorm(100, 3, 1)
  )
  df_year[["year"]] = 1:100
  df_date <- data.table(
    site_id = rep(1:10, each = 10),
    time = rep(as.Date("2000-01-01") + 0:9, times = 10),
    y = rpois(100, 4)
  )
  
  # Call the function
  result <- post_calc_join_yeardate(df_year, df_date)
  


## read_locs
rr <- read_locs(
    fun_aqs = amadeus::process_aqs,
    path = 
      list.files("input/aqs", pattern = "daily_88101_2018.csv$", full.names = TRUE),
    date = c("2018-01-15", "2018-02-21"),
    mode = "sparse",
    return_format = "data.table")


terra::crs("EPSG:4326")

kx <-
amadeus::process_nlcd(
  path = "input/nlcd/raw",
  year = 2019
)

kxt <- amadeus::calc_nlcd(
  from = kx,
  locs = tar_read(sf_feat_proc_aqs_sites),
  locs_id = "site_id",
  radius = 1000,
  max_cells = 1e8
)

tar_read(sf_feat_proc_aqs_sites) -> kk
sf::st_as_sf(kk, coords = 2:3, crs = 4326) -> kk


calc_nlcd <- function(from,
                      locs,
                      locs_id = "site_id",
                      radius = 1000,
                      max_cells = 1e8,
                      ...) {
  # check inputs
  if (!is.numeric(radius)) {
    stop("radius is not a numeric.")
  }
  if (radius <= 0) {
    stop("radius has not a likely value.")
  }
  if (!methods::is(locs, "SpatVector")) {
    message("locs is not a terra::SpatVector.")
    locs <- tryCatch({
      if (data.table::is.data.table(locs)) {
        locs <- as.data.frame(locs)
      }
      locsa <- terra::deepcopy(terra::vect(locs))
      locs_crs <- terra::crs(locsa)
      if (locs_crs == "" || is.na(locs_crs)) {
        terra::crs(locsa) <- "EPSG:4326"
      }
      locsa
    },
    error = function(e) {
      stop("Failed to locs to a terra::SpatVector.")
    }
    )
  }
  if (!methods::is(from, "SpatRaster")) {
    stop("from is not a SpatRaster.")
  }
  year <- try(as.integer(terra::metags(from, name = "year")))
  # select points within mainland US and reproject on nlcd crs if necessary
  us_main <-
    terra::ext(c(xmin = -127, xmax = -65, ymin = 24, ymax = 51)) |>
    terra::vect() |>
    terra::set.crs("EPSG:4326") |>
    terra::project(y = terra::crs(locs))
  data_vect_b <- locs |>
    terra::intersect(x = us_main)
  data_vect_b <- terra::project(data_vect_b, terra::crs(from))
  # create circle buffers with buf_radius
  bufs_pol <- terra::buffer(data_vect_b, width = radius) |>
    sf::st_as_sf() |>
    sf::st_geometry()
  # ratio of each nlcd class per buffer
  nlcd_at_bufs <-
    exactextractr::exact_extract(
      from,
      bufs_pol,
      fun = "frac",
      #stack_apply = TRUE,
      force_df = TRUE,
      progress = FALSE,
      max_cells_in_memory = max_cells)
  # select only the columns of interest
  cfpath <- system.file("extdata", "nlcd_classes.csv", package = "amadeus")
  nlcd_classes <- utils::read.csv(cfpath)
  nlcd_at_bufs <-
    nlcd_at_bufs[
      sort(names(nlcd_at_bufs)[
        grepl(paste0("frac_(", paste(nlcd_classes$value, collapse = "|"), ")"),
              names(nlcd_at_bufs))
      ])
    ]
  # change column names
  nlcd_names <- names(nlcd_at_bufs)
  nlcd_names <- sub(pattern = "frac_", replacement = "", x = nlcd_names)
  nlcd_names <- as.numeric(nlcd_names)
  nlcd_names <- nlcd_classes[nlcd_classes$value %in% nlcd_names, c("class")]
  new_names <- sapply(
    nlcd_names,
    function(x) {
      sprintf("LDU_%s_0_%05d", x, radius)
    }
  )
  names(nlcd_at_bufs) <- new_names
  # merge data_vect with nlcd class fractions (and reproject)
  new_data_vect <- cbind(data_vect_b, nlcd_at_bufs)
  new_data_vect <- terra::project(new_data_vect, terra::crs(locs))
  new_data_vect$time <- as.integer(year)
  return(new_data_vect)
}


system.time(
kxt <- amadeus::calc_nlcd(
  from = kx,
  locs = tar_read(sf_feat_proc_aqs_sites),
  locs_id = "site_id",
  radius = 1000,
  max_cells = 1e6
)
)

read_locs(
  path = list.files("input/aqs", pattern = "daily_88101_*.*.csv$", full.names = TRUE),
  date = c("2018-01-15", "2018-02-21"),
  mode = "location",
  return_format = "sf"
)


##
xl <- tar_read(list_feat_calc_base)[[1]]
xlr <- lapply(xl, \(dt) dt[, time := as.character(time)])
reduce_merge(xlr)