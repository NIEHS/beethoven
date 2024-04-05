## for dynamic branching

library(dplyr)

blueprint <-
    tribble(
        ~dataset,   ~buffers, ~input,
        "mod11",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD11A1",
        "mod13",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD13A2",
        "mcd19",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MCD19A2",
        "mod06",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD06_L2",
        "mod09",    c(1e3, 1e4, 5e4),    "input/modis/raw/61/MOD09GA",
        "vnp46",    c(1e3, 1e4, 5e4),    "input/viirs/raw/61/VNP46A2",
        "ecoregion",    c(-1),    "input/ecoregions",
        "tri",  c(1e3, 1e4, 5e4),    "input/tri",
        "nei",  c(-1),   "input/nei",
        "hms",  c(-1),    "input/hms",
        "koppen",   c(0),    "input/koppen_geiger",
        "groads",   c(1e3, 1e4, 5e4),    "input/sedac_groads",
        "pop",  c(1e3, 1e4, 5e4),    "input/sedac_population",
        "nlcd", c(1e3, 1e4, 5e4),    "input/nlcd",
        "geos", c(1e3, 1e4, 5e4),    "input/geos",
        "gmted",  c(1e3, 1e4, 5e4),    "input/gmted",
        "narr_monolevel", c(1e3, 1e4, 5e4),    "input/narr/monolevel",
        "narr_plevels", c(1e3, 1e4, 5e4),    "input/narr/p_levels"
    )

blueprint <- blueprint |>
  tidyr::unnest(cols = buffers)

readr::write_csv(blueprint, "tools/pipeline/blueprint.csv")

# quicktest meta-tar
targets::tar_target(as.symbol(sprintf("%s_%d", "target0", 1)), foo())
shQuote(as.symbol(sprintf("%s_%d", "target0", 1)))

targets::tar_target("c", 1)
targets::tar_target(name = sprintf("%s", "c"), 1)

path0 <-
  list.files(
    file.path("../../../..", "group/set/Projects/NRT-AP-Model", "input", "aqs"),
    "daily_88101_[0-9]{4}.csv",
    full.names = TRUE
  )
sspat <- amadeus::process_aqs(
  path = path0
) #read_locs(mr("dir_input_aqs"))

sspat[, 1:3] |>
  sf::st_as_sf() |>
  unique() -> sspat0

dyn <-
  calculate_multi(
    # sequence: could be refered from dates
    domain = c(2018, 2019, 2020, 2021, 2022),
    locs = sspat0[1:10, ],
    path = mr("dir_input_tri"),
    covariate = "tri",
    locs_id = mr("pointid")
  )

source("./tools/pipeline/pipeline_base_functions.R")

amadeus::calc_koppen_geiger(
  kg, terra::vect(sspat0), locs_id = "site_id"
)
kg <- amadeus::process_koppen_geiger(path = file.path(mr("dir_input_koppen"), "Beck_KG_V1_present_0p083.tif"))
amadeus::process_nlcd()
amadeus::calc_nlcd()
amadeus::calc_hms()
project(vect(sspat0), crs(kg))

sspat0s <- terra::vect(sspat0[1:10, ])
calculate_single(
  covariate = "koppen",
  locs_id = mr("pointid"),
  locs = sspat[1:10, ],
  path = file.path(mr("dir_input_koppen"), "Beck_KG_V1_present_0p083.tif"),
  year = NULL
)
calculate_single(
  covariate = "ecoregion",
  locs_id = mr("pointid"),
  locs = sspat[1:10, ],
  path = file.path(mr("dir_input_ecoregion"), "raw/us_eco_l3_state_boundaries.shp")
)


hmsd <- as.Date(
        stringi::stri_extract_first_regex(
          list.files(
            "../../../../group/set/Projects/NRT-AP-Model/input/HMS_Smoke/data", pattern = "*.shp$", full.names = FALSE
          ),
          pattern = "2[0-1][0-9]{2}[0-1][0-9]([0-2][0-9]|3[0-1])"
        ), format = "%Y%m%d"
      )
hmj <-
purrr::map(
        .x = hmsd[1:50],
        .f = function(d) {
          calculate_single(
            locs = sspat0s,
            covariate = "hms",
            # FIXME: mr("dir_input_hms")
            path = "../../../../group/set/Projects/NRT-AP-Model/input/HMS_Smoke/data",
            date = c(d, d),
            variable = "Heavy",
            locs_id = mr("pointid")
          )
        })
do.call(rbind, hmj)


rx <-
calculate_single(
        locs = sf::st_drop_geometry(sspat0),
        # fix mr("dir..._groads")
        path = "../../../../group/set/Projects/NRT-AP-Model/input/sedac_groads/gROADS-v1-americas.gdb",
        locs_id = mr("pointid"),
        covariate = "roads",
        radius = 1000
      )


amadeus::calc_sedac_population()

amadeus::process_sedac_groads()
amadeus::calc_sedac_groads()
narcheck <-
amadeus::process_narr(path = "../../../../group/set/Projects/NRT-AP-Model/input/narr/omega", variable = "omega", date = c("2020-03-01", "2020-03-01"))
narcheck2 <-
amadeus::process_narr(path = "../../../../group/set/Projects/NRT-AP-Model/input/narr/omega", variable = "omega", date = c("2020-03-01", "2022-01-01"))


# amadeus::process_narr(path = "../../../../group/set/Projects/NRT-AP-Model/input/narr/omega", variable = "omega", date = c("2020-03-01", "2022-01-01"))
# 


amadeus::calc_nei()
calc_nei(from = NULL, locs = NULL, locs_id = "site_id", ...)
amadeus::process_nei()
process_nei(path = NULL, county = NULL, year = c(2017, 2020))


neicheck <-
      calculate_multi(
        # manual domain
        domain = c(2017, 2017, 2020, 2020, 2020),
        locs = terra::vect(sspat0),
        covariate = "nei",
        path = mr("dir_input_nei"),
        county = lco,
        locs_id = mr("pointid")
      )
lconei <-
process_nei(path = "./input/nei/nei_onroad_byregions_2017", county = lco, year = c(2017))



amadeus::process_gmted()
process_gmted(variable = NULL, path = NULL, ...)

amadeus::calc_gmted()
calc_gmted(from, locs, locs_id = NULL, radius = 0, fun = "mean", ...)



amadeus::process_geos()
process_geos(
  date = c("2018-01-01", "2018-01-01"),
  variable = NULL,
  path = NULL,
  ...
)
amadeus::calc_geos()
calc_geos(from, locs, locs_id = NULL, radius = 0, fun = "mean", ...)
