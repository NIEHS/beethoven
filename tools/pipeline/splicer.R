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
