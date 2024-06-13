library(amadeus)
library(terra)
library(sf)
sf_use_s2(F)

# file loading
cnty <- readRDS("/ddn/gs1/home/songi2/projects/beethoven/input/counties_2018_2022.rds")
#nei <- process_nei(path = "~/projects/beethoven/input/nei/", )
neiyears <- c(rep(2017, 2), rep(2020, 3))
nom_years <- seq(2018, 2022)
neipaths <- sprintf("~/projects/beethoven/input/nei/nei_onroad_byregions_%d/", neiyears)
neiyears <- split(neiyears, seq_along(neiyears))
neipaths <- split(neipaths, seq_along(neipaths))
cnty2 <- cnty
cnty2[[5]] <- cnty2[[4]]

nei_each <-
    mapply(
    function(x, y, z, u) {
        neix <- process_nei(path = x,
                    year = y,
                    county = vect(z))
        neix$time <- u
        return(neix)
        
    }, neipaths, neiyears, cnty2, nom_years, SIMPLIFY = FALSE)

nei_processed <- nei_each
for (i in seq_along(nei_each)) {
    nei_processed[[i]] <- calc_nei(sitesv, from = nei_each[[i]])
}
nei_all <- Reduce(rbind, nei_processed)
nei_all <- as.data.frame(nei_all)
saveRDS(nei_all[, -2:-4], "../../../../group/set/Projects/NRT-AP-Model/output/NRTAP_Covars_NEI.rds", compress = "xz")


saveRDS(nei_all, "~/projects/beethoven/output/nei_all.rds", compress = "xz")

## groads
groads <- process_sedac_groads(
    path = "/ddn/gs1/group/set/Projects/NRT-AP-Model/input/sedac_groads/gROADS-v1-americas.gdb"
)

sites_all <-
    process_aqs(
      path = list.files("/ddn/gs1/group/set/Projects/NRT-AP-Model/input/aqs", pattern = "csv", full.names = TRUE)[2],
      date = NULL
    )

library(tigris)
options(tigris_use_cache = TRUE)
usn <- tigris::states(year = 2020)
usn <- usn[!usn$STUSPS %in% c("AK", "VI", "PR", "HI", "GU", "MP", "AS"), ]
usn <- terra::vect(usn)
usn <- terra::project(usn, "EPSG:4326")

sites_us <- sites_all[usn, ]
## til 2021

## as-is
sites <- read.csv("../beethoven/input/unique_sites.csv")
sitesv <- terra::vect(sites, keepgeom = TRUE, crs = "EPSG:4326")

sites

## groads comp
sites_groads_1k <-
    calc_sedac_groads(
        locs = sites,
        from = groads,
        radius = 1000,
        locs_id = "site_id"
    )
sites_groads_10k <-
    calc_sedac_groads(
        locs = sites,
        from = groads,
        radius = 10000,
        locs_id = "site_id"
    )
sites_groads_50k <-
    calc_sedac_groads(
        locs = sites,
        from = groads,
        radius = 50000,
        locs_id = "site_id"
    )

# join sites_groads_*
sites_groads <-
    sites_groads_1k %>%
    dplyr::full_join(sites_groads_10k, by = "site_id") %>%
    dplyr::full_join(sites_groads_50k, by = "site_id") %>%
    dplyr::mutate(dplyr::across(starts_with("GRD"), ~ ifelse(is.na(.x), 0, .x)))
saveRDS(sites_groads, "../../../../group/set/Projects/NRT-AP-Model/output/NRTAP_Covars_SEDAC_Groads.rds", compress = "xz")

# nei spatial join with sites
# 1. using each year's county
nei_each 



## download again
download_aqs_data(
    parameter_code = 88101,
    year_start = 2018,
    year_end = 2023,
    directory_to_download = "/ddn/gs1/group/set/Projects/NRT-AP-Model/input/aqs",
    directory_to_save = "/ddn/gs1/group/set/Projects/NRT-AP-Model/input/aqs",
    data_download_acknowledgement = TRUE,
    remove_zip = TRUE,
    download = TRUE,
    remove_command = TRUE
)
