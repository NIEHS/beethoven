# to look at the path and package settings,
# consult "load_packages.r"
source("./input/Rinput/processing_functions/load_packages.R")


## You will get "sites" in memory after sourcing the file above
kg_file <- terra::rast("./input/koppen_geiger/raw/Beck_KG_V1_present_0p0083.tif")


sites_tr <- terra::vect(sites)
sites_kg <- terra::project(sites_tr, terra::crs(kg_file))
sites_kg_extract <- terra::extract(kg_file, sites_kg)


kg_class <-
c(NA, "Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", "Csa", "Csb",
  "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc",
  "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
  "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF")
kg_coltab <- terra::coltab(kg_file)
kg_coltab <- kg_coltab[[1]][seq(1, 31), ]
kg_colclass <- data.frame(
  value = kg_coltab$value,
  class_kg = kg_class
)

sites_kg_extract$site_id <- sites_kg$site_id
colnames(sites_kg_extract)[2] <- "value"
sites_kg_extract_e <- merge(sites_kg_extract, kg_colclass, by = "value")

# errorfix
sites_kg_extract_e[which(sites_kg_extract_e$site_id == "44009000788101"), "class_kg"] <- "Dfa"# 25
sites_kg_extract_e[which(sites_kg_extract_e$site_id == "48061200488101"), "class_kg"] <- "BSh"# 6
sites_kg_extract_e[which(sites_kg_extract_e$site_id == "33015001488101"), "class_kg"] <- "Dfb"# 26

sites_kg_extract_e$class_kg <- as.factor(substr(sites_kg_extract_e$class_kg, 1, 1))
getform <- reformulate(
  response = "site_id",
  termlabels = "class_kg"
)

aelabels <- LETTERS[1:5]
df_ae_separated <- 
  split(aelabels, aelabels) |>
  lapply(function(x) { as.integer(sites_kg_extract_e$class_kg == x) }) |>
  Reduce(f = cbind, x = _) |>
  as.data.frame()

kg_extracted <-
  cbind(
    site_id = sites_kg_extract_e$site_id,
    df_ae_separated
  )
saveRDS(kg_extracted, file = "~/NRTAP_Covars_Koppen_Geiger_AE_binary.rds")
