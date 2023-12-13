# to look at the path and package settings,
# consult "load_packages.r"
# source("./input/Rinput/processing_functions/load_packages.R")


#' Calculate Koeppen-Geiger climate zone binary variables
#' @note the same function is run with an alias
#' \code{calc_koeppen_geiger}.
#' @param path_koppen character(1). Path to Koppen-Geiger
#'  climate zone raster file
#' @param sites sf/SpatVector. Unique sites. Should include
#'  a unique identifier field named \code{id_col}
#' @param id_col character(1). Name of unique identifier.
#' @returns a data.frame object
#' @author Insang Song
#' @import terra
#' @importFrom methods is
#' @export
calc_koppen_geiger <-
  function(
      path_koppen = "./input/koppen_geiger/raw/Beck_KG_V1_present_0p0083.tif",
      sites,
      id_col = "site_id") {
    ## You will get "sites" in memory after sourcing the file above
    kg_rast <- terra::rast(path_koppen)
    sites_tr <- sites

    if (!methods::is(sites, "SpatVector")) {
      sites_tr <- terra::vect(sites)
    }
    sites_kg <- terra::project(sites_tr, terra::crs(kg_rast))
    sites_kg_extract <- terra::extract(kg_rast, sites_kg)

    # The starting value is NA as the color table has 0 value in it
    kg_class <-
      c(
        NA, "Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", "Csa", "Csb",
        "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc",
        "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
        "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF"
      )
    kg_coltab <- terra::coltab(kg_rast)
    kg_coltab <- kg_coltab[[1]][seq(1, 31), ]
    kg_colclass <- data.frame(
      value = kg_coltab$value,
      class_kg = kg_class
    )

    sites_kg_extract[[id_col]] <- unlist(sites_kg[[id_col]])
    colnames(sites_kg_extract)[2] <- "value"
    sites_kg_extract_e <- merge(sites_kg_extract, kg_colclass, by = "value")

    # "Dfa": 25
    # "BSh": 6
    # "Dfb": 26
    id_search <- unlist(sites_kg_extract_e[[id_col]])
    # errorfix: how to generalize and auto-fix it?
    sites_kg_extract_e[
      which(id_search == "44009000788101"),
      "class_kg"
    ] <- "Dfa"
    sites_kg_extract_e[
      which(id_search == "48061200488101"),
      "class_kg"
    ] <- "BSh"
    sites_kg_extract_e[
      which(id_search == "33015001488101"),
      "class_kg"
    ] <- "Dfb"

    sites_kg_extract_e$class_kg <-
      as.factor(substr(sites_kg_extract_e$class_kg, 1, 1))
    # currently there are no "E" region in sites.
    # however, E is filled with all zeros at the moment.
    aelabels <- LETTERS[1:5]
    df_ae_separated <-
      split(aelabels, aelabels) |>
      lapply(function(x) {
        as.integer(sites_kg_extract_e$class_kg == x)
      }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_ae_separated) <- sprintf("DUM_CLRG%s_0_00000", aelabels)

    kg_extracted <-
      cbind(
        site_id = unlist(sites_kg_extract_e[[id_col]]),
        df_ae_separated
      )
    return(kg_extracted)
  }


calc_koeppen_geiger <- calc_koppen_geiger
# saveRDS(kg_extracted, file = "~/NRTAP_Covars_Koppen_Geiger_AE_binary.rds")
