#' Calculate EPA Ecoregions level 2/3 binary variables
#' @param path character(1). Path to Ecoregion Shapefiles
#' @param sites sf/SpatVector. Unique sites. Should include
#'  a unique identifier field named \code{id_col}
#' @param id_col character(1). Name of unique identifier.
#' @returns a data.frame object
#' @author Insang Song
#' @importFrom methods is
#' @import terra
#' @export
calc_ecoregion <-
  function(
    path = "./input/data/ecoregions/raw/us_eco_l3_state_boundaries.shp",
    sites,
    id_col = "site_id"
  ) {

    if (!methods::is(sites, "SpatVector")) {
      sites <- terra::vect(sites)
    }
    ecoreg <- terra::vect(path)
    ecoreg <- ecoreg[, grepl("^(L2_KEY|L3_KEY)", names(ecoreg))]

    sites <- terra::project(sites, terra::crs(ecoreg))

    sites_in <- terra::intersect(sites, ecoreg)
    sites_out <-
      sites[!unlist(sites[[id_col]]) %in% unlist(sites_in[[id_col]]), ]

    sites_snapped <- terra::snap(sites_out, ecoreg, tolerance = 50)
    sites_fixed <- rbind(sites_in, sites_snapped)
    extracted <- terra::extract(ecoreg, sites_fixed)

    # Generate field names from extracted ecoregion keys
    # TODO: if we keep all-zero fields, the initial reference
    # should be the ecoregion polygon, not the extracted data
    key2_sorted <- unlist(extracted[, 3])
    key2_num <-
      regmatches(key2_sorted, regexpr("\\d{1,2}\\.[1-9]", key2_sorted))
    key2_num <- as.integer(10 * as.numeric(key2_num))
    key2_num <- sprintf("DUM_ECOREG_2_%03d", key2_num)
    key2_num_unique <- sort(unique(key2_num))

    key3_sorted <- unlist(extracted[, 2])
    key3_num <-
      regmatches(key3_sorted, regexpr("\\d{1,3}", key3_sorted))
    key3_num <- as.integer(as.numeric(key3_num))
    key3_num <- sprintf("DUM_ECOREG_3_%03d", key3_num)
    key3_num_unique <- sort(unique(key3_num))


    df_lv2 <-
      split(key2_num_unique, key2_num_unique) |>
      lapply(function(x) { as.integer(key2_num == x) }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_lv2) <- key2_num_unique
    df_lv3 <-
      split(key3_num_unique, key3_num_unique) |>
      lapply(function(x) { as.integer(key3_num == x) }) |>
      Reduce(f = cbind, x = _) |>
      as.data.frame()
    colnames(df_lv3) <- key3_num_unique
    
    sites_ecoreg <- cbind(sites[[id_col]], df_lv2, df_lv3)

    return(sites_ecoreg)
  }

# er3 <- calc_ecoregion(sites = vect(sites))

