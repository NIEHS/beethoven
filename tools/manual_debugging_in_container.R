
chr_daterange <- c("2018-01-01", "2018-03-31") # Date range | critical
# chr_nasa_token <- readLines("/inst/extdata/nasa_token.txt") # NASA Earthdata token | critical
chr_mod06_links <- "/inst/extdata/mod06_links_2018_2022.csv" # File of MOD06 links | critical
chr_input_dir <- "/input" # Data directory | critical
num_dates_split <- 122 # Number of days in each temporal split | critical
chr_store <- "/opt/_targets/objects/" # Path to {targets} store

chr_dates <- amadeus::generate_date_sequence(
  chr_daterange[1],
  chr_daterange[2],
  sub_hyphen = FALSE
)
chr_years <- unique(lubridate::year(chr_dates))
list_dates <- beethoven::split_dates(
  dates = chr_daterange,
  n = num_dates_split,
  year = TRUE
)
list_dates_julian <- lapply(list_dates, function(x) format(as.Date(x), "%Y%j"))
list_dates_small <- beethoven::split_dates(
  dates = chr_daterange,
  n = 10,
  year = TRUE
)
chr_iter_radii <- c(1000, 10000, 50000)
arglist_common <- beethoven::set_args_calc(
  char_siteid = "site_id",
  char_timeid = "time",
  char_period = chr_daterange,
  num_extent = c(-126, -62, 22, 52),
  char_user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
  char_input_dir = chr_input_dir
)

sf_us_contig <- sf::st_read("inst/extdata/us_contiguous.gpkg")
chr_hex_res8_index <- polyfill(sf_us_contig, res = 8)
chr_hex_res3_index <- map_chr(chr_hex_res8_index, h3_to_parent, res = 3)
list_h3_res8_index <- split(chr_hex_res8_index, chr_hex_res3_index)
chr_hex_res2_index <- map_chr(chr_hex_res8_index, h3_to_parent, res = 2)
list_h3_res8_index2 <- split(chr_hex_res8_index, chr_hex_res2_index)

############################################################################
############################################################################
############################################################################

###########################         HMS          ###########################
list_pred_calc_hms <- {
  h3_locs <- h3_to_geo_sf(list_h3_res8_index[[1]])
  h3_locs$site_id <- h3_locs$h3_index
  beethoven::inject_calculate(
    covariate = "hms",
    locs = h3_locs,
    injection = list(
      path = file.path(chr_input_dir, "hms", "data_files"),
      date = beethoven::fl_dates(unlist(list_dates)),
      covariate = "hms"
    )
  )[[1]] |>
    dplyr::select(
      -dplyr::any_of(c("h3_index", "geometry", "hms_year"))
    )
}

##############################   NLCD    #############################
chr_iter_calc_nlcd <- c(2019, 2021)

list_pred_calc_nlcd <- {
  h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
  h3_locs$site_id <- h3_locs$h3_index

  unique_radii <- chr_iter_radii
  nlcd_years <- chr_iter_calc_nlcd
  # threefold lists
  # First level: years (rowbind)
  lapply(
    nlcd_years,
    function(yearj) {
      # Second level: radii (full join)
      lapply(
        unique_radii,
        function(radi) {
          inject_nlcd(
            year = yearj,
            radius = radi,
            from = amadeus::process_nlcd(
              path = file.path(chr_input_dir, "nlcd", "data_files"),
              year = yearj,
            ),
            locs = h3_locs, # full dataset (no chunking by rows)
            locs_id = arglist_common$char_siteid,
            mode = "exact",
            max_cells = 3e7
          )
        }
      ) %>%
        Reduce(
          f = function(d, e) {
            dplyr::full_join(d, e, by = c("site_id", "time"))
          }
        )
    }
  ) %>%
    collapse::rowbind(., fill = TRUE) %>%
    as.data.frame()
}

###########################         GEOS         ###########################
chr_iter_calc_geos <- c("aqc_tavg_1hr_g1440x721_v1", "chm_tavg_1hr_g1440x721_v1")

list_pred_calc_geos_aqc <- {
  h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
  h3_locs$site_id <- h3_locs$h3_index
  download_geos_buffer <- TRUE
  beethoven::calc_geos_strict(
    path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[1]),
    date = beethoven::fl_dates(unlist(list_dates)),
    locs = h3_locs,
    locs_id = "site_id"
  )
}

list_pred_calc_geos_chm <- {
  download_geos_buffer
  h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
  h3_locs$site_id <- h3_locs$h3_index
  beethoven::calc_geos_strict(
    path = file.path(chr_input_dir, "geos", chr_iter_calc_geos[2]),
    date = beethoven::fl_dates(unlist(list_dates)),
    locs = h3_locs,
    locs_id = "site_id"
  )
}


###########################         NARR         ###########################
chr_iter_calc_narr <- c(
  "air.sfc",
  "albedo",
  "apcp",
  "dswrf",
  "evap",
  "hcdc",
  "hpbl",
  "lcdc",
  "lhtfl",
  "mcdc",
  "pr_wtr",
  "prate",
  "pres.sfc",
  "shtfl",
  "snowc",
  "soilm",
  "tcdc",
  "ulwrf.sfc",
  "uwnd.10m",
  "vis",
  "vwnd.10m",
  "weasd",
  "omega",
  "shum"
)
chr_iter_calc_narr_lag <- c(
  "air.sfc",
  "apcp",
  "pres.sfc",
  "shum",
  "uwnd.10m",
  "vwnd.10m"
)


###
list_pred_calc_narr <- {
  h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
  h3_locs$site_id <- h3_locs$h3_index
  lapply(
    chr_iter_calc_narr,
    function(name) {
      dt_iter_calc_narr <- amadeus::calculate_narr(
        from = amadeus::process_narr(
          path = file.path(chr_input_dir, "narr", name),
          variable = name,
          date = beethoven::fl_dates(unlist(list_dates))
        ),
        locs = h3_locs,
        locs_id = "site_id",
        radius = 0,
        fun = "mean",
        geom = FALSE
      )
      if (length(grep("level", names(dt_iter_calc_narr))) == 1) {
        dt_iter_calc_narr <-
          dt_iter_calc_narr[dt_iter_calc_narr$level == 1000, ]
        dt_iter_calc_narr <-
          dt_iter_calc_narr[, -grep("level", names(dt_iter_calc_narr))]
      }
      dt_iter_calc_narr
    }
  ) %>%
    Reduce(
      f = function(d, e) {
        dplyr::full_join(d, e, by = c("site_id", "time"))
      },
      .
    ) %>%
    as.data.frame()
}

list_pred_calc_mod11 <- {
  search_dir <- file.path(chr_input_dir, "modis_preprocessed", "MOD11A1")
  date_find <- list_dates

  h3_locs <- h3_to_geo_sf(list_h3_res8_index2[[1]])
  h3_locs$site_id <- h3_locs$h3_index

  lapply(chr_iter_radii, function(r) {
    Map(
      f = function(date_i) {
        date_in <- paste("(", date_i, ")", sep = "")
        target_file <- list.files(
          path = search_dir,
          pattern = paste0(date_in, "\\.tif$"),
          full.names = TRUE
        )
        res <- tryCatch(
          {
            beethoven::calculate_modis_direct(
              file = target_file,
              site = h3_locs,
              site_id = arglist_common[["char_siteid"]],
              radius = r,
              colheader = c("MOD_SFCTD_0_", "MOD_SFCTN_0_"),
              mark = TRUE
            )
          },
          error = function(e) {
            res <- expand.grid(
              site_id = h3_locs[["site_id"]],
              time = date_i,
              MOD_SFCTD_0_ = NA_real_,
              MOD_SFCTN_0_ = NA_real_
            )
            names(res)[3:4] <- paste0(names(res)[3:4], sprintf("%05d", r))
            return(res)
          }
        )
        res
      },
      date_find
    ) %>%
      collapse::rowbind(., fill = TRUE)
  }) %>%
    Reduce(
      f = function(d, e) {
        dplyr::full_join(d, e, by = c("site_id", "time"))
      },
      .
    ) %>%
    as.data.frame()
}
