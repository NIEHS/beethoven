#' Spatial subset on the testdata area:
#' Durham, Wake, and Orange counties in North Carolina, US
#' @author Eva Marques
#' @param sp SpatRaster or SpatVector from terra package
#' @return an object similar to sp but cropped on the testdata area
sp_subset <- function(sp) {
  nc_file <- system.file("shape/nc.shp", package = "sf")
  nc <- terra::vect(nc_file)
  ext <- nc[nc$NAME %in% c("Durham", "Wake", "Orange")]
  ext_proj <- terra::project(ext, terra::crs(sp))
  if (class(sp)[[1]] == "SpatRaster") {
    crop_sp <- terra::crop(sp, ext_proj, mask = TRUE, snap = "out")
  } else {
    crop_sp <- terra::crop(sp, ext_proj)
  }
  return(crop_sp)
}

#' The data paths are stored in a .csv file but they sometimes contain encoded
#' dates with characters 'yday', yyyy', 'mm', 'dd'. This function replace these
#' character strings by the good value contained in date. 
#' @author Eva Marques
#' @param string a character (eg: filepath)
#' @param date a Date 
#' @return the same string with encoded characters replaced by date info. 
replace_dateinfo <- function(string, date) {
  output <- gsub("yyyy", lubridate::year(date), string) |>
    gsub(
      pattern = "yday",
      replacement = stringr::str_pad(lubridate::yday(date),
        3,
        pad = "0"
      )
    ) |>
    gsub(
      pattern = "mm",
      replacement = stringr::str_pad(lubridate::month(date),
        2,
        pad = "0"
      )
    ) |>
    gsub(
      pattern = "dd",
      replacement = stringr::str_pad(lubridate::day(date),
        2,
        pad = "0"
      )
    )
  return(output)
}

#' Create raw testdata files for National Land Cover Dataset
#' (for both years 2019, 2021)
#' @author Eva Marques
#' @param dict_path character path to the .csv file storing all data paths
#' @param testdata_path character path to the folder where testdata should
#' be stored
testdata_nlcd <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    testdata_path = "../tests/testdata/raw/nlcd/") {
  name <- folder <- filename <- NULL
  dict <- data.table::fread(dict_path, sep = ",")
  fpath <- paste0(
    dict[name == "nlcd", folder],
    dict[name == "nlcd", filename]
  )
  r <- terra::rast(fpath)
  r_samp <- sp_subset(r)
  testdata_file <- paste0(
    testdata_path,
    dict[name == "nlcd", filename]
  )
  terra::writeRaster(r_samp, testdata_file, overwrite = TRUE)
}

#' Create raw testdata files for ecoregions
#' @author Eva Marques
#' @param dict_path character path to the .csv file storing all data paths
#' @param testdata_path character path to the folder where testdata should
#' be stored
testdata_ecoregions <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    testdata_path = "../tests/testdata/raw/ecoregions/") {
  name <- folder <- filename <- NULL
  dict <- data.table::fread(dict_path, sep = ",")
  fpath <- paste0(
    dict[name == "ecoregions", folder],
    dict[name == "ecoregions", filename]
  )
  r <- terra::vect(fpath)
  r_samp <- sp_subset(r)
  testdata_file <- paste0(
    testdata_path,
    dict[name == "ecoregions", filename]
  )
  terra::writeVector(r_samp, testdata_file, overwrite = TRUE)
}

#' Create raw testdata file for NARR variables stored as monthly rasters 
#' @author Eva Marques
#' @param dict_path character path to the .csv file storing all data paths
#' @param start_date Date of when testdata starts
#' @param end_date Date of when testdata ends
#' @param testdata_path character path to the folder where testdata should
#' be stored
#' @param var character of the variable name
testdata_narr <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    start_date = as.Date("2022-01-01"),
    end_date = as.Date("2022-01-02"),
    testdata_path = "../tests/testdata/raw/narr/",
    var) {
  name <- folder <- filename <- NULL
  dict <- data.table::fread(dict_path, sep = ",")
  # files are stored per year
  # we consider that start_date and end_date have the same year
  fpath <- paste0(
    dict[name == var, folder],
    dict[name == var, filename]
  ) |>
    replace_dateinfo(start_date)
  period <- seq(lubridate::yday(start_date), lubridate::yday(end_date), 1)
  r_var <- terra::rast(fpath)[[period]]
  r_var_samp <- sp_subset(r_var)
  # same here
  new_fpath <- paste0(
    testdata_path,
    dict[name == var, filename]
  ) |>
    replace_dateinfo(start_date)
  terra::writeCDF(r_var_samp,
    new_fpath,
    varname = names(r_var_samp),
    unit = terra::units(r_var_samp),
    split = TRUE,
    overwrite = TRUE
  )
  cat("✓ ", new_fpath, "\n")
}

#' Create raw testdata file for NARR variables stored as monthly rasters 
#' with pressure levels (omega and shum variables)
#' @author Eva Marques
#' @param dict_path character path to the .csv file storing all data paths
#' @param start_date Date of when testdata starts
#' @param end_date Date of when testdata ends
#' @param testdata_path character path to the folder where testdata should
#' be stored
#' @param var character of the variable name ("omega" or "shum")
testdata_narr_lev <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    start_date = as.Date("2022-01-01"),
    end_date = as.Date("2022-01-02"),
    testdata_path = "../tests/testdata/raw/narr/",
    var) {
  name <- folder <- filename <- NULL
  dict <- data.table::fread(dict_path, sep = ",")
  # files are stored per month
  # we consider that start_date and end_date have the same month
  fpath <- paste0(
    dict[name == var, folder],
    dict[name == var, filename]
  ) |>
    replace_dateinfo(start_date)
  period <- seq(start_date, end_date, 1)
  r <- terra::rast(fpath)
  r_var <- r[[terra::time(r) %in% period]]
  r_var_samp <- sp_subset(r_var)
  new_fpath <- paste0(
    testdata_path,
    dict[name == var, filename]
  ) |>
    replace_dateinfo(start_date)
  terra::writeCDF(r_var_samp,
    new_fpath,
    varname = names(r_var_samp),
    unit = terra::units(r_var_samp),
    split = TRUE,
    overwrite = TRUE
  )
  cat("✓ ", new_fpath, "\n")
}

#' Create raw testdata file for GEOS-CF variables 
#' Note1: raw downloaded files have .nc4 extension, which leads to a warning 
#' when testdata .nc4 files are created.
#' Note2: an error can occur when creating ncfile. Relaunching the code solves
#' the problem for now...  
#' @author Eva Marques, Mitchell Manware, Insang Song
#' @param dict_path character path to the .csv file storing all data paths
#' @param start_date Date of when testdata starts
#' @param end_date Date of when testdata ends
#' @param testdata_path character path to the folder where testdata should
#' be stored
#' @param collection character of the data collection ("aqc" or "chm")
testdata_geos <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    start_date = as.Date("2022-01-01"),
    end_date = as.Date("2022-01-02"),
    testdata_path,
    collection) {
  name <- folder <- filename <- NULL
  period <- seq(from = start_date, to = end_date, by = "1 day")
  dict <- data.table::fread(dict_path, sep = ",")
  for (p in period) {
    p <- as.Date(p)
    for (h in seq(0, 23, by = 1)) {
      new_fpath <- paste0(
        testdata_path,
        dict[name == collection, filename]
      ) |>
        replace_dateinfo(p) |>
        gsub(pattern = "HH", replacement = stringr::str_pad(h, 2, pad = "0"))
      if (!file.exists(new_fpath)) {
        fpath <- paste0(
          dict[name == collection, folder],
          dict[name == collection, filename]
        ) |>
          replace_dateinfo(p) |>
          gsub(pattern = "HH", replacement = stringr::str_pad(h, 2, pad = "0"))
        r <- terra::rast(fpath)
        terra::crs(r) <- "EPSG:4326"
        r_samp <- sp_subset(r)
        terra::writeCDF(r_samp,
          new_fpath,
          varname = names(r_samp),
          split = TRUE,
          unit = terra::units(r_samp),
          overwrite = TRUE
        )
        cat("\n✓ ", new_fpath)
      }
    }
  }
}
