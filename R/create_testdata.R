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

#' Creates the testdata directory if it does not already exists. 
#' @author Eva Marques
#' @param dir_path a character path to the directory. 
create_dir <- function(dir_path) {
  if (!file.exists(dir_path)) {
    dir.create(dir_path)
  }
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
  create_dir(testdata_path)
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
  create_dir(testdata_path)
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


#' Create raw testdata files for Koppen-Geiger climate zone binary variables
#' @author Eva Marques
#' @param dict_path character path to the .csv file storing all data paths
#' @param testdata_path character path to the folder where testdata should
#' be stored
testdata_kg <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    testdata_path = "../tests/testdata/raw/koppen_geiger/") {
  name <- folder <- filename <- NULL
  create_dir(testdata_path)
  dict <- data.table::fread(dict_path, sep = ",")
  fpath <- paste0(
    dict[name == "koppen_geiger", folder],
    dict[name == "koppen_geiger", filename]
  )
  r <- terra::rast(fpath)
  r_samp <- sp_subset(r)
  testdata_file <- paste0(
    testdata_path,
    dict[name == "koppen_geiger", filename]
  )
  terra::writeRaster(r_samp, testdata_file, overwrite = TRUE)
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
  create_dir(testdata_path)
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
  create_dir(testdata_path)
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
  create_dir(testdata_path)
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

#' Create raw testdata files for TRI
#' @author Eva Marques
#' @param dict_path character path to the .csv file storing all data paths
#' @param year numeric giving the year
#' @param testdata_path character path to the folder where testdata should
#' be stored
testdata_tri <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    year = 2022,
    testdata_path = "../tests/testdata/raw/tri/") {
  name <- folder <- filename <- NULL
  create_dir(testdata_path)
  dict <- data.table::fread(dict_path, sep = ",")
  tri_path <- paste0(
      dict[name == "tri", folder],
      dict[name == "tri", filename]
    ) |>
      replace_dateinfo(date = as.Date(paste0(year, "-01-01")))
  tri_samp <- data.table::fread(tri_path) |>
    subset(`1. YEAR` == year & `7. COUNTY` %in% c("DURHAM", "WAKE", "ORANGE"))
  new_fpath <- paste0(
    testdata_path,
    dict[name == "tri", filename]
  ) |>
    replace_dateinfo(date = as.Date(paste0(year, "-01-01")))
  data.table::fwrite(
    tri_samp,
    new_fpath
  )
}


#' Create raw testdata files for AQS 
#' (start_date and end_date are supposed to be from the same year)
#' @author Eva Marques
#' @param dict_path character path to the .csv file storing all data paths
#' @param testdata_path character path to the folder where testdata should
#' be stored
testdata_aqs <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    start_date = as.Date("2022-01-01"),
    end_date = as.Date("2022-01-02"),
    testdata_path = "../tests/testdata/raw/aqs/") {
  name <- folder <- filename <- NULL
  create_dir(testdata_path)
  dict <- data.table::fread(dict_path, sep = ",")
  fpath <- paste0(
      dict[name == "aqs", folder],
      dict[name == "aqs", filename]
    ) |>
      replace_dateinfo(date = start_date)
  aqs <- data.table::fread(fpath) |>
    subset(`Date Local` >= start_date & `Date Local` <= end_date)
  aqs_vect <- terra::vect(aqs,
                          geom = c("Longitude", "Latitude"),
                          crs = "EPSG:4326"
  )
  aqs_samp <- sp_subset(aqs_vect) |>
    data.table::as.data.table()
  new_fpath <- paste0(
    testdata_path,
    dict[name == "aqs", filename]
    ) |>
      replace_dateinfo(start_date)
    data.table::fwrite(
      aqs_samp,
      new_fpath
    )
  }
}


#' Create raw testdata files for NEI
#' @author Eva Marques
#' @param dict_path character path to the .csv file storing all data paths
#' @param testdata_path character path to the folder where testdata should
#' be stored
testdata_nei <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    testdata_path = "../tests/testdata/raw/NEI/") {
  name <- folder <- filename <- NULL
  create_dir(testdata_path)
  dict <- data.table::fread(dict_path, sep = ",")
  onroads <- c("4", "5", "67", "123", "8910")
  for (onr in onroads) {
    nei_path <- paste0(
      dict[name == paste0("nei_onroad_", onr), folder],
      dict[name == paste0("nei_onroad_", onr), filename]
    ) |>
      replace_dateinfo(date = "2020-01-01")
    nei_samp <- data.table::fread(nei_path) |>
      subset(county %in% c("Durham", "Wake", "Orange"))
    
    new_fpath <- paste0(
      testdata_path,
      dict[name == paste0("nei_onroad_", onr), filename]
    ) |>
      replace_dateinfo("2020-01-01")
    data.table::fwrite(
      nei_samp,
      new_fpath
    )
  }
}



#' Create raw testdata files for HMS smoke shapefiles 
#' Important note: the extracted dates are not the same than the rest of 
#' testdata in favor of more interesting smoke polygons above
#' Wake-Durham-Orange area. 
#' @author Eva Marques
#' @param dict_path character path to the .csv file storing all data paths
#' @param testdata_path character path to the folder where testdata should
#' be stored
testdata_hms_smoke <- function(
    dict_path = "../inst/extdata/downloaded_files_metadata.csv",
    testdata_path = "../tests/testdata/raw/hms_smoke/") {
  name <- folder <- filename <- NULL
  create_dir(testdata_path)
  dict <- data.table::fread(dict_path, sep = ",")
  period <- c("2022-06-18", "2022-06-21")
  for (p in period) {
    fpath <- paste0(
      dict[name == "hms_smoke", folder],
      dict[name == "hms_smoke", filename]
    ) |>
      replace_dateinfo(date = as.Date(p))
    r <- terra::vect(fpath)
    r_samp <- sp_subset(r)
    new_fpath <- paste0(
      testdata_path,
      dict[name == "hms_smoke", filename]
    ) |>
      replace_dateinfo(as.Date(p))
    terra::writeVector(r_samp, new_fpath, overwrite = TRUE)
  }
}


  
