# vnp46a2

source("./input/Rinput/processing_functions/load_packages.R")


files_vnp46 <-
    list.files(path = "./input/data/modis/raw/5000",
               pattern = "*.h5$",
               full.names = TRUE,
               recursive = TRUE)
# reference files
# files_mod09 <-
#     list.files(path = "./input/data/modis/raw/61/MOD09GA/2018/001",
#                pattern = "*.hdf$",
#                full.names = TRUE,
#                recursive = TRUE)

# tile_extracted <-
#     regmatches(files_mod13, regexpr("h[0-9]+{2,2}v[0-9]+{2,2}", files_mod13))
modis_crs <-
    "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"


# as.numeric(6371007.181 * pi * 2.00 / 36.00)

# mod13_ranges <- lapply(
#     files_mod13,
#     function(x) {
#         matrix(as.vector(terra::ext(terra::rast(x))), nrow = 1)
#     }
# )
# names(mod13_ranges) <- tile_extracted
# mod13_ranges[[2]]
# mod13_ranges <- Reduce(rbind, mod13_ranges)
# colnames(mod13_ranges) <- c("xmin", "xmax", "ymin", "ymax")

# tiledf <- data.frame(tile = tile_extracted)
# tiledf <- cbind(tiledf, mod13_ranges)
# diff_x <- sort(unique(diff(tiledf$xmin)))[2]
# diff_y <- sort(unique(diff(tiledf$ymin)))[2]


# manually define tile grid UL-LR coordinates
# OBSOLETE
# tile_def <-
#     expand.grid(
#         vaddr = sprintf("v%02d", 3:6),
#         haddr = sprintf("h%02d", 6:13)
#     )
# tile_def$tile <- paste0(tile_def$haddr, tile_def$vaddr)
# tile_def <- data.frame(tile = tile_def$tile)
# tile_def$xmin <- rep(seq(-12231456 - 1111950.5, -5559751, 1111950.5), each = 4)
# tile_def$xmax <- tile_def$xmin + 1111950.5
# tile_def$ymin <- rep(seq(5559753, 2223901, -1111950.5), 8)
# tile_def$ymax <- tile_def$ymin + 1111950.5
# tile_def


## Check if modis_grid in degrees are corresponding with converted sinu
# modis_ref <- read.csv("~/modis_tiles.csv")
# modis_ref <- modis_ref[which(modis_ref$lon_min >= -180.00), ]
# modis_ref

# modis_crs_deg <-
#     "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=deg +no_defs"
# test <- ext(unname(unlist(modis_ref[1, 3:6])))
# testv <- vect(test, crs = modis_crs_deg)

# VNP46 tile coverage is significantly larger than
# MOD13 tile coverage.

## manually assign VIIRS linear grid
wgs_ext <- c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)
wgs_ext <- ext(wgs_ext)
wgs_ext <- vect(wgs_ext)
crs(wgs_ext) <- "OGC:WGS84"
rr <- crs("EPSG:6933")
# wgs_extg <- rast(wgs_ext, nrows = 18, ncols = 36)

viirs_ext <- c(-17367530, 17367530, -7324185, 7324185)
viirs_ext <- c(-1.8e7, 1.8e7, -9e6, 9e6)
viirs_ext <- ext(viirs_ext)
viirs_ext <- vect(viirs_ext)
crs(viirs_ext) <- crs("EPSG:6933")
# viirs_ext <- project(wgs_ext, rr)
viirs_ext <- rast(viirs_ext, nrows = 18, ncols = 36)
viirs_poly <- as.polygons(viirs_ext)
data(world, package = "spData")
world_viirs <- project(vect(world), rr)


plot(viirs_poly)
plot(world_viirs, add = T, border = "red")

## Does not match the plot in User Manual...
## Reprojection strategy does not work
## TODO: access h5 directly

library(rhdf5)
# h5o <- rhdf5::H5Fopen(files_vnp46[1])
h5o <- rhdf5::h5read(files_vnp46[1], "HDFEOS INFORMATION")
h5a <- rhdf5::h5read(files_vnp46[1], "HDFEOS")
cat(h5o[[1]])
## OKAY!
# h06v03 (7, 4)
UL <- c(-1.2e8, 6e7)
LR <- c(-1.1e8, 5e7)
h5closeAll()
grandUL <- c(-1.8e8, 9e7)
viirs_ext <- expand.grid(
    xmin = seq(-1.8e8, 1e7, 1.7e8),
    ymin = seq(-9e7, 1e7, 8e7)
)





#' @description This function will return a SpatRaster object with
#' georeferenced h5 files of VNP46A2 product.
#' @param tile_df prespecified data.frame with "tile" and "exts" columns,
#' where the former stores tile number (h00v00 format) and the latter
#' stores terra::ext output.
assign_ext_vnp46 <- function(
    filepaths,
    date,
    tile_df = tile_def,
    crs_ref = modis_crs
) {
    if (is.character(date)) {
        if (nchar(date) != 10) {
            stop("Check the date format.\n")
        }
        date <- as.Date(date)
    }
    datejul <- strftime(date, format = "%Y/%j")
    stdtile <- tile_df$tile

    filepaths_today <- grep(as.character(datejul), filepaths, value = TRUE)
    # today's filenames
    # filepaths_tiles <-
    #     regmatches(filepaths_today,
    #                regexpr("h[0-9]+{2,2}v[0-9]+{2,2}", filepaths_today))
    filepaths_today <- grep(paste("(", paste(stdtile, collapse = "|"), ")"), filepaths_today, value = TRUE)
    print(filepaths_today)
    filepaths_today_tiles <-
        regmatches(filepaths_today,
                   regexpr("h[0-9]+{2,2}v[0-9]+{2,2}", filepaths_today))
    # filepaths_tiles <- filepaths_tiles[which(filepaths_tiles %in% stdtile)]
    # print(filepaths_tiles)
    # filepaths_tiles_list <- split(filepaths_tiles, filepaths_tiles)
    vnp46_today <- unname(split(filepaths_today, filepaths_today))
    filepaths_today_tiles_list <-
        unname(split(filepaths_today_tiles, filepaths_today_tiles))

    vnp_assigned <-
        mapply(function(vnp, tile_in) {
            vnp_ <- terra::rast(vnp, subds = 3)
            tile_ext <- tile_df[tile_df$tile == tile_in, -1]
            print(tile_ext)
            terra::crs(vnp_) <- terra::crs(crs_ref)
            terra::ext(vnp_) <- unlist(tile_ext)
            return(vnp_)
        }, vnp46_today, filepaths_today_tiles_list, SIMPLIFY = FALSE)
    vnp_all <- do.call(terra::merge, vnp_assigned)
    return(vnp_all)
}

vnp46_2018001 <- assign_ext_vnp46(files_vnp46, "2018-01-01")
vnp46_2018001[(vnp46_2018001 == 65535)] <- NA

plot(vnp46_2018001)
