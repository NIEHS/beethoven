## ----init-setting, echo = FALSE-----------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## ----load-terra---------------------------------------------------------------
library(terra)
library(tigris)

## ----load-usmain--------------------------------------------------------------
usmain <- tigris::states(progress_bar = FALSE)
exclude <- c("02", "15", "60", "66", "68", "69", "72", "78")
usmain <- usmain[!usmain$STATEFP %in% exclude, ]
usmain <- terra::vect(usmain)
usmain <- terra::aggregate(usmain)
usmain <- terra::project(usmain, "EPSG:5070")
plot(usmain)


## ----gen-grid-prep------------------------------------------------------------
corner_ul <- c(-2.40, 3.26) * 1e6
corner_lr <- c(2.40, 0.12) * 1e6

corners <- c(corner_ul, corner_lr)
# reorganize xmin, ymin, xmax, ymax, which are ll, ur form
corners_re <- corners[c(1, 3, 4, 2)]
names(corners_re) <- c("xmin", "xmax", "ymin", "ymax")
corners_ext <- terra::ext(corners_re)

## ----gen-grid-1km, eval = FALSE-----------------------------------------------
#  corners_ras <-
#    terra::rast(
#      corners_ext,
#      resolution = c(1000L, 1000L),
#      crs = "EPSG:5070"
#    )
#
#  terra::values(corners_ras) <- 1L
#  corners_ras_sub <-
#    terra::crop(
#      corners_ras,
#      usmain,
#      snap = "out",
#      mask = TRUE
#    )
#
#  corners_pnts <- terra::as.points(corners_ras_sub)
#  corners_pnts_df <- as.data.frame(corners_pnts, geom = "XY")
#  corners_pnts_df$site_id <- seq(1, nrow(corners_pnts_df))
#  names(corners_pnts_df)[2:3] <- c("lon", "lat")
#  corners_pnts_df <- corners_pnts_df[, c("site_id", "lon", "lat")]
#

## ----save-rds, eval = FALSE---------------------------------------------------
#  saveRDS(
#    corners_pnts_df,
#    file = "./input/prediction_grid.rds",
#    compress = "xz"
#  )
#

# nolint start
## ----gen-grid-10km, echo = FALSE, message = FALSE, error = FALSE, fig.width = 8, fig.height = 4.8----
# nolint end
corners_ras10 <-
  terra::rast(
    corners_ext,
    resolution = c(10000L, 10000L),
    crs = "EPSG:5070"
  )

terra::values(corners_ras10) <- 1L
corners_ras_sub10 <-
  terra::crop(
    corners_ras10,
    usmain,
    snap = "out",
    mask = TRUE
  )

corners_pnts10 <- terra::as.points(corners_ras_sub10)

## ----plot-grid-10km-----------------------------------------------------------
plot(
  corners_pnts10,
  cex = 0.1,
  main = "10-km grid points in the mainland US"
)
