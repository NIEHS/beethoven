
options(width = 140)
files_mod06 <-
    list.files(path = "./input/modis/raw/MOD06_L2",
               pattern = "*.hdf$",
               full.names = TRUE,
               recursive = TRUE)

v18001 <- grep("A2018001", files_mod06, value = T)



terra::describe(v18001[1], sds = TRUE)

# Cloud fraction night, day (37, 39, respectively)
indices <- c(37, 39)
terra::rast(v18001[1], subds = 37, lyr = 5) -> v1800137
v1800137

header <- "HDF4_EOS:EOS_SWATH:"
suffix <- ":mod06:"
parsing <- c("Cloud_Fraction_Day", "Cloud_Fraction_Night")
filename <- v18001[1:10]
parsinga <- sprintf("%s%s%s%s", header, filename, suffix, parsing[2])
parsinga

library(terra)
cf1 <- terra::rast(parsinga[1])
cf2 <- terra::rast(parsinga[2])
cf1r <- terra::rectify(cf1, method = "nearest", filename = "~/test.tif")

cf1r <- terra::rectify(parsing[1], method = "nearest")


cf12 <- terra::mosaic(cf1, cf2)

cf1
plot(cf1)

cf1@pnt@rotated <- FALSE
?terra::rotate
?is.rotated

cf1r <- rectify(cf1)

nt1 <- stars::read_stars(parsinga[1])
nt2 <- stars::read_stars(parsinga[2])
lightmosaicked <-
stars::st_mosaic(nt1, nt2)
stars::st_warp()

lightmosaicked
terra:::rectify
showMethods(rectify)


testfile <- "~/MYD06_L2.A2021005.0320.061.2021005180405.hdf"
cf1 <- terra::rast(testfile, subds = 48)
