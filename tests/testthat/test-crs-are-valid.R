#' @author Insang Song
#' @description
#' unit testing for the output has a valid CRS
#' list of valid CRS: EPSG:5070, EPSG:4326
#' 
#'
test_that("Output CRS is valid", {
  library(stars)
  library(sf)

  # 1. read netcdf output file
  path_results = "./output/model_output.nc"
  model_results <- stars::read_stars(path_results)

  crs_list_acceptable = c("EPSG:4326", "EPSG:5070")

  # 2. main function
  check_crs_is_valid <- function(
    model_output = model_results,
    crs_list = crs_list_acceptable
  ) {
    crs_output = sf::st_crs(model_output)
    checked <- sapply(crs_list, function(x) identical(crs_output, sf::st_crs(x)))
    checked <- any(checked)
    return(checked)
  }

  iscrsvalid = check_crs_is_valid()

  expect_equal(iscrsvalid, TRUE)
})
