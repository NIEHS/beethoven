#' @author Kyle P Messier
#' @description
#' unit testing that the dependent variable is read and converted to the expected sf/sftime class
#' 
#' 
#' 
#'
test_that("dependent variable object is an sf class", {
  aqs.sftime <- sf::st_read("../testdata/aqs-test-data.gpkg") |>
    st_as_sftime()
  expect_s3_class(aqs.sftime, "sf")
})
