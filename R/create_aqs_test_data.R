#' create_aqs_test_data
#' @author Kyle P Messier
#' @return NULL - saves csv to folder
#' @export 
#'
#' @examples
create_aqs_test_data <- function(){
  require(sf)
  require(sftime)
  require(dplyr)
  print("loading full dataset")
  # Read full AQS dataset from input folder and create dataframe
  aqs <- read.csv("input/aqs/daily_88101_2018-2022.csv")
  print("subsetting")
  
  # Random sample of 10 rows, set seed
  set.seed(10)
  idx <- runif(10,1,nrow(aqs)) |> round()
  
  # Subset the rows and columns
  aqs.col <- aqs[idx,c("Latitude","Longitude","Date.Local","Arithmetic.Mean","Method.Code",
                    "ID.Code")]
  # Write the data to a CSV
  print("writing to testdata folder - csv")
  write.csv(aqs.col,"tests/testdata/aqs-test-data.csv",row.names = FALSE)
  
  # Convert to an sf object
  print("converting and writing to testdata folder - GeoPackage (gpkg)")
  # Note coords is 2,1 for longitude,latitude
  aqs.sf <- st_as_sf(aqs.col,coords = c(2,1),crs = "EPSG:4269") |> 
    dplyr::mutate(time = as.POSIXct(Date.Local)) |>
    dplyr::select(-Date.Local)
  
  # Convert to an sftime object
  aqs.sftime <- st_as_sftime(aqs.sf)
  
  # write the data to an sftime object
  st_write(aqs.sftime,"tests/testdata/aqs-test-data.gpkg",append = FALSE)
  
  
}
