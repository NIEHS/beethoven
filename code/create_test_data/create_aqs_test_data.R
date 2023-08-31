#' create_aqs_test_data
#' @author Kyle P Messier
#' @return NULL - saves csv to folder
#' @export 
#'
#' @examples
create_aqs_test_data <- function(){
  print("loading full dataset")
  # Read full AQS dataset from input folder and create dataframe
  aqs <- read.csv("input/aqs/daily_88101_2018-2022.csv")
  print("subsetting")
  # Random sample of 10 rows, set seed
  set.seed(10)
  idx <- runif(10,1,nrow(aqs)) |> round()
  # Establish the required rows
  aqs.col <- aqs[idx,c("Latitude","Longitude","Arithmetic.Mean","Method.Name",
                    "ID.Code")]
  print("writing to testdata folder")
  write.csv(aqs.col,"tests/testdata/aqs-test-data.csv")
}