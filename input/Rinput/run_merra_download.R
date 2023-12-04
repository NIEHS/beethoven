

source("./input/Rinput/download_functions/download_merra2_data.R")
# testrun
download_merra2_data(
  date_start = "2018-01-01",
  date_end = "2018-01-31",
  collection = "inst3_3d_aer_Nv",
  directory_to_save = "./input/data/merra2/raw/",
  data_download_acknowledgement = TRUE
)


download_merra2_data(
  date_start = "2018-01-01",
  date_end = "2018-01-01",
  collection = "tavg1_2d_aer_Nx",
  directory_to_save = "./input/data/merra2/raw/",
  data_download_acknowledgement = TRUE
)
