# Download EPA AQS data and filter to a single POC per site#
# Author: Mariana Alifa Kassien
# September 2023

# Source functions
source("R/download_aqs_data.R")
source("R/filter_minimum_poc.R")

# Define parameters of interest for download
parameter_code = 88101
year_start = 2018
year_end = 2022
resolution_temporal = "daily"
directory_to_download = "./input/aqs/"
directory_to_save = "./input/aqs/"
url_aqs_download = "https://aqs.epa.gov/aqsweb/airdata/"

# Run data download function
download_aqs_data(parameter_code, year_start,year_end,resolution_temporal,
                  directory_to_download, directory_to_save,
                  url_aqs_download, remove_zips = FALSE)

# Read in sites dataframe for filtering
input_df= read.csv(paste(directory_to_save, resolution_temporal, "_", parameter_code, "_", year_start, "-", year_end, ".csv", sep=""), 
                   stringsAsFactors = F)

# Optional: view column names to determine right inputs for filtering
#colnames(input_df)

# Create a site ID column that does not include the POC number
input_df = input_df |>
  mutate(ID.Site = substr(ID.Monitor, 1, 17))

site_id="ID.Site"
poc_name="POC"

# Filter sites with multiple POCs (select lowest-numbered POC per site)
data_filtered =filter_minimum_poc(input_df, site_id, poc_name)

# Save new filtered data - wasn't sure what to name it so I added "filtered_" to the beginning. We can change that too if there's a better idea.
output_name=paste(directory_to_save, "filtered_",resolution_temporal, "_", parameter_code, "_", year_start, "-", year_end, ".csv", sep="") 
  # output_name
  # [1] "./input/aqs/filtered_daily_88101_2018-2022.csv"
write.csv(data_filtered, output_name)
