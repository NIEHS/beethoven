################################################################################
# Date created: 2023-09-19
# Packages required: terra
################################################################################

################################################################################
#' process_noaa_hms_smoke_data: download daily wildfire smoke plume data from NOAA Hazard Mapping System Fire and Smoke Product
#' 
#' @param date_start character(1). length of 10. Start date of downloaded data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date of downloaded data. Format YYYY-MM-DD (ex. September 10, 2023 = "2023-09-10").
#' @param data_format character(1). "Shapefile" or "KML" file type. ####
#' @param separate_output logical(1). Export separate processed files for "light", "medium", "heavy" smoke density classification or one file with all classifications.
#' @param output_format character(1). "Shapefile" or "Geopackage" file type. ####
#' @param directory_with_downloaded_data character(1). Directory storing downloaded ".shp" or ".kml" files downloaded from NOAA Hazard Mapping System Fire and Smoke Product.
#' @param directory_to_save character(1). Directory to save processed files.
#' @param remove_downloaded logical(1). Remove downloaded data files in directory_with_downloaded_data.
#' @author Mitchell Manware.
#' @return NULL; Separate ### TERRA SPATVECTOR FILE TYPE ### for each smoke density classification
#'                        ##################################
#' @export
process_noaa_hms_smoke_data <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-02",
    data_format = "Shapefile",
    separate_output = FALSE,
    output_format = "Shapefile",
    directory_with_downloaded_data = "./input/noaa_hms/",
    directory_to_save = "./input/noaa_hms/",
    remove_downloaded = TRUE
) {
  chars_dir_download = nchar(directory_with_downloaded_data)
  chars_dir_save = nchar(directory_to_save)
  
  if (substr(directory_with_downloaded_data, chars_dir_download, chars_dir_download) != "/") {
    directory_with_downloaded_data = paste(directory_with_downloaded_data, "/", sep="")
  }
  if (substr(directory_to_save, chars_dir_save, chars_dir_save) != "/") {
    directory_to_save = paste(directory_to_save, "/", sep="")
  }
  
  
  #### 1. define date sequence in character format
  date_start_date_format = as.Date(date_start, format = "%Y-%m-%d")
  date_end_date_format = as.Date(date_end, format = "%Y-%m-%d")
  date_sequence = seq(date_start_date_format, date_end_date_format, "day")
  date_sequence = gsub("-", "", as.character(date_sequence))
  
  
  #### 2. define list of data file names
  if(data_format == "Shapefile"){
    hms_files = list.files(path = directory_with_downloaded_data,
                           pattern = "hms_smoke_Shapefile_",
                           full.names = TRUE)
    hms_files_shp = hms_files[grep(".shp", hms_files)]
  } else if(data_format == "KML"){
    hms_files = list.files(path = directory_with_downloaded_data,
                           pattern = "hms_smoke_KML",
                           full.names = TRUE)
  }
  

  #### 3. import and aggregate data
  if(data_format == "Shapefile"){
    for(h in 1:length(hms_files_shp)){
      if(h == 1){
        smoke_data = terra::vect(hms_files_shp[h])
        smoke_data = terra::aggregate(smoke_data, by = "Density", dissolve = TRUE)
        smoke_data$Date = paste0(date_sequence[h])
      } else if(h > 1){
        smoke_data_2 = terra::vect(hms_files_shp[h])
        smoke_data_2 = terra::aggregate(smoke_data_2, by = "Density", dissolve = TRUE)
        smoke_data_2$Date = paste0(date_sequence[h])
        smoke_data = rbind(smoke_data, smoke_data_2)
      }
    }
    smoke_data$Density = factor(smoke_data$Density, levels = c("Light", "Medium", "Heavy"))
  } else if(data_format == "KML"){
    print("Functionality for file type KML not complete.")
  }

  
  #### 4. export files based on separate_output selection 
  if(separate_output == FALSE){
    if(output_format == "Shapefile"){
      terra::writeVector(smoke_data, filename = paste0(directory_to_save, "hms_smoke_processed_", gsub("-", "", date_start), "_", gsub("-", "", date_end), ".shp"), "ESRI Shapefile", overwrite=TRUE)
    } else if(output_format == "Geopackage"){
      terra::writeVector(smoke_data, filename = paste0(directory_to_save, "hms_smoke_processed_", gsub("-", "", date_start), "_", gsub("-", "", date_end), ".gpkg"), "GPKG", overwrite=TRUE)
    }
  } else if(separate_output == TRUE){
    dens = unique(smoke_data$Density)
    if(output_format == "Shapefile"){
      for(d in 1:length(dens)){
        smoke_data_dens = subset(smoke_data, smoke_data$Density==dens[d])
        terra::writeVector(smoke_data_dens, filename = paste0(directory_to_save, "hms_smoke_processed_", dens[d], "_", gsub("-", "", date_start), "_", gsub("-", "", date_end), ".shp"), "ESRI Shapefile", overwrite=TRUE)
      }
    } else if(output_format == "Geopackage"){
      for(d in 1:length(dens)){
        smoke_data_dens = subset(smoke_data, smoke_data$Density==dens[d])
        terra::writeVector(smoke_data_dens, filename = paste0(directory_to_save, "hms_smoke_processed_", dens[d], "_", gsub("-", "", date_start), "_", gsub("-", "", date_end), ".gpkg"), "GPKG", overwrite=TRUE)
      }
    }
  }
  
  
  #### 5. remove raw data.
  if(remove_downloaded == TRUE){
    for(z in 1:length(hms_files)){
      file.remove(hms_files[z])
    }
  }
}
