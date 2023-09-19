################################################################################
# Date created: 2023-09-19
# Packages required: None
################################################################################

################################################################################
#' download_noaa_hms_smoke_data: download daily wildfire smoke plume data from NOAA Hazard Mapping System Fire and Smoke Product
#' 
#' @param date_start character(1). length of 10. Start date for downloading data. Format YYYY-MM-DD (ex. September 1, 2023 = "2023-09-01").
#' @param date_end character(1). length of 10. End date for donwloading data. Format YYYY-MM-DD (ex. September 10, 2023 = "2023-09-10").
#' @param data_format character(1). "Shapefile" or "KML" file type. ####
#' @param directory_to_download character(1). Directory to download zip files from NOAA Hazard Mapping System Fire and Smoke Product.
#' @param directory_to_save character(1). Directory to decompress zip files.
#' @param url_noaa_hms_smoke_data character(1). URL to the NOAA Hazard Mapping System Fire and Smoke Product data.
#' @param remove_download logical(1). Remove download files in directory_to_download.
#' @author Mitchell Manware.
#' @return NULL; Separate shapefile or KML files of wildfire smoke plume polygon data will be stored in directory_to_save.
#' @export
download_noaa_hms_smoke_data <- function(
    date_start = "2023-09-01",
    date_end = "2023-09-02",
    data_format = "Shapefile",
    directory_to_download = "./input/noaa_hms/",
    directory_to_save = "./input/noaa_hms/",
    url_noaa_hms_smoke_data = "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/",
    remove_download = TRUE
) {
   chars_dir_download = nchar(directory_to_download)
   chars_dir_save = nchar(directory_to_save)
   
   if (substr(directory_to_download, chars_dir_download, chars_dir_download) != "/") {
       directory_to_download = paste(directory_to_download, "/", sep="")
   }
   if (substr(directory_to_save, chars_dir_save, chars_dir_save) != "/") {
     directory_to_save = paste(directory_to_save, "/", sep="")
   }
   
   #### 1. define date sequence in character format
   date_start_date_format = as.Date(date_start, format = "%Y-%m-%d")
   date_end_date_format = as.Date(date_end, format = "%Y-%m-%d")
   date_sequence = seq(date_start_date_format, date_end_date_format, "day")
   date_sequence = gsub("-", "", as.character(date_sequence))
   
   #### 2. define data download URLs and download names
   for(f in 1:length(date_sequence)){
     
     year = substr(date_sequence[f], 1, 4)
     month = substr(date_sequence[f], 5, 6)
     
     if (data_format == "Shapefile"){
       file_urls = paste0(url_noaa_hms_smoke_data, data_format, "/", year, "/", month, "/hms_smoke", date_sequence, ".zip")
       download_names = paste0(directory_to_download, "hms_smoke_", data_format, "_", date_sequence, ".zip")
     } else if (data_format == "KML"){
       file_urls = paste0(url_noaa_hms_smoke_data, data_format, "/", year, "/", month, "/hms_smoke", date_sequence, ".kml")
       download_names = paste0(directory_to_download, "hms_smoke_", data_format, "_", date_sequence, ".kml")
     }
   }
   
   #### 3. Downloading data
   if(!any(file.exists(download_names))){
     cat(paste0("Downloading requested files...\n"))
     download.file(file_urls, download_names, method = "libcurl", quiet = TRUE)
     cat(paste0("Requested files downloaded.\n"))
   }
   
   #### 4. Unzip "Shapefile" or copy "KML" files
   cat(paste0("Saving files in ", directory_to_save, ".\n"))
   if(data_format == "Shapefile"){
     for(u in 1:length(download_names)){
       unzip(download_names[u], exdir = directory_to_save)
     }
   } else if(data_format == "KML"){
     for(u in 1:length(download_names)){
       file.copy(from = download_names[u], to = directory_to_save)
     }
   }
   cat(paste0("Files saves.\n"))
   
   #### 5. Remove zip and .kml files from download directory
   if(remove_download == TRUE){
     cat(paste0("Deleting download files...\n"))
     for(z in 1:length(download_names)){
       file.remove(download_names[z])
     }
     cat(paste0("Download files deleted.\n"))
   }
   
}
