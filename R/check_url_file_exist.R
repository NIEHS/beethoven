# Function to test status of download function URLs
# Date created: 2023-11-30
# Insang Song (based on comment #164 Location of download functions)
# Mitchell Manware

#' Check if sample of download URLs exist
#'
#' @param url Download URL to be checked.
#' @author Insang Song; Mitchell Manware
#' @export
check_url_file_exist <- function(url) {
  http_status_ok <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  return(status == http_status_ok)
}
