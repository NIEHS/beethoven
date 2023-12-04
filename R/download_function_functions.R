################################################################################
# Date created: 2023-12-04
# Packages required: None
################################################################################

################################################################################
#' execute_download: execute or skip `system_command` in data download function.
#' 
#' @description
#' Execute or skip the commands listed in the ...wget/curl_commands.txt file
#' produced by one of the data download functions.
#' @param download logical(1). Execute (`TRUE`) or skip (`FALSE`) download.
execute_download <- function(
  download = FALSE,
  system_command = NULL,
  commands_txt = NULL
) {
  if (download == TRUE) {
    cat(paste0("Downloading requested files...\n"))
    system(command = system_command)
    cat(paste0("Requested files have been downloaded.\n"))
    file.remove(commands_txt)
  } else if (download == FALSE) {
    return(cat(paste0("Skipping data download.\n")))
  }
}
