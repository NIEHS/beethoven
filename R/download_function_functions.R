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
#' @param system_command character(1). Linux command to execute downloads.
#' Inherited from data download function.
#' @param commands_txt character(1).
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

#' directory_setup: standard path name and create directories if they do not
#' exist.
#' 
#' @description
#' Ensure standard path names for directory to save and download, and create
#' one or both directories if they do not already exist.
#' @param directory_to_download character(1).
#' @param directory_to_save character(1).
directory_setup <- function(
  directory = NULL
) {
  chars_dir <- nchar(directory)
  if (substr(directory,
             chars_dir,
             chars_dir) != "/") {
    directory <- paste(directory,
                       "/",
                       sep = "")
  }
  if (dir.exists(directory) == FALSE) {
    dir.create(directory)
  }
  return(directory)
}
