# nocov start

## file check: chunking
## if using tarchetypes::tar_files,
## the file *lists* should be stored as a single file
## Provided that the download is completed in a defined
## time period such that users can distiguish a **set** of files
## from each other,
## timestamp check: `fs::file_info(...)$modification_time`
## can be used in bulk file check (will be time consuming as
## the number of files grow, though).
## The file list of the previous successful run will be stored as a file
## and we just save the file list of the current run, which are
## older than a certain rerun interval (e.g., 6 months).
## If one really wants to keep the shorter rerun interval,
## the strategy should be changed.
## THINK: How can we know the downloaded files are complete and correct?
## quick diagram:
## file set 1 ...  file set x
## (listing function runs)
## list1.rds  ...  listx.rds
## (hashed; not modified) ...   (not run)
## (pass)    ...   (run)
## ...       ...   (downstream process + calculation)
## (as-is)   ...   (as-is)    --- unless modified or manually invalidated

#' Load arguments from the formatted argument list file
#' @description This function loads the list object of arguments
#'   to be injected into the calculation functions defined at
#'   each target. The arguments are numeric or character, and some
#'   of these are function names. In this case, the internal function
#'  `unmarshal_function` is called to convert the function name
#'  to the actual function.
#' @keywords Utility
#' @param argfile character(1). Path to the argument file. RDS format.
#' @param dataset character(1). Dataset name.
#' @return A list of arguments stored in `dataset` slot of the
#'   argument file.
#' @importFrom qs qread
#' @export
loadargs <- function(argfile, dataset) {
  if (endsWith(argfile, ".rds")) {
    arglist <- readRDS(argfile)
  } else if (endsWith(argfile, ".qs")) {
    arglist <- qs::qread(argfile)
  } else {
    stop("Invalid format.")
  }
  check_args <- arglist[[dataset]]
  namecheck <- grep("preprocess|_function", names(check_args))
  if (length(namecheck) > 0) {
    for (i in namecheck) {
      check_args[[i]] <- unmarshal_function(check_args[[i]])
    }
  }
  return(check_args)
}


# nolint start
#' Load MODIS files from a specified path.
#'
#' This function takes a path and an optional pattern as input and
#'   returns a list of MODIS files found in the specified path.
#' @keywords Utility
#' @param path The path where the MODIS files are located.
#' @param pattern An optional regular expression pattern to filter the files.
#'   The default pattern is "hdf$".
#' @param date A vector of two dates to filter the files by.
#'   The default is an empty character vector.
#' @return A list of full file names of the MODIS files found
#'   in the specified path.
#'
#' @examples
#' \dontrun{
#' # Load MODIS files from the current directory
#' modis_files <- load_modis_files(".", date = c("2018-01-01", "2018-01-31"))
#'
#' # Load MODIS files from a specific directory with a custom pattern
#' modis_files <- load_modis_files("/path/to/files", pattern = "MOD.*hdf$", date = c("2018-01-01", "2018-01-31"))
#' }
#' @export
# nolint end
load_modis_files <- function(path, pattern = "hdf$", date = character(2)) {
  modis_files <-
    list.files(
      path, pattern = pattern,
      recursive = TRUE,
      full.names = TRUE
    )
  date_exp <-
    amadeus::generate_date_sequence(date[1], date[2], sub_hyphen = FALSE)
  date_exp <- strftime(date_exp, format = "%Y%j")
  modis_files <-
    grep(
      sprintf("(%s)", paste(paste0("A", date_exp), collapse = "|")),
      modis_files, value = TRUE
    )
  return(modis_files)
}


#' Read AQS data
#' @keywords Utility
#' @param fun_aqs function to import AQS data.
#' Default is `amadeus::process_aqs`
#' @param export Export the file to qs. Default is FALSE.
#' @param ... Passed arguments to `fun_aqs`
#' @return Depending on `fun_aqs` specification.
#' @importFrom qs qsave
#' @importFrom amadeus process_aqs
#' @export
read_locs <-
  function(
    fun_aqs = amadeus::process_aqs,
    export = FALSE,
    ...
  ) {
    aqs_read <- fun_aqs(...)
    if (export) qs::qsave(aqs_read, file = "input/sf_feat_proc_aqs_sites.qs")
    return(aqs_read)
  }



#' Unmarshal functions
#' @keywords Utility
#' @param pkg_func_str Character string specifying the package and function.
#' @return Function object.
#' @note The function name string must include two colons `::`.
#' Also, the package preceding the two colons should be loaded in the
#' current environment.
#' @description this function is developed to avoid
#'   random errors in compressing and decompressing R function objects
#'   with `qs::qsave` and `qs::qread`. If you encounter such errors, please use
#'   this function with function name strings to save and load the function
#'   objects.
#' @export
#' @examples
#' unmarshal_function("amadeus::process_aqs")
unmarshal_function <-
  function(pkg_func_str) {
    stopifnot(grepl("::", pkg_func_str))
    pkg_func_split <- strsplit(pkg_func_str, "::")[[1]]
    pkg_name <- pkg_func_split[1]
    func_name <- pkg_func_split[2]
    get(func_name, envir = asNamespace(pkg_name))
  }



#' Read paths from a directory with a specific file extension
#' @keywords Utility
#' @param path The directory path from which to read the paths.
#' @param extension The file extension to match. Defaults to ".hdf".
#' @param target_dates A character vector of length 2 containing
#'  the start and end dates.
#' @param julian logical(1). If `TRUE`, the dates are in Julian format.
#' @return A character vector containing the full paths of the matching files.
#'
#' @examples
#' \dontrun{
#' # Read paths from a directory with default extension
#' read_paths("/path/to/directory")
#'
#' # Read paths from a directory with custom extension
#' read_paths("/path/to/directory", ".txt")
#' }
#' @export
read_paths <-
  function(
    path,
    extension = ".hdf",
    target_dates = c("2020-01-01", "2020-01-15"),
    julian = FALSE
  ) {
    flist <-
      list.files(
        path = path,
        pattern = sprintf("%s$", extension),
        full.names = TRUE,
        recursive = TRUE
      )
    if (!missing(target_dates)) {
      dateseq <-
        seq(as.Date(target_dates[1]), as.Date(target_dates[2]), by = "day")
      dateseq <-
        if (julian) format(dateseq, "%Y%j") else format(dateseq, "%Y%m%d")
      dateseq <- sprintf("A(%s)", paste(dateseq, collapse = "|"))
      flist <- grep(dateseq, flist, value = TRUE)
    }
    return(flist)
  }

# nocov end
