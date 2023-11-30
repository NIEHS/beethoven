#' @author Mitchell Manware
#' @description Unit test for for checking data download functions.
#'
testthat::test_that("GEOS-CF download URLs exist.", {
  withr::local_package("httr")
  # function parameters
  date_start <- "2018-01-01"
  date_end <- "2022-12-31"
  collections <- c("chm_tavg_1hr_g1440x721_v1",
                   "aqc_tavg_1hr_g1440x721_v1")
  directory_to_save <- "../testdata/"
  for (c in seq_along(collections)) {
    # run download function
    download_geos_cf_data(date_start = date_start,
                          date_end = date_end,
                          collection = collections[c],
                          directory_to_save = directory_to_save,
                          data_download_acknowledgement = TRUE,
                          download = FALSE)
    # TEST that directory_to_save exists
    testthat::expect_true(dir.exists(directory_to_save))
    # path with commands
    commands_path <- paste0(directory_to_save,
                            collections[c],
                            "_wget_commands.txt")
    # TEST that path with commands exists
    testthat::expect_true(file.exists(commands_path))
    # import wget commands
    wget_commands <- read.csv(commands_path,
                              header = FALSE)
    # convert to character
    wget_commands <- wget_commands[seq_len(nrow(wget_commands)), ]
    # extract URLs from `wget_commands`
    url_list <- NULL
    for (w in seq_along(wget_commands)) {
      command <- wget_commands[w]
      url <- stringr::str_split_i(command, " ", 2)
      url_list <- c(url_list, url)
    }
    # sample URLs
    url_sample <- sample(url_list, 30L, replace = FALSE)
    # apply check_url_file_exist to sample of urls
    url_status <- sapply(url_sample, check_url_file_exist)
    # TEST that URLs are character
    testthat::expect_true(is.character(url_list))
    # TEST that URLs exist
    testthat::expect_true(all(url_status))
  }
})
