#' @author Mitchell Manware
#' @description Unit test for for checking data download functions.
#'
setwd("/Volumes/manwareme/NRT-AP-Model/tests/testthat/")
library(testthat)
testthat::test_that("GEOS-CF download URLs exist.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2018-01-01"
  date_end <- "2022-12-31"
  collections <- c("aqc_tavg_1hr_g1440x721_v1",
                   "chm_inst_1hr_g1440x721_p23")
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
    # remove path with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("GMTED download URLs exist.", {
  withr::local_package("httr")
  # function parameters
  statistics <- c("Breakline Emphasis", "Systematic Subsample",
                  "Median Statistic", "Minimum Statistic",
                  "Mean Statistic", "Maximum Statistic",
                  "Standard Deviation Statistic")
  resolution <- "7.5 arc-seconds"
  directory_to_download <- "../testdata/"
  directory_to_save <- "../testdata/"
  for (s in seq_along(statistics)) {
    # run download function
    download_gmted_data(statistic = statistics[s],
                        resolution = resolution,
                        directory_to_download = directory_to_download,
                        directory_to_save = directory_to_save,
                        data_download_acknowledgement = TRUE,
                        unzip = FALSE,
                        remove_zip = FALSE,
                        download = FALSE)
    # TEST that directory_to_download exists
    testthat::expect_true(dir.exists(directory_to_download))
    # TEST that directory_to_save exists
    testthat::expect_true(dir.exists(directory_to_save))
    # path with commands
    commands_path <- paste0(directory_to_download,
                            "gmted_",
                            gsub(" ", "", statistics[s]),
                            "_",
                            gsub(" ", "", resolution),
                            "_curl_command.txt")
    # TEST that path with command exists
    testthat::expect_true(file.exists(commands_path))
    # import curl command
    curl_command <- read.csv(commands_path,
                             header = FALSE)
    # convert to character
    curl_command <- curl_command[seq_len(nrow(curl_command)), ]
    # extract URL from `curl_command`
    url <- stringr::str_split_i(curl_command, " ", 6)
    # apply check_url_file_exist to URL
    url_status <- check_url_file_exist(url)
    # TEST that URLs are character
    testthat::expect_true(is.character(url))
    # TEST that URLs exist
    testthat::expect_true(all(url_status))
    # remove path with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("NCEP NARR monolevel download URLs exist.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2022
  variables <- c("weasd", "air.2m")
  directory_to_save <- "../testdata/"
  # run download function
  download_narr_monolevel_data(year_start = year_start,
                               year_end = year_end,
                               variables = variables,
                               directory_to_save = directory_to_save,
                               data_download_acknowledgement = TRUE,
                               download = FALSE)
  # TEST that directory_to_save exists
  testthat::expect_true(dir.exists(directory_to_save))
  # path with commands
  commands_path <- paste0(directory_to_save,
                          "narr_monolevel_curl_commands.txt")
  # TEST that path with commands exists
  testthat::expect_true(file.exists(commands_path))
  # import curl commands
  curl_commands <- read.csv(commands_path,
                            header = FALSE)
  # convert to character
  curl_commands <- curl_commands[seq_len(nrow(curl_commands)), ]
  # extract URLs from `wget_commands`
  url_list <- NULL
  for (w in seq_along(curl_commands)) {
    command <- curl_commands[w]
    url <- stringr::str_split_i(command, " ", 6)
    url_list <- c(url_list, url)
  }
  # sample URLs
  url_sample <- sample(url_list, 10L, replace = FALSE)
  # apply check_url_file_exist to sample of urls
  url_status <- sapply(url_sample, check_url_file_exist)
  # TEST that URLs are character
  testthat::expect_true(is.character(url_list))
  # TEST that URLs exist
  testthat::expect_true(all(url_status))
  # remove path with commands after test
  file.remove(commands_path)
})

testthat::test_that("NCEP NARR pressure levels download URLs exist.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2022
  variables <- c("shum", "omega")
  directory_to_save <- "../testdata/"
  # run download function
  download_narr_p_levels_data(year_start = year_start,
                              year_end = year_end,
                              variables = variables,
                              directory_to_save = directory_to_save,
                              data_download_acknowledgement = TRUE,
                              download = FALSE)
  # TEST that directory_to_save exists
  testthat::expect_true(dir.exists(directory_to_save))
  # path with commands
  commands_path <- paste0(directory_to_save,
                          "narr_p_levels_curl_commands.txt")
  # TEST that path with commands exists
  testthat::expect_true(file.exists(commands_path))
  # import curl commands
  curl_commands <- read.csv(commands_path,
                            header = FALSE)
  # convert to character
  curl_commands <- curl_commands[seq_len(nrow(curl_commands)), ]
  # extract URLs from `wget_commands`
  url_list <- NULL
  for (w in seq_along(curl_commands)) {
    command <- curl_commands[w]
    url <- stringr::str_split_i(command, " ", 6)
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
  # remove path with commands after test
  file.remove(commands_path)
})
