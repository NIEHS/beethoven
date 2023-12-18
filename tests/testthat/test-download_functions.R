#' @author Mitchell Manware
#' @description Unit test for for checking data download functions.
testthat::test_that("Error when data_download_acknowledgement = FALSE", {
  download_datasets <- c("aqs", "ecoregion", "geos", "gmted", "koppen",
                         "koppengeiger", "merra2", "merra", "narr_monolevel",
                         "narr_p_levels", "nlcd", "noaa", "sedac_groads",
                         "sedac_population", "groads", "population", "plevels",
                         "p_levels", "monolevel", "hms", "smoke")
  for (d in seq_along(download_datasets)) {
    expect_message(
      download_data(dataset_name = download_datasets[d],
                    data_download_acknowledgement = FALSE),
      paste0("Please refer to the argument list and the error message above ",
             "to rectify the error.")
    )
  }
})

testthat::test_that("Error when one parameter is NULL.", {
  # will be expanded to all dataset names
  download_datasets <- c("aqs", "geos", "gmted", "narr_monolevel",
                         "narr_p_levels", "nlcd", "noaa", "plevels",
                         "p_levels", "monolevel", "hms", "smoke")
  for (d in seq_along(download_datasets)) {
    expect_message(
      download_data(dataset_name = download_datasets[d],
                    data_download_acknowledgement = TRUE,
                    directory_to_save = NULL),
      paste0("Please refer to the argument list and the error message above ",
             "to rectify the error.")
    )
  }
})

testthat::test_that("EPA AQS download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  year_start <- 2018
  year_end <- 2022
  resolution_temporal <- "daily"
  parameter_code <- 88101
  directory_to_download <- "../testdata/"
  directory_to_save <- "../testdata/"
  # run download function
  download_aqs_data(year_start = year_start,
                    year_end = year_end,
                    directory_to_save = directory_to_save,
                    directory_to_download = directory_to_download,
                    data_download_acknowledgement = TRUE,
                    unzip = FALSE,
                    remove_zip = FALSE,
                    download = FALSE,
                    remove_command = FALSE)
  # define file path with commands
  commands_path <- paste0(
    directory_to_download,
    "aqs_",
    parameter_code,
    "_",
    year_start, "_", year_end,
    "_",
    resolution_temporal,
    "_curl_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 2)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = length(urls), method = "HEAD")
  # implement unit tets
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})

testthat::test_that("GEOS-CF download URLs have HTTP status 200.", {
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
    # define file path with commands
    commands_path <- paste0(directory_to_save,
                            collections[c],
                            "_",
                            date_start,
                            "_",
                            date_end,
                            "_wget_commands.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 2)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 30L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("GMTED download URLs have HTTP status 200.", {
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
    # define file path with commands
    commands_path <- paste0(directory_to_download,
                            "gmted_",
                            gsub(" ", "", statistics[s]),
                            "_",
                            gsub(" ", "", resolution),
                            "_",
                            Sys.Date(),
                            "_curl_command.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 6)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("MERRA2 download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2018-01-01"
  date_end <- "2022-12-31"
  collections <- c("inst1_2d_asm_Nx", "inst3_3d_asm_Np")
  directory_to_save <- "../testdata/"
  for (c in seq_along(collections)) {
    # run download function
    download_merra2_data(date_start = date_start,
                         date_end = date_end,
                         collection = collections[c],
                         directory_to_save = directory_to_save,
                         data_download_acknowledgement = TRUE,
                         download = FALSE)
    # define path with commands
    commands_path <- paste0(directory_to_save,
                            collections[c],
                            "_",
                            date_start,
                            "_",
                            date_end,
                            "_wget_commands.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 2)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 30L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("NARR monolevel download URLs have HTTP status 200.", {
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
  # define path with commands
  commands_path <- paste0(directory_to_save,
                          "narr_monolevel_",
                          year_start, "_", year_end,
                          "_curl_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 6)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 5L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})

testthat::test_that("NARR p-levels download URLs have HTTP status 200.", {
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
  # define file path with commands
  commands_path <- paste0(directory_to_save,
                          "narr_p_levels_",
                          year_start, "_", year_end,
                          "_curl_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 6)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 30L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})

testthat::test_that("NOAA HMS Smoke download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  date_start <- "2022-01-01"
  date_end <- "2022-12-31"
  directory_to_download <- "../testdata/"
  directory_to_save <- "../testdata/"
  # run download function
  download_noaa_hms_smoke_data(date_start = date_start,
                               date_end = date_end,
                               directory_to_download = directory_to_download,
                               directory_to_save = directory_to_save,
                               data_download_acknowledgement = TRUE,
                               download = FALSE,
                               remove_command = FALSE,
                               unzip = FALSE,
                               remove_zip = FALSE)
  # define file path with commands
  commands_path <- paste0(directory_to_download,
                          "hms_smoke_",
                          gsub("-", "", date_start),
                          "_",
                          gsub("-", "", date_end),
                          "_curl_commands.txt")
  # import commands
  commands <- read_commands(commands_path = commands_path)
  # extract urls
  urls <- extract_urls(commands = commands, position = 6)
  # check HTTP URL status
  url_status <- check_urls(urls = urls, size = 30L, method = "HEAD")
  # implement unit tests
  test_download_functions(directory_to_save = directory_to_save,
                          commands_path = commands_path,
                          url_status = url_status)
  # remove file with commands after test
  file.remove(commands_path)
})

testthat::test_that("NLCD download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- c(2021, 2019, 2016)
  collections <- c(rep("Coterminous United States", 2), "Alaska")
  collection_codes <- c(rep("l48", 2), "ak")
  directory_to_download <- "../testdata/"
  directory_to_save <- "../testdata/"
  # run download function
  for (y in seq_along(years)) {
    download_nlcd_data(year = years[y],
                       collection = collections[y],
                       directory_to_download = directory_to_download,
                       directory_to_save = directory_to_save,
                       data_download_acknowledgement = TRUE,
                       download = FALSE,
                       remove_command = FALSE,
                       unzip = FALSE,
                       remove_zip = FALSE)
    # define file path with commands
    commands_path <- paste0(directory_to_download,
                            "nlcd_",
                            years[y],
                            "_land_cover_",
                            collection_codes[y],
                            "_",
                            Sys.Date(),
                            "_curl_command.txt")
    # import commands
    commands <- read_commands(commands_path = commands_path)
    # extract urls
    urls <- extract_urls(commands = commands, position = 5)
    # check HTTP URL status
    url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
    # implement unit tests
    test_download_functions(directory_to_download = directory_to_download,
                            directory_to_save = directory_to_save,
                            commands_path = commands_path,
                            url_status = url_status)
    # remove file with commands after test
    file.remove(commands_path)
  }
})

testthat::test_that("SEDAC groads download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  data_regions <- c("Americas", "Global", "Africa", "Asia",
                    "Europe", "Oceania East", "Oceania West")
  data_formats <- c("Geodatabase", "Shapefile")
  directory_to_download <- "../testdata/"
  directory_to_save <- "../testdata/"
  # run download function
  for (r in seq_along(data_regions)) {
    data_region <- data_regions[r]
    for (f in seq_along(data_formats)) {
      download_sedac_groads_data(data_format = data_formats[f],
                                 data_region = data_region,
                                 directory_to_download = directory_to_download,
                                 directory_to_save = directory_to_save,
                                 data_download_acknowledgement = TRUE,
                                 download = FALSE,
                                 unzip = FALSE,
                                 remove_zip = FALSE,
                                 remove_command = FALSE)
      # define file path with commands
      commands_path <- paste0(directory_to_download,
                              "sedac_groads_",
                              gsub(" ", "_", tolower(data_region)),
                              "_",
                              Sys.Date(),
                              "_curl_command.txt")
      # import commands
      commands <- read_commands(commands_path = commands_path)
      # extract urls
      urls <- extract_urls(commands = commands, position = 11)
      # check HTTP URL status
      url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
      # implement unit tests
      test_download_functions(directory_to_download = directory_to_download,
                              directory_to_save = directory_to_save,
                              commands_path = commands_path,
                              url_status = url_status)
      # remove file with commands after test
      file.remove(commands_path)
    }
  }
})

testthat::test_that("SEDAC population download URLs have HTTP status 200.", {
  withr::local_package("httr")
  withr::local_package("stringr")
  # function parameters
  years <- c("2000", "2005", "2010", "2015", "2020", "all")
  data_formats <- c("ASCII", "GeoTIFF")
  data_resolutions <- cbind(c("60 minute", "30 second", "2.5 minute",
                              "15 minute", "30 minute"),
                            c("1_deg", "30_sec", "2pt5_min",
                              "15_min", "30_min"))
  directory_to_download <- "../testdata/"
  directory_to_save <- "../testdata/"
  for (f in seq_along(data_formats)) {
    data_format <- data_formats[f]
    for (y in seq_along(years)) {
      year <- years[y]
      for (r in seq_len(nrow(data_resolutions))) {
        # run download function
        download_sedac_population_data(
          year = year,
          data_format = data_format,
          data_resolution = data_resolutions[r, 1],
          directory_to_download = directory_to_download,
          directory_to_save = directory_to_save,
          data_download_acknowledgement = TRUE,
          download = FALSE,
          unzip = FALSE,
          remove_zip = FALSE,
          remove_command = FALSE)
        # define file path with commands
        if (year == "all") {
          year <- "totpop"
        } else {
          year <- year
        }
        if (year == "totpop" && data_resolutions[r, 2] == "30_sec") {
          resolution <- "2pt5_min"
        } else {
          resolution <- data_resolutions[r, 2]
        }
        commands_path <- paste0(directory_to_download,
                                "sedac_population_",
                                year,
                                "_",
                                resolution,
                                "_",
                                Sys.Date(),
                                "_curl_commands.txt")
        # import commands
        commands <- read_commands(commands_path = commands_path)
        # extract urls
        urls <- extract_urls(commands = commands, position = 11)
        # check HTTP URL status
        url_status <- check_urls(urls = urls, size = 1L, method = "HEAD")
        # implement unit tests
        test_download_functions(directory_to_download = directory_to_download,
                                directory_to_save = directory_to_save,
                                commands_path = commands_path,
                                url_status = url_status)
        # remove file with commands after test
        file.remove(commands_path)
      }
    }
  }
})

