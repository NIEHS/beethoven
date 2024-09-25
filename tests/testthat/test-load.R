################################################################################
##### unit and integration tests for argument loading functions
##### main files: R/load.R

################################################################################
##### loadargs
testthat::test_that("load_args (download)", {
  # expect error with .csv
  testthat::expect_error(
    loadargs("error_file.csv", dataset = "hms")
  )
  # expect no error with .rds
  testthat::expect_no_error(
    args_rds <- loadargs(
      testthat::test_path("..", "testdata", "load", "download_args.rds"),
      dataset = "hms"
    )
  )
  # expect a list
  testthat::expect_true(is.list(args_rds))
  # expect length 6
  testthat::expect_length(args_rds, 6)

  # expect no error with .qs
  testthat::expect_no_error(
    args_qs <- loadargs(
      testthat::test_path("..", "testdata", "load", "download_args.qs"),
      dataset = "hms"
    )
  )
  # expect a list
  testthat::expect_true(is.list(args_qs))
  # expect length 6
  testthat::expect_length(args_qs, 6)

  # expect no error with two datasets
  testthat::expect_no_error(
    args_2 <- loadargs(
      testthat::test_path("..", "testdata", "load", "download_args.qs"),
      dataset = c("hms", "narr_monolevel")
    )
  )
  # expect a list
  testthat::expect_true(is.list(args_2))
  # expect length 2
  testthat::expect_length(args_2, 2)
  # expect sub-lists of length 6 (hms) and 22 (narr_monolevel)
  testthat::expect_true(all(c(6, 22) %in% unlist(lapply(args_2, length))))
})

testthat::test_that("load_args (calc)", {
  # expect no error with .qs (calc)
  testthat::expect_no_error(
    args_narr <- loadargs(
      testthat::test_path("..", "testdata", "load", "calc_args.qs"),
      dataset = "narr"
    )
  )
  # expect a list (calc)
  testthat::expect_true(is.list(args_narr))
  # expect length 10
  testthat::expect_length(args_narr, 10)
})


################################################################################
##### load_modis_files
testthat::test_that("load_modis_files", {
  # expect no error
  testthat::expect_no_error(
    files <- load_modis_files(
      path = testthat::test_path("..", "testdata", "load", "modis"),
      pattern = "hdf$",
      date = c("2018-01-01", "2018-01-01")
    )
  )
  # expect vector
  testthat::expect_vector(files)
  # expect 23 files
  testthat::expect_length(files, 23)
})

################################################################################
##### read_locs


################################################################################
##### unmarshal_function
testthat::test_that("unmarshal_function", {
  withr::local_package("qs")
  # expect no error for function name
  testthat::expect_no_error(
    unmarshal_example <- unmarshal_function("qs::qread")
  )
  # expect the output to be a function
  testthat::expect_true(is.function(unmarshal_example))
})


################################################################################
##### read_paths
testthat::test_that("read_paths", {
  # expect no error
  testthat::expect_no_error(
    files_date <- read_paths(
      path = testthat::test_path("..", "testdata", "load", "modis"),
      extension = ".hdf",
      target_dates = c("2018-01-01", "2018-01-01"),
      julian = TRUE
    )
  )
  # expect vector
  testthat::expect_vector(files_date)
  # expect 23 files with target dates
  testthat::expect_length(files_date, 23)

  # expect no error
  testthat::expect_no_error(
    files_nodate <- read_paths(
      path = testthat::test_path("..", "testdata", "load", "modis"),
      extension = ".hdf",
      target_dates = c("2018-01-01", "2018-01-02"),
      julian = TRUE
    )
  )
  # expect vector
  testthat::expect_vector(files_nodate)
  # expect 46 files without target dates
  testthat::expect_length(files_nodate, 46)
})
