################################################################################
##### unit and integration tests for argument setting functions
##### main files: R/set_arguments.R


################################################################################
##### set_args_download
testthat::test_that("set_args_download exports qs or rds file", {
  withr::local_package("qs")

  # create a temporary directory
  tmpdir <- tempdir()
  tempqsfile <- file.path(tmpdir, "specdl_test.qs")
  temprdsfile <- file.path(tmpdir, "specdl_test.rds")

  # run set_args_download with qs extension
  testthat::expect_warning(
    calcspec1 <- beethoven::set_args_download(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      path_export = tempqsfile
    )
  )
  testthat::expect_true(is.list(calcspec1))
  testthat::expect_length(calcspec1, 20)

  # run set_args_download with rds extension
  testthat::expect_warning(
    calcspec2 <- beethoven::set_args_download(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      path_export = temprdsfile
    )
  )
  testthat::expect_true(is.list(calcspec2))
  testthat::expect_length(calcspec2, 20)

  # run set_args_download with qs file and nasa token
  testthat::expect_no_error(
    dlspec1 <- beethoven::set_args_download(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      nasa_earth_data_token = "mytoken",
      path_export = tempqsfile
    )
  )
  testthat::expect_true(is.list(dlspec1))
  testthat::expect_length(dlspec1, 20)

  # run set_args_download with rds file and nasa token
  testthat::expect_no_error(
    dlspec2 <- beethoven::set_args_download(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      nasa_earth_data_token = "mytoken",
      path_export = temprdsfile
    )
  )
  testthat::expect_true(is.list(dlspec2))
  testthat::expect_length(dlspec2, 20)

  # export download spec to qs file will give a message
  testthat::expect_message(
    dlspecqs <- beethoven::set_args_download(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      nasa_earth_data_token = "mytoken",
      export = TRUE,
      path_export = tempqsfile
    )
  )
  testthat::expect_true(is.list(dlspecqs))
  testthat::expect_equal(length(dlspecqs), 20)
  testthat::expect_true(file.exists(tempqsfile))

  # export download spec to rds file will give a message
  testthat::expect_message(
    dlspecrds <- beethoven::set_args_download(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      nasa_earth_data_token = "mytoken",
      export = TRUE,
      path_export = temprdsfile
    )
  )
  testthat::expect_true(is.list(dlspecrds))
  testthat::expect_equal(length(dlspecrds), 20)
  testthat::expect_true(file.exists(temprdsfile))

  # export download spec to a file with other extensions will stop
  testthat::expect_error(
    dlspecrds <- beethoven::set_args_download(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      nasa_earth_data_token = "mytoken",
      export = TRUE,
      path_export = file.path(tmpdir, "specdl_test.txt")
    )
  )

  # warning if no nasa_earth_data_token is provided
  testthat::expect_warning(
    dlspecrds <- beethoven::set_args_download(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      nasa_earth_data_token = NULL,
      export = TRUE,
      path_export = file.path(tmpdir, "specdl_test.qs")
    )
  )

  unlink(tmpdir, recursive = TRUE)

})


################################################################################
##### set_args_calc
testthat::test_that("set_args_calc exports qs or rds file", {
  withr::local_package("qs")

  # create a temporary directory
  tmpdir <- tempdir()
  tempqsfile <- file.path(tmpdir, "spec_test.qs")
  temprdsfile <- file.path(tmpdir, "spec_test.rds")

  # run set_args_calc with qs extension
  testthat::expect_no_error(
    calcspec1 <- beethoven::set_args_calc(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      path_export = tempqsfile
    )
  )
  testthat::expect_true(is.list(calcspec1))
  testthat::expect_length(calcspec1, 15)

  # run set_args_calc with rds extension
  testthat::expect_no_error(
    calcspec2 <- beethoven::set_args_calc(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      path_export = temprdsfile
    )
  )
  testthat::expect_true(is.list(calcspec2))
  testthat::expect_length(calcspec2, 15)

  # blank paths if try to search nonexisting paths
  testthat::expect_no_error(
    calcspec_neq <- beethoven::set_args_calc(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      export = TRUE,
      path_export = tempqsfile
    )
  )
  testthat::expect_true(is.list(calcspec_neq))
  testthat::expect_length(calcspec_neq, 15)
  testthat::expect_true(file.exists(tempqsfile))

  # blank paths if try to search nonexisting paths
  testthat::expect_no_error(
    calcspec_ner <- beethoven::set_args_calc(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      export = TRUE,
      path_export = temprdsfile
    )
  )
  testthat::expect_true(is.list(calcspec_ner))
  testthat::expect_length(calcspec_ner, 15)
  testthat::expect_true(file.exists(temprdsfile))

  # GlobalEnv objects if no path_export is provided
  testthat::expect_no_error(
    calcspec_nullpath <- beethoven::set_args_calc(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      export = TRUE,
      path_export = NULL
    )
  )
  testthat::expect_true(is.list(arglist_calcspec))

  # error with non-qs or -rds extension
  testthat::expect_error(
    beethoven::set_args_calc(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      export = TRUE,
      path_export = file.path(tmpdir, "specdl_test.txt")
    )
  )

  unlink(tmpdir, recursive = TRUE)

})
