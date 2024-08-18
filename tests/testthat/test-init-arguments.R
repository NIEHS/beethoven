testthat::test_that("set_args_calc exports qs or rds file", {
  withr::local_package("qs")

  # create a temporary directory
  tmpdir <- tempdir()
  tempqsfile <- file.path(tmpdir, "spec_test.qs")
  temprdsfile <- file.path(tmpdir, "spec_test.rds")

  # run set_args_calc with qs extension
  testthat::expect_no_error(
    calcspec <- set_args_calc(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      path_export = tempqsfile
    )
  )

  # run set_args_calc with rds extension
  testthat::expect_no_error(
    calcspec <- set_args_calc(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      path_export = temprdsfile
    )
  )

  # side effect if try to search nonexisting paths
  testthat::expect_error(
    calcspec <- set_args_calc(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      export = TRUE,
      path_export = tempqsfile
    )
  )

  # side effect if try to search nonexisting paths
  testthat::expect_error(
    calcspec <- set_args_calc(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      export = TRUE,
      path_export = temprdsfile
    )
  )

})

testthat::test_that("set_args_download exports qs or rds file", {
  withr::local_package("qs")

  # create a temporary directory
  tmpdir <- tempdir()
  tempqsfile <- file.path(tmpdir, "specdl_test.qs")
  temprdsfile <- file.path(tmpdir, "specdl_test.rds")

  # run set_args_download with qs extension
  testthat::expect_warning(
    calcspec <- set_args_download(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      path_export = tempqsfile
    )
  )

  # run set_args_download with rds extension
  testthat::expect_warning(
    calcspec <- set_args_download(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      path_export = temprdsfile
    )
  )

  # run set_args_download with qs extension
  testthat::expect_no_error(
    dlspec <- set_args_download(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      nasa_earth_data_token = "mytoken",
      path_export = tempqsfile
    )
  )

  # run set_args_download with rds extension
  testthat::expect_no_error(
    dlspec <- set_args_download(
      char_period = c("2018-01-01", "2018-01-31"),
      export = FALSE,
      nasa_earth_data_token = "mytoken",
      path_export = temprdsfile
    )
  )


  # export download spec to qs file will give a message
  testthat::expect_message(
    dlspecqs <- set_args_download(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      export = TRUE,
      path_export = tempqsfile
    )
  )
  testthat::expect_true(is.list(dlspecqs))
  testthat::expect_equal(length(dlspecqs), 20)

  # export download spec to rds file will give a message
  testthat::expect_message(
    dlspecrds <- set_args_download(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      export = TRUE,
      path_export = temprdsfile
    )
  )
  testthat::expect_true(is.list(dlspecrds))
  testthat::expect_equal(length(dlspecrds), 20)

  # export download spec to a file with other extensions will stop
  testthat::expect_error(
    dlspecrds <- set_args_download(
      char_input_dir = "/path/to/nonexisting",
      char_period = c("2018-01-01", "2018-01-31"),
      export = TRUE,
      path_export = file.path(tmpdir, "specdl_test.txt")
    )
  )

})
