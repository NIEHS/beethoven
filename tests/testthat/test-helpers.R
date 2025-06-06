################################################################################
##### unit and integration tests for helper functions
##### main files: R/helpers.R

################################################################################
##### test
testthat::test_that("test", {
  # expect error for lack of `.sif` file
  withr::with_tempdir({
      testthat::expect_error(beethoven:::test())
    }, clean = TRUE)
})

################################################################################
##### cov
testthat::test_that("cov", {
  # expect error for lack of `.sif` file
  withr::with_tempdir({
      testthat::expect_error(beethoven:::cov("test"))
    }, clean = TRUE)
})

################################################################################
##### interactive
testthat::test_that("interactive", {
  # expect sh error for lack of `.sif` file but not caught by `expect_error`
  withr::with_tempdir({
      testthat::expect_no_error(beethoven:::interactive())
    }, clean = TRUE)
})
