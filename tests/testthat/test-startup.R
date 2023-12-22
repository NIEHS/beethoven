testthat::test_that("startup_nrtap works properly", {

  tdir <- tempdir()
  testthat::expect_no_error(
    startup_nrtap(
      workdir = tdir,
      user_mode = TRUE,
      username = "test"
    )
  )

  startup_nrtap(
    workdir = tdir,
    user_mode = TRUE,
    username = "test"
  )

  testthat::expect_true(dir.exists(file.path(tdir, "output", "test")))
  testthat::expect_equal(getwd(), tdir)
})
