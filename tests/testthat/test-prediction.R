################################################################################
##### unit and integration tests for prediciton grid functions
##### main files: R/prediction.R

################################################################################
##### reduce_list
testthat::test_that("reduce_list", {
  l1 <- list(
    data.frame(a = 1:3, b = 4:6),
    data.frame(a = 7:9, b = 10:12)
  )
  testthat::expect_no_error(
    rl1 <- reduce_list(l1)
  )
  testthat::expect_true(is.list(rl1))
  testthat::expect_length(rl1, 1)
  testthat::expect_s3_class(rl1[[1]], "data.frame")
  testthat::expect_equal(dim(rl1[[1]]), c(6, 2))

  l2 <- append(l1, list(data.frame(c = 13:15, d = 16:18)))
  testthat::expect_no_error(
    rl2 <- reduce_list(l2)
  )
  testthat::expect_true(is.list(rl2))
  testthat::expect_length(rl2, 2)
  testthat::expect_s3_class(rl2[[2]], "data.frame")
  testthat::expect_equal(dim(rl2[[2]]), c(3, 2))
})


################################################################################
##### split_dates
testthat::test_that("split_dates", {
  testthat::expect_no_error(
    sd1 <- split_dates(c("2022-01-01", "2022-12-31"), 5, year = TRUE)
  )
  testthat::expect_true(is.list(sd1))
  # all lists are 5 because dates are within same year
  testthat::expect_true(unlist(unique(lapply(sd1, length))) == 5)

  testthat::expect_no_error(
    sd2 <- split_dates(c("2022-12-01", "2023-01-31"), 20)
  )
  testthat::expect_true(is.list(sd2))
  testthat::expect_length(sd2, 4)
  testthat::expect_length(unlist(unique(lapply(sd2, length))), 2)

  testthat::expect_no_error(
    sd3 <- split_dates(c("2022-12-01", "2023-01-29"), 5, year = FALSE)
  )
  # all lists are 5 because year = FALSE
  testthat::expect_true(unlist(unique(lapply(sd3, length))) == 5)
})


################################################################################
##### fl_dates
testthat::test_that("fl_dates", {
  testthat::expect_no_error(
    sd2 <- split_dates(c("2022-01-01", "2022-12-31"), 10)
  )
  testthat::expect_no_error(
    fl2 <- fl_dates(sd2[[1]])
  )
  testthat::expect_length(fl2, 2)
  testthat::expect_equal(
    fl2,
    c(as.Date("2022-01-01"), as.Date("2022-01-10"))
  )
})
