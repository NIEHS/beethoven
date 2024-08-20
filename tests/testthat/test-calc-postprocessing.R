testthat::test_that("post_calc_autojoin expands and joins data.frames with different temporal resolutions", {
  withr::local_package("dplyr")
  withr::local_package("data.table")

  # data.frame that are resolved daily
  df_fine1 <-
    expand.grid(
      site_id = rep(LETTERS[1:4], 5),
      time =
      rep(
        seq.Date(as.Date("2021-12-30"), as.Date("2022-01-03"), by = 1),
        each = 4
      ),
      value = rnorm(20)
    )

  df_fine2 <-
    data.frame(
      site_id = rep(c("A", "B", "C", "D"), 2),
      time =
      c(
        as.Date(c("2022-01-01", "2022-01-02", "2021-12-31", "2021-01-03")),
        as.Date(c("2022-01-01", "2022-01-02", "2021-12-31", "2021-01-03")) + 1
      ),
      value2 = c(c(1+2i, 2+3i, 3+4i, 5+6i),
                c(1+2i, 2+3i, 3+4i, 5+6i) + 1)
    )
  # attempt to join two data.frames with the same temporal resolution
  # will run quietly
  testthat::expect_no_error(
    autojoin_iden <-
      post_calc_autojoin(
        df_fine = df_fine1, df_coarse = df_fine2,
        field_sp = "site_id", field_t = "time"
      )
  )

  df_coarse0 <- data.frame(site_id = c("A", "B", "C", "D"),
                           time = rep(2020, 4),
                           other_value = c(10, 20, 30, 40))
  # df_coarse0 includes year values. It will lead to an error when
  # attempting to convert the year values to Date objects.
  # we leverage that characteristics to detect whether the temporal
  # values are year or date. In the current beethoven implementation,
  # there are only two temporal resolutions: daily and yearly.
  testthat::expect_message(
    autojoin_diff <- post_calc_autojoin(df_fine1, df_coarse0)
  )

  # remove the year column
  df_coarse2 <- df_coarse0[, -2]
  # then it will quietly join two data.frames by site_id
  testthat::expect_no_error(
    autojoin_diff <- post_calc_autojoin(df_fine1, df_coarse2)
  )

})
