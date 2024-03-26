target_init <-
  list(
    # tar_target for base directories and files
    targets::tar_target(
      sites_spat,
      read_locs(
        path = list.files(
          path = mr("dir_input_aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE),
        date = NULL
      )[1:10, ]
    )
    ,
    targets::tar_target(
      sites_time,
      read_locs(
        path = list.files(
          path = mr("dir_input_aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE),
        date = c(mr("date_start"), mr("date_end"))
      )[1:10, ]
    )
    ,
    targets::tar_target(
      sites_pm,
      get_aqs_data(
        path = list.files(
          path = mr("dir_input_aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE),
        site_spt = sites_time
      )[1:10, ]
    )
    ,
    targets::tar_target(
      time_range,
      command = c(mr("date_start"), mr("date_end"))
    )
  )