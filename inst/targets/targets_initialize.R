target_init <-
  list(
    targets::tar_target(
      time_range,
      command = c("2020-01-01", "2020-01-15"),
      #c(mr("date_start"), mr("date_end"))
    )
    ,
    targets::tar_target(
      sites_spat,
      read_locs(
        path = list.files(
          path = mr("dir_input_aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE
        ),
        date = NULL,
        return_format = "sf"
      )
    )
    ,
    targets::tar_target(
      sites_time,
      read_locs(
        path = list.files(
          path = mr("dir_input_aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE
        ),
        date = time_range,
        return_format = "sf"
      )
    )
    ,
    targets::tar_target(
      sites_pm,
      get_aqs_data(
        path = list.files(
          path = mr("dir_input_aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE),
        site_spt = sf::st_drop_geometry(sites_time)
      ) |> dplyr::filter(time >= as.Date("2020-01-01") & time <= as.Date("2020-01-15"))
    )
  )
