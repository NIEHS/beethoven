target_init <-
  list(
    targets::tar_target(
      char_feat_proc_timerange,
      command = c("2020-01-01", "2020-01-15"),
      #c(meta_run("date_start"), meta_run("date_end"))
    )
    ,
    targets::tar_target(
      sf_feat_proc_aqs_sites,
      read_locs(
        path = list.files(
          path = meta_run("dir_input_aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE
        ),
        date = NULL,
        return_format = "sf"
      )
    )
    ,
    targets::tar_target(
      sf_feat_proc_aqs_sites_time,
      read_locs(
        path = list.files(
          path = meta_run("dir_input_aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE
        ),
        date = char_feat_proc_timerange,
        return_format = "sf"
      )
    )
    ,
    targets::tar_target(
      sf_feat_proc_aqs_pm25,
      get_aqs_data(
        path = list.files(
          path = meta_run("dir_input_aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE),
        site_spt = sf::st_drop_geometry(sf_feat_proc_aqs_sites_time)
      ) |> dplyr::filter(time >= meta_run("date_start") & time <= meta_run("date_end"))
    )
  )
