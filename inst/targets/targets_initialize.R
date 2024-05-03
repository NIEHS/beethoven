target_init <-
  list(
    targets::tar_target(
      sf_feat_proc_aqs_sites,
      read_locs(
        path = list.files(
          path = "input/aqs",
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE
        ),
        date = NULL,
        mode = "location",
        return_format = "sf"
      ),
      description = "AQS sites"
    )
    ,
    targets::tar_target(
      dt_feat_proc_aqs_sites_time,
      read_locs(
        path = list.files(
          path = "input/aqs",
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE
        ),
        date = arglist_common$char_period,
        mode = "sparse",
        return_format = "data.table"
      ),
      description = "AQS sites with time"
    )
    # ,
    # targets::tar_target(
    #   sf_feat_proc_aqs_pm25,
    #   get_aqs_data(
    #     path = list.files(
    #       path = meta_run("dir_input_aqs"),
    #       pattern = "daily_88101_[0-9]{4}.csv",
    #       full.names = TRUE),
    #     site_spt = sf::st_drop_geometry(sf_feat_proc_aqs_sites_time) |>
    #       dplyr::mutate(time = as.character(time))
    #   ) |> dplyr::filter(time %tin% arglist_common$char_period),
    #   description = "AQS sites with PM2.5"
    # )
  )
