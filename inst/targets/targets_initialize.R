target_init <-
  list(
    targets::tar_target(
      sf_feat_proc_aqs_sites,
      read_locs(
        export = FALSE,
        path = list.files(
          path = file.path(arglist_common$char_input_dir, "aqs"),
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
          path = file.path(arglist_common$char_input_dir, "aqs"),
          pattern = "daily_88101_[0-9]{4}.csv",
          full.names = TRUE
        ),
        date = arglist_common$char_period,
        mode = "sparse",
        data_field = c("Arithmetic.Mean", "Event.Type"),
        return_format = "data.table"
      ),
      description = "AQS sites with time"
    )
  )
