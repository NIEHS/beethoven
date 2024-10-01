################################################################################
##### Import US EPA AQS data
target_aqs <-
  list(
    # targets::tar_target(
    #   sf_feat_proc_aqs_sites_REAL,
    #   read_locs(
    #     export = FALSE,
    #     path = list.files(
    #       path = file.path(
    #         arglist_common$char_input_dir,
    #         "aqs",
    #         "data_files"
    #       ),
    #       pattern = "daily_88101_[0-9]{4}.csv",
    #       full.names = TRUE
    #     ),
    #     date = arglist_common$char_period,
    #     mode = "location",
    #     return_format = "sf"
    #   ),
    #   description = "AQS sites"
    # )
    # ,
    # targets::tar_target(
    #   dt_feat_proc_aqs_sites_time,
    #   read_locs(
    #     path = list.files(
    #       path = file.path(
    #         arglist_common$char_input_dir,
    #         "aqs",
    #         "data_files"
    #       ),
    #       pattern = "daily_88101_[0-9]{4}.csv",
    #       full.names = TRUE
    #     ),
    #     date = arglist_common$char_period,
    #     mode = "available-data",
    #     data_field = c("Arithmetic.Mean", "Event.Type"),
    #     return_format = "data.table"
    #   ),
    #   description = "AQS sites with time"
    # )
    # ,
    #######################    AQS SITES PLACEHOLDER     #######################
    targets::tar_target(
      sf_locs,
      command = sf::read_sf(
        system.file("shape/nc.shp", package = "sf")
      )
    )
    ,
    targets::tar_target(
      sf_grid,
      command = sf::st_as_sf(
        sf::st_sample(sf_locs, 1097)
      )
    )
    ,
    targets::tar_target(
      sf_feat_proc_aqs_sites,
      command = cbind(
        site_id = sprintf(
          "A_%05d",
          seq_len(nrow(sf_grid))
        ),
        sf_grid
      )
    )
  )
