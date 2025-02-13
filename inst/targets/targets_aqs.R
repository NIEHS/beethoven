################################################################################
##### Import US EPA AQS data
target_aqs <-
  list(
    ###########################         AQS          ###########################
    targets::tar_target(
      list_feat_proc_aqs_sites,
      command = {
        # download_aqs
        sf_feat_proc_aqs_sites_date <- amadeus::process_aqs(
          path = file.path(chr_input_dir, "aqs", "data_files"),
          date = chr_daterange,
          mode = "location",
          data_field = "Arithmetic.Mean",
          return_format = "sf"
        )
        # list_feat_split_aqs_sites <- lapply(
        #   split(
        #     sf_feat_proc_aqs_sites_date,
        #     sf_feat_proc_aqs_sites_date$site_id
        #   ),
        #   function(x) {
        #     rownames(x) <- NULL
        #     x
        #   }
        # )
        # list_feat_state_aqs_sites <- lapply(
        #   lapply(
        #     split(
        #       names(list_feat_split_aqs_sites),
        #       substr(names(list_feat_split_aqs_sites), 1, 2)
        #     ), function(x) list_feat_split_aqs_sites[x]
        #   ),
        #   function(x) dplyr::bind_rows(x)
        # )
        # # list_feat_state_aqs_sites
        # list_feat_split_aqs_sites
        sf_feat_proc_aqs_sites_date
      },
      description = "AQS locations | aqs"
    )
    ,
    targets::tar_target(
      dt_feat_proc_aqs_sites_time,
      command = {
        # download_aqs
        amadeus::process_aqs(
          path = list.files(
            path = file.path(
              arglist_common$char_input_dir,
              "aqs",
              "data_files"
            ),
            pattern = "daily_88101_[0-9]{4}.csv",
            full.names = TRUE
          ),
          date = arglist_common$char_period,
          mode = "available-data",
          data_field = c("Arithmetic.Mean", "Event.Type"),
          return_format = "data.table"
        )
      },
      description = "AQS locations with time and data | aqs"
    )
  )
