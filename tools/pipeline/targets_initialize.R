target_init <-
    list(
    # tar_target for base directories and files
    targets::tar_target(sites_spat, read_locs(mr("dir_input_aqs")))
    ,
    targets::tar_target(sites_time, read_locs(mr("dir_input_aqs")))
    ,
    targets::tar_target(
        sites_pm,
        get_aqs_data()
    )
    )