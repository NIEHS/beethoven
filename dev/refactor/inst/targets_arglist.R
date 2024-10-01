################################################################################
##### Set pipeline arguments
target_arglist <-
  list(
    targets::tar_target(
      chr_daterange,
      command = c("2018-01-01", "2018-07-31"),
      description = "Date range ***** CRITIAL TARGET *****"
    )
    ,
    targets::tar_target(
      list_dates,
      command = beethoven::split_dates(
        dates = chr_daterange,
        n = 50
      ),
      description = "Split date range into list"
    )
    ,
    targets::tar_target(
      chr_dates,
      command = names(list_dates),
      description = "Names of date list"
    )
    ,
    targets::tar_target(
      generate_list_download,
      command = FALSE,
      description = "Export (TRUE) or not (FALSE) download argument file"
    )
    ,
    targets::tar_target(
      arglist_download,
      command = set_args_download(
        char_period = chr_daterange,
        char_input_dir = "input",
        nasa_earth_data_token = NULL, #Sys.getenv("NASA_EARTHDATA_TOKEN"),
        mod06_filelist = "inst/mod06_links_2018_2022.csv",
        export = generate_list_download,
        path_export = "inst/download_spec.qs"
      ),
      description = "Set download arguments"
    )
    ,
    targets::tar_target(
      generate_list_calc,
      command = FALSE,
      description = "Export (TRUE) or not (FALSE) calculation argument file"
    )
    ,
    targets::tar_target(
      arglist_common,
      command = set_args_calc(
        char_siteid = "site_id",
        char_timeid = "time",
        char_period = chr_daterange,
        num_extent = c(-126, -62, 22, 52),
        char_user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
        export = generate_list_calc,
        path_export = "inst/calc_spec.qs",
        char_input_dir = "/ddn/gs1/group/set/Projects/NRT-AP-Model/input"
      ),
      description = "Set calculation arguments"
    )
  )
