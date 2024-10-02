################################################################################
##### Set pipeline arguments
target_arglist <-
  list(
    targets::tar_target(
      chr_daterange,
      command = c("2018-01-01", "2018-07-31"),
      description = "Date range ***** CRITICAL TARGET *****"
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
      chr_dates_julian,
      command = format(
        amadeus::generate_date_sequence(
          chr_daterange[1],
          chr_daterange[2],
          FALSE
        ),
        "%Y%j"
      ),
      description = "Julian dates"
    )
    ,
    targets::tar_target(
      list_dates_julian,
      command = split(
        chr_dates_julian,
        ceiling(seq_along(chr_dates_julian) / 50)
      )
    )
    ,
    targets::tar_target(
      arglist_download,
      command = set_args_download(
        char_period = chr_daterange,
        char_input_dir = "input",
        nasa_earth_data_token = NULL, #Sys.getenv("NASA_EARTHDATA_TOKEN"),
        mod06_filelist = "inst/mod06_links_2018_2022.csv",
        export = FALSE
      ),
      description = "Set download arguments"
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
        export = FALSE,
        # char_input_dir = "/ddn/gs1/group/set/Projects/NRT-AP-Model/input"
        char_input_dir = "./input"
      ),
      description = "Set calculation arguments"
    )
  )
