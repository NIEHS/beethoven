################################################################################
##### Set pipeline arguments
target_initiate <-
  list(
    ############################################################################
    ############################################################################
    ###########################     CRITICAL TARGETS      ######################
    ##### 1. chr_daterange controls all time-related targets for the entire
    #####    pipeline. This is the only target that needs to be changed to
    #####    update the pipeline with a new temopral range. Month and year
    #####    specific arguments are derived from the time range defined by
    #####    chr_daterange.
    targets::tar_target(
      chr_daterange,
      command = c("2018-01-01", "2018-01-31"),
      description = "Date range"
    )
    ,
    ##### 2. chr_nasa_token sets the file path to the user's NASA Earthdata
    #####    account credentials. We can create a group credential file,
    #####    but this target is still critical since the CREDENTIALS
    #####    EXPIRE AT ~90 DAY INTERVALS. Regardless of the user or group
    #####    credential file, the token must be updated every 90 days.
    targets::tar_target(
      chr_nasa_token,
      command = readLines("~/nasa_token.txt"),
      description = "NASA Earthdata token"
    )
    ,
    ##### 3. chr_mod06_links is the file path to the MOD06 links file. These
    #####    links must be manually downloaded per the `amadeus::download_modis`
    #####    function. The links are then stored in a CSV file that is read
    #####    by the function. The new file with links must be updated to match
    #####    the new date range.
    targets::tar_target(
      chr_mod06_links,
      command = "/inst/targets/mod06_links_2018_2022.csv",
      description = "File of MOD06 links"
    )
    ,
    ############################################################################
    ############################################################################
    ############################################################################
    targets::tar_target(
      chr_years,
      command = seq(
        as.numeric(substr(chr_daterange[1], 1, 4)),
        as.numeric(substr(chr_daterange[2], 1, 4))
      ),
      description = "Year range"
    )
    ,
    targets::tar_target(
      list_dates,
      command = beethoven::split_dates(
        dates = chr_daterange,
        n = 10
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
        ceiling(seq_along(chr_dates_julian) / 10)
      )
    )
    ,
    targets::tar_target(
      chr_iter_radii,
      command = c(1000, 10000),
      # command = c(1000, 10000, 50000),
      description = "Buffer radii"
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
        char_input_dir = "/input"
      ),
      description = "Set calculation arguments"
    )
  )
