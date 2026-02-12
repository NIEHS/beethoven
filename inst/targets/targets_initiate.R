################################################################################
##### Initiate pipeline arguments
target_initiate <-
  list(
    targets::tar_target(
      chr_dates,
      command = amadeus::generate_date_sequence(
        chr_daterange[1],
        chr_daterange[2],
        sub_hyphen = FALSE
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_initiate"
        )
      ),
      description = "initiate | Date range as character"
    ),
    targets::tar_target(
      chr_years,
      command = unique(lubridate::year(chr_dates)),
      iteration = "list",
      description = "initiate | Year range ",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_initiate"
        )
      )
    ),
    targets::tar_target(
      list_dates,
      command = beethoven::split_dates(
        dates = chr_daterange,
        n = num_dates_split,
        year = TRUE
      ),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_initiate"
        )
      ),
      description = "initiate | Dates as list (YYYY-MM-DD)"
    ),
    targets::tar_target(
      list_dates_julian,
      command = lapply(list_dates, function(x) format(as.Date(x), "%Y%j")),
      iteration = "list",
      description = "initiate | Dates as list (YYYYDDD)",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_initiate"
        )
      )
    ),
    targets::tar_target(
      list_dates_small,
      command = beethoven::split_dates(
        dates = chr_daterange,
        n = 10,
        year = TRUE
      ),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_initiate"
        )
      )
    ),
    targets::tar_target(
      chr_iter_radii,
      command = c(1000, 10000, 50000),
      iteration = "list",
      description = "initiate | Buffer radii",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_initiate"
        )
      )
    ),
    targets::tar_target(
      arglist_common,
      command = beethoven::set_args_calc(
        char_siteid = "site_id",
        char_timeid = "time",
        char_period = chr_daterange,
        num_extent = c(-126, -62, 22, 52),
        char_user_email = paste0(Sys.getenv("USER"), "@nih.gov"),
        char_input_dir = chr_input_dir
      ),
      description = "initiate | Calculation arguments",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_initiate"
        )
      )
    )
  )
