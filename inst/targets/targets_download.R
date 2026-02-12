target_download <-
  list(
    targets::tar_target(
      list_download_args,
      command = list(
        unzip = TRUE,
        remove_zip = TRUE,
        remove_command = FALSE,
        acknowledgement = TRUE,
        download = TRUE,
        hash = TRUE
      ),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_general_beethoven"
        )
      ),
      description = "Download | Common download arguments"
    ),
    ###########################         AQS          ###########################
    targets::tar_target(
      download_aqs,
      command = {
        amadeus::download_aqs(
          directory_to_save = file.path(chr_input_dir, "aqs"),
          year = chr_years,
          unzip = list_download_args$unzip,
          remove_zip = list_download_args$remove_zip,
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(chr_years),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm"
        )
      ),
      description = "Download | Download AQS data"
    ),
    ###########################         GEOS         ###########################
    targets::tar_target(
      chr_iter_calc_geos,
      command = c("aqc_tavg_1hr_g1440x721_v1", "chm_tavg_1hr_g1440x721_v1"),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm"
        )
      ),
      description = "Download | GEOS-CF features"
    ),
    targets::tar_target(
      download_geos,
      command = amadeus::download_geos(
        collection = chr_iter_calc_geos,
        directory_to_save = file.path(chr_input_dir, "geos"),
        date = beethoven::fl_dates(unlist(list_dates)),
        remove_command = list_download_args$remove_command,
        acknowledgement = list_download_args$acknowledgement,
        download = list_download_args$download,
        hash = list_download_args$hash
      ),
      pattern = cross(chr_iter_calc_geos, list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download | Download GEOS-CF data"
    ),
    ###########################         NARR         ###########################
    targets::tar_target(
      chr_iter_calc_narr,
      command = c(
        "air.sfc",
        "albedo",
        "apcp",
        "dswrf",
        "evap",
        "hcdc",
        "hpbl",
        "lcdc",
        "lhtfl",
        "mcdc",
        "pr_wtr",
        "prate",
        "pres.sfc",
        "shtfl",
        "snowc",
        "soilm",
        "tcdc",
        "ulwrf.sfc",
        "uwnd.10m",
        "vis",
        "vwnd.10m",
        "weasd",
        "omega",
        "shum"
      ),
      description = "Download | NARR features",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_initiate"
        )
      )
    ),
    targets::tar_target(
      chr_iter_calc_narr_lag,
      command = c(
        "air.sfc",
        "apcp",
        "pres.sfc",
        "shum",
        "uwnd.10m",
        "vwnd.10m"
      ),
      iteration = "list",
      description = "Download | NARR features | lag",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_initiate"
        )
      )
    ),
    targets::tar_target(
      download_narr,
      command = amadeus::download_narr(
        variables = chr_iter_calc_narr,
        directory_to_save = file.path(chr_input_dir, "narr"),
        year = chr_years,
        remove_command = list_download_args$remove_command,
        acknowledgement = list_download_args$acknowledgement,
        download = list_download_args$download,
        hash = list_download_args$hash
      ),
      pattern = cross(chr_iter_calc_narr, chr_years),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download NARR data | download"
    ),
    targets::tar_target(
      download_narr_lag,
      command = amadeus::download_narr(
        variables = chr_iter_calc_narr_lag,
        directory_to_save = file.path(chr_input_dir, "narr"),
        year = chr_years[1] - 1,
        remove_command = list_download_args$remove_command,
        acknowledgement = list_download_args$acknowledgement,
        download = list_download_args$download,
        hash = list_download_args$hash
      ),
      pattern = map(chr_iter_calc_narr_lag),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download NARR data | lag | download"
    ),

    ###########################         HMS          ###########################
    targets::tar_target(
      download_hms,
      command = amadeus::download_hms(
        directory_to_save = file.path(chr_input_dir, "hms"),
        date = beethoven::fl_dates(unlist(list_dates)),
        unzip = list_download_args$unzip,
        remove_zip = list_download_args$remove_zip,
        remove_command = list_download_args$remove_command,
        acknowledgement = list_download_args$acknowledgement,
        download = list_download_args$download,
        hash = list_download_args$hash
      ),
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download HMS data | download"
    ),
    ###########################       MODIS - MOD11       ######################
    targets::tar_target(
      download_mod11,
      command = {
        amadeus::download_modis(
          product = "MOD11A1",
          nasa_earth_data_token = chr_nasa_token,
          date = beethoven::fl_dates(unlist(list_dates)),
          directory_to_save = file.path(
            chr_input_dir,
            "modis",
            "raw",
            "061",
            "MOD11A1"
          ),
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download MODIS - MOD11 data | download"
    ),
    ###########################       MODIS - MOD06       ######################
    targets::tar_target(
      download_mod06,
      command = {
        amadeus::download_modis(
          product = "MOD06_L2",
          nasa_earth_data_token = chr_nasa_token,
          date = beethoven::fl_dates(unlist(list_dates)),
          directory_to_save = file.path(
            chr_input_dir,
            "modis",
            "raw",
            "061",
            "MOD06_L2"
          ),
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download MODIS - MOD06 data | download"
    ),
    ###########################       MODIS - MOD13       ######################
    targets::tar_target(
      download_mod13,
      command = {
        amadeus::download_modis(
          product = "MOD13A2",
          nasa_earth_data_token = chr_nasa_token,
          date = beethoven::fl_dates(unlist(list_dates)),
          directory_to_save = file.path(
            chr_input_dir,
            "modis",
            "raw",
            "061",
            "MOD13A2"
          ),
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download MODIS - MOD13 data | download"
    ),
    ###########################       MODIS - MCD19       ######################
    targets::tar_target(
      download_mcd19,
      command = {
        amadeus::download_modis(
          product = "MCD19A2",
          nasa_earth_data_token = chr_nasa_token,
          date = beethoven::fl_dates(unlist(list_dates)),
          directory_to_save = file.path(
            chr_input_dir,
            "modis",
            "raw",
            "61",
            "MCD19A2"
          ),
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download MODIS - MCD19 data | download"
    ),
    ###########################       MODIS - MOD09       ######################
    targets::tar_target(
      download_mod09,
      command = {
        amadeus::download_modis(
          product = "MOD09GA",
          nasa_earth_data_token = chr_nasa_token,
          date = beethoven::fl_dates(unlist(list_dates)),
          directory_to_save = file.path(
            chr_input_dir,
            "modis",
            "raw",
            "061",
            "MOD09GA"
          ),
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm"
        )
      ),
      description = "Download MODIS - MOD09 data | download"
    ),

    ###########################       MODIS - VIIRS       ######################
    targets::tar_target(
      download_viirs,
      command = {
        amadeus::download_modis(
          product = "VNP46A2",
          version = "5000",
          nasa_earth_data_token = chr_nasa_token,
          date = beethoven::fl_dates(unlist(list_dates)),
          directory_to_save = file.path(
            chr_input_dir,
            "modis",
            "raw",
            "5000",
            "VNP46A2"
          ),
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(list_dates),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download MODIS - VIIRS data | download"
    ),
    ###########################         GMTED        ###########################
    targets::tar_target(
      chr_iter_calc_gmted_vars,
      command = c(
        "Breakline Emphasis",
        "Systematic Subsample",
        "Median Statistic",
        "Minimum Statistic",
        "Mean Statistic",
        "Maximum Statistic",
        "Standard Deviation Statistic"
      ),
      description = "Download | GMTED features"
    ),
    targets::tar_target(
      download_gmted,
      command = {
        amadeus::download_gmted(
          statistic = chr_iter_calc_gmted_vars,
          resolution = "7.5 arc-seconds",
          directory_to_save = file.path(chr_input_dir, "gmted"),
          unzip = list_download_args$unzip,
          remove_zip = list_download_args$remove_zip,
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(chr_iter_calc_gmted_vars),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download GMTED data | download"
    ),
    ###########################         NLCD         ###########################
    targets::tar_target(
      chr_iter_calc_nlcd,
      command = c(2018, 2019, 2020, 2021, 2022),
      iteration = "list",
      description = "Download | NLCD years"
    ),
    targets::tar_target(
      download_nlcd,
      command = {
        amadeus::download_nlcd(
          year = chr_iter_calc_nlcd,
          directory_to_save = file.path(
            chr_input_dir,
            chr_iter_calc_nlcd,
            "nlcd"
          ),
          unzip = list_download_args$unzip,
          remove_zip = list_download_args$remove_zip,
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(chr_iter_calc_nlcd),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download NLCD data"
    ),
    ###########################        KOPPEN        ###########################
    targets::tar_target(
      download_koppen,
      command = {
        amadeus::download_koppen_geiger(
          data_resolution = "0.0083",
          time_period = "Present",
          directory_to_save = file.path(chr_input_dir, "koppen_geiger"),
          unzip = list_download_args$unzip,
          remove_zip = list_download_args$remove_zip,
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download Koppen-Geiger data | download"
    ),
    ###########################      POPULATION      ###########################
    targets::tar_target(
      download_population,
      command = {
        amadeus::download_population(
          data_resolution = "30 second",
          data_format = "GeoTIFF",
          year = "2020",
          directory_to_save = file.path(chr_input_dir, "population"),
          unzip = list_download_args$unzip,
          remove_zip = list_download_args$remove_zip,
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download population data | download"
    ),
    ###########################         TRI          ###########################
    targets::tar_target(
      download_tri,
      command = amadeus::download_tri(
        year = chr_years,
        directory_to_save = file.path(chr_input_dir, "tri"),
        remove_command = list_download_args$remove_command,
        acknowledgement = list_download_args$acknowledgement,
        download = list_download_args$download,
        hash = list_download_args$hash
      ),
      pattern = map(chr_years),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download TRI data | download"
    ),
    ###########################         NEI          ###########################
    targets::tar_target(
      chr_iter_calc_nei,
      command = c(2017, 2020),
      description = "Download | NEI features"
    ),
    targets::tar_target(
      download_nei,
      command = {
        amadeus::download_nei(
          year = chr_iter_calc_nei,
          directory_to_save = file.path(chr_input_dir, "nei"),
          unzip = list_download_args$unzip,
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      pattern = map(chr_iter_calc_nei),
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm"
        )
      ),
      description = "Download NEI data | download"
    ),
    ###########################      ECOREGIONS      ###########################
    targets::tar_target(
      download_ecoregions,
      command = {
        amadeus::download_ecoregion(
          directory_to_save = file.path(chr_input_dir, "ecoregions"),
          unzip = list_download_args$unzip,
          remove_zip = list_download_args$remove_zip,
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download ecoregions data | download"
    ),
    ###########################        GROADS        ###########################
    targets::tar_target(
      download_groads,
      command = {
        amadeus::download_groads(
          data_region = "Americas",
          data_format = "Geodatabase",
          directory_to_save = file.path(chr_input_dir, "groads"),
          unzip = list_download_args$unzip,
          remove_zip = list_download_args$remove_zip,
          remove_command = list_download_args$remove_command,
          acknowledgement = list_download_args$acknowledgement,
          download = list_download_args$download,
          hash = list_download_args$hash
        )
      },
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(
          controller = "controller_download_norm_big"
        )
      ),
      description = "Download gRoads data | download"
    )
  )
