################################################################################
##### Define critical targets
target_critical <-
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
      command = readLines("/inst/extdata/nasa_token.txt"),
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
    ##### 4. chr_input_dir is the file path to the input directory. This target
    #####    controls where the raw data files are downloaded to and imported
    #####    from. This file path **MUST** be mounted to the container at run
    #####    time in the `run_container.sh` script.
    targets::tar_target(
      chr_input_dir,
      command = "/input",
      description = "Input directory"
    )
    ,
    ##### 5. chr_dates_split controls the size of temporal splits. Splitting the
    #####    temporal range into smaller chunks allows for parallel processing
    #####    across multiple workers. It also allows for dispatching new dynamic
    #####    branches when the temporal range is updated.
    targets::tar_target(
      num_dates_split,
      command = 10,
      description = "Number of days to include in each temporal split"
    )
    ############################################################################
    ############################################################################
    ############################################################################
  )