target_download <-
  list(
    tarchetypes::tar_files_input(
      name = file_prep_download_args,
      files = list.files("inst/targets", pattern = "download_spec.qs$", full.names = TRUE),
      # cue = tar_invalidate(tar_older(Sys.time() - as.difftime(4, units = "weeks"))),
      format = "file",
      iteration = "vector",
      description = "Download arguments in QS file"
    )
    ,
    targets::tar_target(
      char_rawdir_download,
      command =
        sprintf("dir_input_%s",
          c("aqs", "nei", "narr_monolevel", "narr_p_levels",
            paste0("modis_",
              c("mod11", "mod13", "mcd19", "mod06", "mod09")
            ),
            "viirs", "nlcd", "ecoregions", "koppen", "gmted",
            "population", "groads",
            "hms", "tri", "geoscf_chm", "geoscf_aqc" # add covariate lists below if necessary
          )
        ),
      iteration = "vector"
    )
    ,
    # each dataset is branched
    targets::tar_target(
      lgl_rawdir_download,
      command =
      feature_raw_download(
        path = file_prep_download_args,
        dataset_name = char_rawdir_download),
      pattern = map(file_prep_download_args, char_rawdir_download),
      iteration = "list"
    )
  )
## Status up to here is stored in meta as hash and rds/qs files
## How do we know if downloaded files were exactly what we expected?