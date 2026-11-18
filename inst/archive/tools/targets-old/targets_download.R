target_download <-
  list(
    targets::tar_target(
      dirs,
      command =
        sprintf("dir_input_%s",
          c("aqs", "nei", "narr",
            paste0("modis_",
              c("mod11", "mod13", "mcd19", "mod06", "mod09")
            ),
            "viirs", "nlcd", "ecoregions", "koppen", "gmted",
            "population", "groads",
            "hms", "tri", "geoscf" # add covariate lists below if necessary
          )
        ),
      iteration = "vector"
    )
    ,
    # each dataset is branched
    targets::tar_target(
      download_raw,
      command = feature_raw_download(meta_run(dirs)),
      pattern = map(dirs),
      iteration = "vector"
    )
  )
## Status up to here is stored in meta as hash and rds/qs files
## How do we know if downloaded files were exactly what we expected?