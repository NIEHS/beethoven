target_download <-
  list(
    targets::tar_target(
      dirs,
      command =
        sprintf("dir_input_%s",
          c("aqs", "nei", "narrmono", "narrplevels",
            paste0("modis_",
              c("mod11", "mod13", "mcd19", "mod06", "mod09", "vnp46")
            ),
            "nlcd", "ecoregion", "koppen", "gmted",
            "sedac_population", "sedac_groads",
            "hms", "tri", "geos" # add covariate lists below if necessary
          )
        ),
      iteration = "vector"
    )
    ,
    # each dataset is branched
    targets::tar_target(
      download_raw,
      command = fastdown(mr(dirs)),
      pattern = map(dirs),
      iteration = "vector"
    )
  )
## Status up to here is stored in meta as hash and rds/qs files