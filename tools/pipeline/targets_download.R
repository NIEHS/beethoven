target_download <-
	list(
    targets::tar_target(
        dirs,
        command = sprintf("dir_input_%s",
                c("aqs", "nei", "narrmono", "narrplevels",
                paste0("modis_",
                    c("mod11", "mod13", "mcd19", "mod06", "mod09", "vnp46")
                ),
                "nlcd", "ecoregion", "koppen", "gmted",
                "sedac_population", "sedac_groads",
                "hms", "tri", "geos")),
        iteration = "vector"
    )
    ,
    targets::tar_target(
        download_raw,
        command = fastdown(mr(dirs)),
        pattern = map(dirs),
        iteration = "vector"
    )
    ,
    targets::tar_target(
        file_target,
        command = download_raw
    )
	)