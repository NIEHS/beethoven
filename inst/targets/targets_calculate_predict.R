
## Targets for prediction grid features ####
## FIXME: align with the "_fit" targets
## TODO: chopin's gridset implementation
## TODO: prospective or retrospective, or both?

target_calculate_predict <-
  list(
    targets::tar_target(
      df_pred_calc_grid,
      command = qs::qread(
        list.files(
          path = file.path("inst", "targets"),
          pattern = "prediction_grid.qs",
          full.names = TRUE
        )
      ),
      description = "Import prediction grid"
    ),
    targets::tar_target(
      sf_pred_raw_grid,
      command = sf::st_transform(
        sf::st_as_sf(
          df_pred_calc_grid,
          coords = c("lon", "lat"),
          crs = 5070
        ),
        crs = 4326
      ),
      description = "Prediction grid as sf (no coordinates)"
    ),
    targets::tar_target(
      df_pred_raw_grid,
      command = sf::st_coordinates(sf_pred_raw_grid),
      description = "Prediction grid coordinates as df"
    ),
    targets::tar_target(
      sf_pred_calc_grid,
      command = cbind(
        sf_pred_raw_grid,
        lon = df_pred_raw_grid[, 1],
        lat = df_pred_raw_grid[, 2]
      ),
      description = "Prediction grid as sf (with coordinates)"
    )
    ,
    targets::tar_target(
      list_pred_calc_grid,
      command = base::split(
        sf_pred_calc_grid,
        ceiling(seq_len(nrow(sf_pred_calc_grid)) / 100000)
      ),
      description = "Split prediction grid into list (SAMPLE)"
    )
    ,
    targets::tar_target(
      name = chr_pred_calc_grid,
      command = names(list_pred_calc_grid),
      description = "Names of prediction grid sf objects (SAMPLE)"
    )
    ,
    targets::tar_target(
      list_pred_split_dates,
      command = split_dates(arglist_common$char_period, 50),
      description = "Split period into list of dates"
    )
    ,
    targets::tar_target(
      chr_pred_split_dates,
      command = names(list_pred_split_dates),
      description = "Names of split dates lists"
    )
    ,
    ###########################################################################
    ##### SAMPLE TARGETS FOR DEVELOPMENT #####
    targets::tar_target(
      list_pred_calc_grid_DEV,
      command = base::split(
        sf_pred_calc_grid[500001:700000, ],
        ceiling(seq_len(nrow(sf_pred_calc_grid[500001:700000, ])) / 100000) # sample of 200,000 sites
      ),
      description = "Split prediction grid into list (DEV SAMPLE)"
    )
    ,
    targets::tar_target(
      name = chr_pred_calc_grid_DEV,
      command = names(list_pred_calc_grid_DEV),
      description = "Names of prediction grid sf objects (DEV SAMPLE)"
    )
    ,
    targets::tar_target(
      list_pred_split_dates_DEV,
      command = list_pred_split_dates[1:2], # sample of 100 days
      description = "Split period into list of dates (DEV SAMPLE)"
    )
    ,
    targets::tar_target(
      chr_pred_split_dates_DEV,
      command = names(list_pred_split_dates_DEV),
      description = "Names of split dates lists (DEV SAMPLE)"
    )
    ,
    ###########################################################################
    # # # targets::tar_target(
    # # #   list_pred_split_calc_hms,
    # # #   command = inject_calculate(
    # # #     covariate = "hms",
    # # #     locs = list_pred_calc_grid[[chr_pred_calc_grid]],
    # # #     injection = list(
    # # #       path = loadargs(file_prep_calc_args, "hms")$path,
    # # #       date = fl_dates(list_pred_split_dates[[chr_pred_split_dates]]),
    # # #       covariate = "hms"
    # # #     )
    # # #   ),
    # # #   pattern = cross(
    # # #     chr_pred_split_dates,
    # # #     chr_pred_calc_grid
    # # #   ),
    # # #   iteration = "list",
    # # #   description = "Calculate HMS features with branched sublists",
    # # #   resources = set_slurm_resource(
    # # #     ntasks = 1,
    # # #     ncpus = 4,
    # # #     memory = 8
    # # #   )
    # # # )
    # # # ,
    # # # targets::tar_target(
    # # #   dt_pred_calc_hms,
    # # #   command = reduce_merge(
    # # #     reduce_list(lapply(list_pred_split_calc_hms, function(x) x[[1]]))
    # # #   ),
    # # #   description = "List of HMS features"
    # # # )
    # # # ,
    targets::tar_target(
      chr_iter_pred_features,
      command = chr_iter_calc_features |> base::setdiff(c("hms", "ecoregions")),
      description = "Drop HMS and ecoregions from base features"
    )
    ,
    # targets::tar_target(
    #   list_pred_split_calc_base,
    #   command =
    #     inject_calculate(
    #       covariate = chr_iter_pred_features,
    #       locs = sf_pred_calc_grid,
    #       injection = loadargs(file_prep_calc_args, chr_iter_pred_features)),
    #   pattern = cross(
    #     file_prep_calc_args,
    #     chr_iter_pred_features
    #   ),
    #   iteration = "list",
    #   description = "Calculate base features (pred)",
    #   priority = 1,
    #   resources = set_slurm_resource(
    #     ntasks = 1,
    #     ncpus = 4,
    #     memory = 8
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_pred_calc_eco,
    #   command = inject_calculate(
    #     covariate = "ecoregions",
    #     locs = sf_pred_calc_grid,
    #     injection = loadargs(file_prep_calc_args, "ecoregions")
    #   ),
    #   description = "Calculate ecoregions features (pred)",
    #   resources = set_slurm_resource(
    #     ntasks = 1,
    #     ncpus = 4,
    #     memory = 32
    #   )
    # )
    # ,
    # targets::tar_target(
    #   list_pred_calc_base,
    #   command = list(
    #     lapply(
    #       list(
    #         lapply(list_pred_split_calc_base, function(x) x[[1]]),
    #         list_pred_calc_eco[[1]],
    #         reduce_list(lapply(list_pred_split_calc_hms, function(x) x[[1]]))
    #       ),
    #       function(x) x[[1]]
    #     )
    #   ),
    #   description = "Append HMS to base features (pred)"
    # )
    # ,
    # targets::tar_target(
    #   list_pred_calc_base_flat,
    #   command = lapply(list_pred_calc_base,
    #     function(x) {
    #       if (length(x) == 1) {
    #         x[[1]]
    #       } else if (
    #         sum(grepl("light|medium|heavy",
    #               sapply(x, \(t) names(t)))) == 3) {
    #         xr <- lapply(x, \(dt) {
    #                 dta <- data.table::copy(dt)
    #                 dta <- dta[, time := as.character(time)]
    #                 return(dta)
    #               })
    #         xrr <- Reduce(
    #           function(x, y) {
    #             collapse::join(x, y, on = c("site_id", "time"), how = "full") },
    #           xr)
    #         return(xrr)
    #       } else {
    #         collapse::rowbind(x, use.names = TRUE, fill = TRUE)
    #       }
    #       }),
    #   description = "Calculated base feature list (all dt) (pred)"
    # )
    # ,
    targets::tar_target(
      name = list_pred_split_calc_nlcd,
      command = inject_nlcd(
        year = df_feat_calc_nlcd_params$year,
        radius = df_feat_calc_nlcd_params$radius,
        from = amadeus::process_nlcd(
          path = loadargs(file_prep_calc_args, "nlcd")$path,
          year = df_feat_calc_nlcd_params$year
        ),
        locs = list_pred_calc_grid_DEV[[chr_pred_calc_grid_DEV]],
        locs_id = arglist_common$char_siteid,
        nthreads = 16L,
        mode = "exact",
        max_cells = 3e7
      ),
      pattern = cross(
        file_prep_calc_args,
        df_feat_calc_nlcd_params,
        chr_pred_calc_grid_DEV
      ),
      iteration = "list",
      description = "Calculate NLCD features with branched sublists (pred)",
      resources = set_slurm_resource(
        ntasks = 1,
        ncpus = 8L,
        memory = 32,
        partition = "normal"
      )
    )
    ,
    # targets::tar_target(
    #   name = list_pred_calc_nlcd,
    #   command = reduce_list(list_pred_split_calc_nlcd),
    #   description = "List of NLCD features (pred)"
    # )
    # ,
    # targets::tar_target(
    #   name = dt_pred_calc_nlcd,
    #   command =
    #     list_pred_calc_nlcd %>%
    #       collapse::rowbind(fill = TRUE) %>%
    #       collapse::funique() %>%
    #       collapse::pivot(
    #         ids = c(arglist_common$char_siteid, arglist_common$char_timeid),
    #         values = names(.)[!names(.) %in% c(arglist_common$char_siteid, arglist_common$char_timeid)]
    #       ) %>%
    #       .[!is.na(.[["value"]]),] %>%
    #       collapse::pivot(
    #         ids = c("site_id", "time"),
    #         values = c("value"),
    #         how = "wider"
    #       ),
    #   description = "NLCD feature list (all dt) (pred)"
    # )
    # ,
    targets::tar_target(
      list_pred_split_calc_nasa,
      command =
        inject_modis_par(
          locs = list_pred_calc_grid_DEV[[chr_pred_calc_grid_DEV]],
          injection = loadargs(file_prep_calc_args, chr_iter_calc_nasa)
        ),
      pattern = cross(
        file_prep_calc_args,
        chr_iter_calc_nasa,
        chr_pred_calc_grid_DEV
      ),
      resources = set_slurm_resource(
            ntasks = 1, ncpus = arglist_common$nthreads_nasa, memory = 8
          ),
      iteration = "list",
      description = "Calculate MODIS/VIIRS features with branched sublists (pred)"
    )
    # ,
    # targets::tar_target(
    #   name = list_pred_calc_nasa,
    #   command = reduce_list(list_pred_split_calc_nasa),
    #   description = "List of MODIS/VIIRS features (pred)"
    # )
    # ,
    # targets::tar_target(
    #   dt_pred_calc_nasa,
    #   command = reduce_merge(list_pred_calc_nasa),
    #   description = "data.table of MODIS/VIIRS features (pred)"
    # )
    # ,
    # # # targets::tar_target(
    # # #   list_pred_split_calc_geoscfaqc,
    # # #   command = inject_geos(
    # # #     locs = list_pred_calc_grid[[chr_pred_calc_grid]],
    # # #     injection = list(
    # # #       path = loadargs(file_prep_calc_args, chr_iter_calc_geoscf[2])$path,
    # # #       date = fl_dates(list_pred_split_dates[[chr_pred_split_dates]]),
    # # #       nthreads = 4
    # # #     )
    # # #   ),
    # # #   pattern = cross(
    # # #     chr_pred_split_dates,
    # # #     chr_pred_calc_grid
    # # #   ),
    # # #   iteration = "list",
    # # #   resources = set_slurm_resource(
    # # #     ntasks = 1,
    # # #     ncpus = 4,
    # # #     memory = 8
    # # #   ),
    # # #   description = "Calculate GEOS-CF (aqc_tavg_1hr_g1440x721_v1) features with branched sublists"
    # # # )
    # # # ,
    # targets::tar_target(
    #   list_pred_split_calc_geoscfchm,
    #   command = inject_geos(
    #     locs = list_pred_calc_grid[[chr_pred_calc_grid]],
    #     injection = list(
    #       path = loadargs(file_prep_calc_args, chr_iter_calc_geoscf[1])$path,
    #       date = fl_dates(list_pred_split_dates[[chr_pred_split_dates]]),
    #       nthreads = 4
    #     )
    #   ),
    #   pattern = cross(
    #     chr_pred_split_dates,
    #     chr_pred_calc_grid
    #   ),
    #   iteration = "list",
    #   resources = set_slurm_resource(
    #     ntasks = 1,
    #     ncpus = 4,
    #     memory = 20,
    #     partition = "highmem"
    #   ),
    #   description = "Calculate GEOS-CF (chm_tavg_1hr_g1440x721_v1) features with branched sublists"
    # )
    # ,
    # targets::tar_target(
    #   dt_pred_calc_geoscf,
    #   command = reduce_merge(
    #     list(
    #       reduce_list(list_pred_split_calc_geoscfaqc)[[1]],
    #       reduce_list(list_pred_split_calc_geoscfchm)[[1]]
    #     )
    #   ),
    #   description = "data.table of GEOS-CF features (pred)"
    # )
    # ,
    # # # targets::tar_target(
    # # #   chr_pred_calc_gmted_radii,
    # # #   command = c(0, 1e3, 1e4, 5e4),
    # # #   description = "Radii for GMTED features"
    # # # )
    # # # ,
    # # # targets::tar_target(
    # # #   name = list_pred_split_calc_gmted,
    # # #   command = inject_gmted(
    # # #     locs = list_pred_calc_grid[[chr_pred_calc_grid]],
    # # #     variable = chr_iter_calc_gmted_vars,
    # # #     radii = chr_pred_calc_gmted_radii,
    # # #     injection = loadargs(file_prep_calc_args, "gmted")
    # # #   ),
    # # #   iteration = "list",
    # # #   pattern = cross(
    # # #     file_prep_calc_args,
    # # #     chr_iter_calc_gmted_vars,
    # # #     chr_pred_calc_grid,
    # # #     chr_pred_calc_gmted_radii
    # # #   ),
    # # #   resources = set_slurm_resource(
    # # #     ntasks = 1,
    # # #     ncpus = arglist_common$nthreads_gmted,
    # # #     memory = 8
    # # #   ),
    # # #   description = "Calculate GMTED features with branched sublists (pred)"
    # # # )
    # # # ,
    # # # targets::tar_target(
    # # #   dt_pred_calc_gmted,
    # # #   command = reduce_merge(
    # # #     reduce_list(list_pred_split_calc_gmted),
    # # #     "site_id"
    # # #   ),
    # # #   description = "data.table of GMTED features (pred)"
    # # # )
    # # # ,
    # # # targets::tar_target(
    # # #   chr_pred_calc_narrmono,
    # # #   command = loadargs(file_prep_calc_args, "narr")$domain %>%
    # # #     dplyr::setdiff(c("omega", "shum")),
    # # #   description = "NARR domain (monolevel)"
    # # # )
    # # # ,
    # # # targets::tar_target(
    # # #   name = list_pred_split_calc_narrmono,
    # # #   command =
    # # #     par_narr(
    # # #       domain = chr_pred_calc_narrmono,
    # # #       path = loadargs(file_prep_calc_args, "narr")$path,
    # # #       date = fl_dates(list_pred_split_dates[[chr_pred_split_dates]]),
    # # #       locs = list_pred_calc_grid[[chr_pred_calc_grid]],
    # # #       nthreads = 4
    # # #     )
    # # #   ,
    # # #   pattern = cross(
    # # #     chr_pred_calc_narrmono,
    # # #     chr_pred_split_dates,
    # # #     chr_pred_calc_grid
    # # #   ),
    # # #   iteration = "list",
    # # #   resources = set_slurm_resource(
    # # #     ntasks = 1,
    # # #     ncpus = 4,
    # # #     memory = 4
    # # #   ),
    # # #   description = "Calculate NARR features with branched sublists (monolevel) (pred)"
    # # # )
    # # # ,
    # # # targets::tar_target(
    # # #   chr_pred_calc_narrpres,
    # # #   command = c("omega", "shum"),
    # # #   description = "NARR domain (pressure)"
    # # # )
    # ,
    # targets::tar_target(
    #   name = list_pred_split_calc_narrpres,
    #   command =
    #     par_narr(
    #       domain = chr_pred_calc_narrpres,
    #       path = loadargs(file_prep_calc_args, "narr")$path,
    #       date = fl_dates(list_pred_split_dates[[chr_pred_split_dates]]),
    #       locs = list_pred_calc_grid[[chr_pred_calc_grid]],
    #       nthreads = 10
    #     )
    #   ,
    #   pattern = cross(
    #     chr_pred_calc_narrpres,
    #     chr_pred_split_dates,
    #     chr_pred_calc_grid
    #   ),
    #   iteration = "list",
    #   resources = set_slurm_resource(
    #     ntasks = 1,
    #     ncpus = 10,
    #     memory = 120,
    #     partition = "highmem"
    #   ),
    #   description = "Calculate NARR features with branched sublists (pressure level) (pred)"
    # )
    # ,
    # targets::tar_target(
    #   dt_pred_calc_narr,
    #   command = reduce_merge(
    #     lapply(
    #       list(list_pred_split_calc_narrmono, list_pred_split_calc_narrpres),
    #       function(x) reduce_merge(reduce_list(lapply(x, "[[", 1)))
    #     ),
    #     by = c("site_id", "time")
    #   ),
    #   description = "data.table of NARR features (pred)"
    # )
    # ,
    # targets::tar_target(
    #   dt_pred_calc_date,
    #   command = 
    #   Reduce(
    #     post_calc_autojoin,
    #     list(
    #       dt_pred_calc_narr,
    #       dt_pred_calc_geoscf,
    #       dt_pred_calc_hms
    #       # dt_pred_calc_nasa
    #     )
    #   ),
    #   description = "data.table of all daily features (pred)"
    # )
    # ,
    # targets::tar_target(
    #   dt_pred_calc_base,
    #   command = 
    #   Reduce(
    #     post_calc_autojoin,
    #     c(
    #       list_pred_calc_base_flat,
    #       list(dt_pred_calc_gmted),
    #       list(dt_pred_calc_nlcd)
    #     )
    #   ),
    #   description = "Base features (pred - no PM2.5)"
    # )
    # ,
    # targets::tar_target(
    #   dt_pred_calc_design,
    #   command =
    #     post_calc_autojoin(
    #       dt_pred_calc_base,
    #       dt_pred_calc_date,
    #       year_start = as.integer(substr(arglist_common$char_period[1], 1, 4)),
    #       year_end = as.integer(substr(arglist_common$char_period[2], 1, 4))
    #     ),
    #   description = "data.table of all features (pred - no PM2.5)"
    # )
  )
