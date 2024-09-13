
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
        lon = df_pred_raw_grid,
        lat = df_pred_raw_grid
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
      description = "Split prediction grid into list"
    )
    ,
    targets::tar_target(
      name = vect_pred_calc_grid,
      command = names(list_pred_calc_grid),
      description = "Names of prediction grid sf objects"
    )
    ,
    targets::tar_target(
      list_pred_calc_base,
      command =
        inject_calculate(
          covariate = chr_iter_calc_features,
          locs = sf_pred_calc_grid,
          injection = loadargs(file_prep_calc_args, chr_iter_calc_features)),
      pattern = cross(file_prep_calc_args, chr_iter_calc_features),
      iteration = "list",
      description = "Calculate base features (pred)",
      priority = 1
    )
    ,
    targets::tar_target(
      list_pred_calc_base_flat,
      command = lapply(list_pred_calc_base,
        function(x) {
          if (length(x) == 1) {
            x[[1]]
          } else if (
            sum(grepl("light|medium|heavy",
                  sapply(x, \(t) names(t)))) == 3) {
            xr <- lapply(x, \(dt) {
                    dta <- data.table::copy(dt)
                    dta <- dta[, time := as.character(time)]
                    return(dta)
                  })
            xrr <- Reduce(
              function(x, y) {
                collapse::join(x, y, on = c("site_id", "time"), how = "full") },
              xr)
            return(xrr)
          } else {
            collapse::rowbind(x, use.names = TRUE, fill = TRUE)
          }
          }),
      description = "Calculated base feature list (all dt) (pred)"
    )
    ,
    targets::tar_target(
      name = list_pred_split_calc_nlcd,
      command = inject_nlcd(
        year = df_feat_calc_nlcd_params$year,
        radius = df_feat_calc_nlcd_params$radius,
        from = amadeus::process_nlcd(
          path = loadargs(file_prep_calc_args, "nlcd")$path,
          year = df_feat_calc_nlcd_params$year
        ),
        locs = list_pred_calc_grid[[vect_pred_calc_grid]],
        locs_id = arglist_common$char_siteid,
        nthreads = 10L,
        mode = "exact",
        max_cells = 3e7
      ),
      pattern = cross(
        file_prep_calc_args,
        df_feat_calc_nlcd_params,
        vect_pred_calc_grid
      ),
      iteration = "list",
      description = "Calculate NLCD features with branched sublists (pred)",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = 10, memory = 8
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
          locs = list_pred_calc_grid[[vect_pred_calc_grid]],
          injection = loadargs(file_prep_calc_args, chr_iter_calc_nasa)
        ),
      pattern = cross(
        file_prep_calc_args,
        chr_iter_calc_nasa,
        vect_pred_calc_grid
      ),
      resources = set_slurm_resource(
            ntasks = 1, ncpus = arglist_common$nthreads_nasa, memory = 8
          ),
      iteration = "list",
      description = "Calculate MODIS/VIIRS features with branched sublists (pred)"
    )
    ,
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
    targets::tar_target(
      list_pred_split_calc_geoscf,
      command = inject_geos(
        locs = list_pred_calc_grid[[vect_pred_calc_grid]],
        injection = loadargs(file_prep_calc_args, chr_iter_calc_geoscf)
      ),
      pattern = cross(
        file_prep_calc_args,
        chr_iter_calc_geoscf,
        vect_pred_calc_grid
      ),
      iteration = "list",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = arglist_common$nthreads_geoscf, memory = 8
          ),
      description = "Calculate GEOS-CF features with branched sublists (pred)"
    )
    ,
    # targets::tar_target(
    #   name = list_pred_calc_geoscf,
    #   command = reduce_list(list_pred_split_calc_geoscf),
    #   description = "List of GEOS-CF features (pred)"
    # )
    # ,
    # targets::tar_target(
    #   dt_pred_calc_geoscf,
    #   command = reduce_merge(list_pred_calc_geoscf),
    #   description = "data.table of GEOS-CF features (pred)"
    # )
    # ,
    targets::tar_target(
      name = list_pred_split_calc_gmted,
      command = inject_gmted(
        locs = list_pred_calc_grid[[vect_pred_calc_grid]],
        variable = chr_iter_calc_gmted_vars,
        radii = c(0, 1e3, 1e4, 5e4),
        injection = loadargs(file_prep_calc_args, "gmted")
      ),
      iteration = "list",
      pattern = cross(
        file_prep_calc_args,
        chr_iter_calc_gmted_vars,
        vect_pred_calc_grid
      ),
      resources = set_slurm_resource(
        ntasks = 1,
        ncpus = arglist_common$nthreads_gmted,
        memory = 8
      ),
      description = "Calculate GMTED features with branched sublists (pred)"
    )
    ,
    # targets::tar_target(
    #   name = list_pred_calc_gmted,
    #   command = reduce_list(list_pred_split_calc_gmted),
    #   description = "List of GMTED features (pred)"
    # )
    # ,
    # targets::tar_target(
    #   dt_pred_calc_gmted,
    #   command = reduce_merge(list_pred_calc_gmted, "site_id"),
    #   description = "data.table of GMTED features (pred)"
    # )
    # ,
    targets::tar_target(
      name = list_pred_calc_narr,
      command =
        par_narr(
          domain = loadargs(file_prep_calc_args, "narr")$domain,
          path = loadargs(file_prep_calc_args, "narr")$path,
          date = arglist_common$char_period,
          locs = list_pred_calc_grid[[vect_pred_calc_grid]],
          nthreads = arglist_common$nthreads_narr
        )
      ,
      pattern = cross(file_prep_calc_args, vect_pred_calc_grid),
      iteration = "list",
      resources = set_slurm_resource(
        ntasks = 1, ncpus = arglist_common$nthreads_narr, memory = 20
      ),
      description = "Calculate NARR features with branched sublists (pred)"
    )
    # ,
    # targets::tar_target(
    #   dt_pred_calc_narr,
    #   command = do.call(
    #     rbind,
    #     lapply(
    #       list_pred_calc_narr,
    #       function(x) reduce_merge(x, by = c("site_id", "time"))
    #     )
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
