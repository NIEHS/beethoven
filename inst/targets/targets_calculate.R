
# meta_run, resting in the tools/pipeline/pipeline_base_functions.R,
# is a function that returns a list of parameters for the pipeline
# for users' convenience and make the pipeline less prone to errors.

target_calculate_fit <-
  list(
    tarchetypes::tar_files_input(
      name = file_prep_calc_args,
      files = list.files("inst/targets", pattern = "^calc*.*.qs$", full.names = TRUE),
      # cue = tar_invalidate(tar_older(Sys.time() - as.difftime(4, units = "weeks"))),
      format = "file",
      iteration = "vector",
      description = "Calculation arguments in QS file"
    )
    ,
    tar_target(
      chr_iter_calc_features,
      command = c("hms", "tri", "nei",
                  "ecoregions", "koppen", "population", "groads"),
      iteration = "list",
      description = "Feature calculation"
    )
    ,
    # "year" is included: tri, nlcd, nei
    # "time" is included: hms
    tar_target(
      chr_iter_calc_nasa,
      command = c(
         "mod11", "mod06", "mod13",
         "mcd19_1km", "mcd19_5km", "mod09", "viirs"
          ),
      iteration = "list",
      description = "MODIS/VIIRS feature calculation"
    )
    ,
    tar_target(
      chr_iter_calc_geoscf,
      command = c("geoscf_chm", "geoscf_aqc"),
      iteration = "vector",
      description = "GEOS-CF feature calculation"
    )
    ,
    tar_target(
      name = chr_iter_calc_gmted_vars,
      command = c(
          "Breakline Emphasis", "Systematic Subsample",
          "Median Statistic", "Minimum Statistic",
          "Mean Statistic", "Maximum Statistic",
          "Standard Deviation Statistic"
        ),
      iteration = "list",
      description = "GMTED variables"
    )
    ,
    tar_target(
      list_feat_calc_base,
      command =
        inject_calculate(
          covariate = chr_iter_calc_features,
          locs = sf_feat_proc_aqs_sites,
          injection = loadargs(file_prep_calc_args, chr_iter_calc_features)),
      pattern = cross(file_prep_calc_args, chr_iter_calc_features),
      iteration = "list",
      description = "Base feature list",
      priority = 1
    )
    ,
    tar_target(
      list_feat_calc_base_flat,
      command = lapply(list_feat_calc_base,
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
      description = "Base feature list (all dt)"
    )
    ,
    tar_target(
      name = df_feat_calc_nlcd_params,
      command = expand.grid(
        year = loadargs(file_prep_calc_args, "nlcd")$domain,
        radius = loadargs(file_prep_calc_args, "nlcd")$radius
      ) %>%
      split(1:nrow(.)),
      iteration = "list"
    )
    ,
    tar_target(
      name = list_feat_calc_nlcd,
      command = inject_nlcd(year = df_feat_calc_nlcd_params$year,
                            radius = df_feat_calc_nlcd_params$radius,
                            from = amadeus::process_nlcd(
                              path = loadargs(file_prep_calc_args, "nlcd")$path,
                              year = df_feat_calc_nlcd_params$year
                            ),
                            locs = sf_feat_proc_aqs_sites,
                            locs_id = arglist_common$char_siteid,
                            nthreads = 10L,
                            mode = "exact",
                            max_cells = 3e7
                            ),
      pattern = cross(file_prep_calc_args, df_feat_calc_nlcd_params),
      iteration = "list",
      description = "NLCD feature list",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = 10, memory = 8
          )
    )
    ,
    tar_target(
      name = dt_feat_calc_nlcd,
      command =
        list_feat_calc_nlcd %>%
          collapse::rowbind(fill = TRUE) %>%
          collapse::funique() %>%
          collapse::pivot(
            ids = c(arglist_common$char_siteid, arglist_common$char_timeid),
            values = names(.)[!names(.) %in% c(arglist_common$char_siteid, arglist_common$char_timeid)]
          ) %>%
          .[!is.na(.[["value"]]),] %>%
          collapse::pivot(
            ids = c("site_id", "time"),
            values = c("value"),
            how = "wider"
          ),
      description = "NLCD feature list (all dt)"
    )
    ,
    tar_target(
      list_feat_calc_nasa,
      command =
        inject_modis_par(
          locs = sf_feat_proc_aqs_sites,
          injection = loadargs(file_prep_calc_args, chr_iter_calc_nasa)),
      pattern = cross(file_prep_calc_args, chr_iter_calc_nasa),
      resources = set_slurm_resource(
            ntasks = 1, ncpus = arglist_common$nthreads_nasa, memory = 8
          ),
      iteration = "list",
      description = "MODIS/VIIRS feature list"
    )
    ,
    tar_target(
      list_feat_calc_geoscf,
      inject_geos(
        locs = sf_feat_proc_aqs_sites,
        injection = loadargs(file_prep_calc_args, chr_iter_calc_geoscf)
      ),
      pattern = cross(file_prep_calc_args, chr_iter_calc_geoscf),
      iteration = "list",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = arglist_common$nthreads_geoscf, memory = 4
          ),
      description = "GEOS-CF feature list"
    )
    ,
    tar_target(
      name = list_feat_calc_gmted,
      command = inject_gmted(
        locs = sf_feat_proc_aqs_sites,
        variable = chr_iter_calc_gmted_vars,
        radii = c(0, 1e3, 1e4, 5e4),
        injection = loadargs(file_prep_calc_args, "gmted")
      ),
      iteration = "list",
      pattern = cross(file_prep_calc_args, chr_iter_calc_gmted_vars),
      resources = set_slurm_resource(
            ntasks = 1, ncpus = arglist_common$nthreads_gmted, memory = 8
          ),
      description = "GMTED feature list"
    )
    ,
    targets::tar_target(
      name = list_feat_calc_narr,
      command = #rlang::inject(
        par_narr(
          domain = loadargs(file_prep_calc_args, "narr")$domain,
          path = loadargs(file_prep_calc_args, "narr")$path,
          date = arglist_common$char_period,
          locs = sf_feat_proc_aqs_sites,
          nthreads = arglist_common$nthreads_narr
        )
      ,
      pattern = map(file_prep_calc_args),
      iteration = "list",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = arglist_common$nthreads_narr, memory = 20 
          )
    )
    ,
    # targets::tar_target(
    #   name = list_feat_calc_narr_apptainer,
    #   command = #rlang::inject(
    #     par_narr_appt(
    #       domain = loadargs(file_prep_calc_args, "narr")$domain_appt,
    #       period = arglist_common$char_period
    #     ),
    #   pattern = map(file_prep_calc_args),
    #   iteration = "list",
    #   resources = set_slurm_resource(
    #         ntasks = 1, ncpus = 2, memory = 40 
    #       )
    # )
    # ,
    tar_target(
      dt_feat_calc_gmted,
      command = reduce_merge(list_feat_calc_gmted, "site_id"),
      description = "data.table of GMTED features"
    )
    ,
    tar_target(
      dt_feat_calc_nasa,
      command = reduce_merge(list_feat_calc_nasa),
      description = "data.table of MODIS/VIIRS features"
    )
    ,
    tar_target(
      dt_feat_calc_geoscf,
      command = reduce_merge(list_feat_calc_geoscf),
      description = "data.table of GEOS-CF features"
    )
    ,
    tar_target(
      dt_feat_calc_narr,
      command =
        # collapse::join(
          reduce_merge(list_feat_calc_narr, by = NULL),
        #   reduce_merge(list_feat_calc_narr_apptainer, by = NULL),
        #   on = c("site_id", "time"),
        #   how = "full"
        # )
    )
    ,
    tar_target(
      dt_feat_calc_date,
      command = 
      Reduce(
        post_calc_autojoin,
        list(
          dt_feat_calc_narr,
          dt_feat_calc_geoscf,
          dt_feat_calc_nasa
        )
      ),
      description = "data.table of all daily features"
    )
    ,
    tar_target(
      dt_feat_calc_base,
      command = 
      Reduce(
        post_calc_autojoin,
        c(
          list(dt_feat_proc_aqs_sites_time),
          list_feat_calc_base_flat,
          list(dt_feat_calc_gmted)
        )
      ),
      description = "Base features with PM2.5"
    )
    ,
    targets::tar_target(
      dt_feat_calc_design,
      command =
        post_calc_autojoin(
          dt_feat_calc_base,
          dt_feat_calc_date,
          year_start = as.integer(substr(arglist_common$char_period[1], 1, 4)),
          year_end = as.integer(substr(arglist_common$char_period[2], 1, 4))
        ) %>%
        post_calc_autojoin(
          df_coarse = dt_feat_calc_nlcd,
          year_start = as.integer(substr(arglist_common$char_period[1], 1, 4)),
          year_end = as.integer(substr(arglist_common$char_period[2], 1, 4))),
      description = "data.table of all features with PM2.5"
    )
    # ,
    # tar_target(
    #   dt_feat_fit_pm,
    #   post_calc_join_pm25_features(
    #     df_pm = sf_feat_proc_aqs_pm25,
    #     df_covar = dt_feat_fit_x,
    #     locs_id = "site_id",
    #     time_id = "time"
    #   ),
    #   description = "data.table of all features with PM2.5"
    # )
    ,
    tar_target(
      dt_feat_calc_cumulative,
      command = append_predecessors(
        path_qs = "output/qs",
        period_new = arglist_common$char_period,
        input_new = dt_feat_calc_design,
        nthreads = arglist_common$nthreads_append
      ),
      description = "Cumulative feature calculation",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = arglist_common$nthreads_append, memory = 16
          )
    ),
    tar_target(
      dt_feat_calc_imputed,
      command =
      impute_all(
        dt_feat_calc_cumulative,
        period = arglist_common$char_period,
        nthreads_dt = arglist_common$nthreads_impute,
        nthreads_collapse = arglist_common$nthreads_impute,
        nthreads_imputation = arglist_common$nthreads_impute),
      description = "Imputed features + lags",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = arglist_common$nthreads_impute, memory = 8
          )
    )
  )




## Targets for prediction grid features ####
## FIXME: align with the "_fit" targets
## TODO: chopin's gridset implementation
## TODO: prospective or retrospective, or both?
target_calculate_predict <-
  list(
    tar_target(
      char_pred_calc_base,
      command =
        terra::writeVector(
          terra::vect(
            data(prediction_grid, package = "chopin"),
            geom = c("lon", "lat"),
            crs = "EPSG:5070"
          ),
          filename = "input/prediction_grid.gpkg"
        )
    ),
    tar_target(
      sf_pred_calc_grid,
      command = chopin::par_make_gridset(

      )
    ),
    tar_target(
      dt_pred_calc_base,
      command = chopin::par_grid(

      )
    )
  )
