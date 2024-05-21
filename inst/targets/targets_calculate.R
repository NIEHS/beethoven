
# meta_run, resting in the tools/pipeline/pipeline_base_functions.R,
# is a function that returns a list of parameters for the pipeline
# for users' convenience and make the pipeline less prone to errors.

target_calculate_fit <-
  list(
    tarchetypes::tar_files_input(
      name = file_prep_calc_args,
      files = list.files("inst/targets", pattern = "*.*.rds$", full.names = TRUE),
      cue = tar_invalidate(any_of(tar_older(Sys.time() - as.difftime(4, units = "weeks")))),
      format = "file",
      iteration = "vector",
      description = "Calculation arguments in RDS file"
    )
    ,
    tar_target(
      chr_iter_calc_features,
      command = c("hms", "nlcd", "tri", "nei",
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
            xrr <- reduce_merge(xr)
            return(xrr)
          } else {
            data.table::rbindlist(x, use.names = TRUE, fill = TRUE)
          }
          }),
      description = "Base feature list (all dt)"
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
            ntasks = 1, ncpus = 20, memory = 8
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
            ntasks = 1, ncpus = 10, memory = 4
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
            ntasks = 1, ncpus = 4, memory = 8
          ),
      description = "GMTED feature list"
    )
    ,
    targets::tar_target(
      name = list_feat_calc_narr,
      command = #rlang::inject(
        par_narr(
          domain = loadargs(file_prep_calc_args, "narr")$domain,
          date = arglist_common$char_period,
          locs = sf_feat_proc_aqs_sites,
          nthreads = 12L 
        )
      ,
      pattern = map(file_prep_calc_args),
      iteration = "list",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = 12, memory = 16
          )
    )
    ,
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
      command = reduce_merge(list_feat_calc_narr, by = NULL)
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
          dt_feat_calc_date
        ),
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
        nthreads = 8L
      ),
      description = "Cumulative feature calculation",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = 8, memory = 100
          )
    ),
    tar_target(
      dt_feat_calc_imputed,
      command = impute_all(dt_feat_calc_cumulative),
      description = "Imputed features + lags",
      resources = set_slurm_resource(
            ntasks = 1, ncpus = 8, memory = 100
          )
    )
  # TODO: compute lagged variables
  )




## Targets for prediction grid features ####
## FIXME: align with the "_fit" targets
## TODO: chopin's gridset implementation
target_calculate_predict <-
  list(

  )