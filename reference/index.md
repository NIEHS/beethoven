# Package index

### Utility

Cleaning and adjusting data structure for succinct pipeline

- [`attach_pred()`](https://niehs.github.io/beethoven/reference/attach_pred.md)
  : Combine base learner prediction values with testing data.
- [`attach_xy()`](https://niehs.github.io/beethoven/reference/attach_xy.md)
  : Attach XY coordinates to a data frame
- [`feature_raw_download()`](https://niehs.github.io/beethoven/reference/feature_raw_download.md)
  : Check file status and download if necessary
- [`fl_dates()`](https://niehs.github.io/beethoven/reference/fl_dates.md)
  : Extract the first and last elements of a list
- [`load_modis_files()`](https://niehs.github.io/beethoven/reference/load_modis_files.md)
  : Load MODIS files from a specified path.
- [`loadargs()`](https://niehs.github.io/beethoven/reference/loadargs.md)
  : Load arguments from the formatted argument list file
- [`read_locs()`](https://niehs.github.io/beethoven/reference/read_locs.md)
  : Read AQS data
- [`read_paths()`](https://niehs.github.io/beethoven/reference/read_paths.md)
  : Read paths from a directory with a specific file extension
- [`reduce_list()`](https://niehs.github.io/beethoven/reference/reduce_list.md)
  : Combine dynamically branched sublists based on common column names
- [`set_args_calc()`](https://niehs.github.io/beethoven/reference/set_args_calc.md)
  : Set arguments for the calculation process
- [`set_args_download()`](https://niehs.github.io/beethoven/reference/set_args_download.md)
  : Generate argument list for raw data download
- [`set_target_years()`](https://niehs.github.io/beethoven/reference/set_target_years.md)
  : Set which years to be processed
- [`split_dates()`](https://niehs.github.io/beethoven/reference/split_dates.md)
  : Split a date range into subranges
- [`sys_beethoven()`](https://niehs.github.io/beethoven/reference/sys_beethoven.md)
  : Configure Library Paths and Environment Variables for Beethoven
  Workflow
- [`unmarshal_function()`](https://niehs.github.io/beethoven/reference/unmarshal_function.md)
  : Unmarshal functions

### Calculation

Main calculation functions for branching, parallel processing, and
parameter injection

- [`calc_geos_strict()`](https://niehs.github.io/beethoven/reference/calc_geos_strict.md)
  : Process atmospheric composition data by chunks
- [`calc_gmted_direct()`](https://niehs.github.io/beethoven/reference/calc_gmted_direct.md)
  : Reflown gmted processing
- [`calc_narr2()`](https://niehs.github.io/beethoven/reference/calc_narr2.md)
  : Calculate aggregated values for specified locations
- [`calc_tri_mod()`](https://niehs.github.io/beethoven/reference/calc_tri_mod.md)
  : Modified isotropic Sum of Exponentially Decaying Contributions
  (SEDC) covariates
- [`calculate()`](https://niehs.github.io/beethoven/reference/calculate.md)
  : Spatiotemporal covariate calculation
- [`calculate_modis()`](https://niehs.github.io/beethoven/reference/calculate_modis.md)
  : Calculate MODIS product covariates in multiple CPU threads
- [`calculate_modis_direct()`](https://niehs.github.io/beethoven/reference/calculate_modis_direct.md)
  : Calculate MODIS from preprocessed file
- [`calculate_spacetime()`](https://niehs.github.io/beethoven/reference/calculate_spacetime.md)
  : Generate spatial and temporal covariates at sf locations
- [`export_tif()`](https://niehs.github.io/beethoven/reference/export_tif.md)
  : Export preprocessed GeoTIFF file of one day MODIS product files
- [`inject_calculate()`](https://niehs.github.io/beethoven/reference/inject_calculate.md)
  : Injects the calculate function with specified arguments.
- [`inject_geos()`](https://niehs.github.io/beethoven/reference/inject_geos.md)
  : Injects geographic information into a data frame
- [`inject_gmted()`](https://niehs.github.io/beethoven/reference/inject_gmted.md)
  : Injects GMTED data into specified locations
- [`inject_match()`](https://niehs.github.io/beethoven/reference/inject_match.md)
  : Injects the calculate function with matched arguments.
- [`inject_modis()`](https://niehs.github.io/beethoven/reference/inject_modis.md)
  : Injects arguments into MODIS/VIIRS data processing function
- [`inject_modis_par()`](https://niehs.github.io/beethoven/reference/inject_modis_par.md)
  : Injects arguments to parallelize MODIS/VIIRS data processing
- [`inject_nlcd()`](https://niehs.github.io/beethoven/reference/inject_nlcd.md)
  : Inject arguments into NLCD calculation function for branching
- [`par_narr()`](https://niehs.github.io/beethoven/reference/par_narr.md)
  : Parallelize NARR feature calculation
- [`process_counties()`](https://niehs.github.io/beethoven/reference/process_counties.md)
  : Load county sf object
- [`process_geos_bulk()`](https://niehs.github.io/beethoven/reference/process_geos_bulk.md)
  : Process atmospheric composition data by chunks (v2)
- [`process_narr2()`](https://niehs.github.io/beethoven/reference/process_narr2.md)
  : Process NARR Data (v2)
- [`query_modis_files()`](https://niehs.github.io/beethoven/reference/query_modis_files.md)
  : Identify MODIS files
- [`sum_edc_mod()`](https://niehs.github.io/beethoven/reference/sum_edc_mod.md)
  : Calculate isotropic Sum of Exponentially Decaying Contributions
  (SEDC) covariates

### Post-calculation

Merging calculation results

- [`add_time_col()`](https://niehs.github.io/beethoven/reference/add_time_col.md)
  : Add Time Column
- [`append_predecessors()`](https://niehs.github.io/beethoven/reference/append_predecessors.md)
  : Append Predecessors
- [`impute_all()`](https://niehs.github.io/beethoven/reference/impute_all.md)
  : Impute missing values and attach lagged features
- [`post_calc_autojoin()`](https://niehs.github.io/beethoven/reference/post_calc_autojoin.md)
  : Automatic joining by the time and spatial identifiers
- [`post_calc_cols()`](https://niehs.github.io/beethoven/reference/post_calc_cols.md)
  : Post-calculation column renaming
- [`post_calc_convert_time()`](https://niehs.github.io/beethoven/reference/post_calc_convert_time.md)
  : Convert time column to character
- [`post_calc_df_year_expand()`](https://niehs.github.io/beethoven/reference/post_calc_df_year_expand.md)
  : Expand a data frame by year
- [`post_calc_drop_cols()`](https://niehs.github.io/beethoven/reference/post_calc_drop_cols.md)
  : Remove columns from a data frame based on regular expression
  patterns. This function removes columns from a data frame that match
  any of the specified regular expression patterns. By default, it
  removes columns with names that match the patterns
  "^lon\$\|^lat\$\|geoid\|year\$\|description".
- [`post_calc_join_yeardate()`](https://niehs.github.io/beethoven/reference/post_calc_join_yeardate.md)
  : Join a data.frame with a year-only date column to that with a full
  date column
- [`post_calc_merge_all()`](https://niehs.github.io/beethoven/reference/post_calc_merge_all.md)
  : Merge spatial and spatiotemporal covariate data
- [`post_calc_merge_features()`](https://niehs.github.io/beethoven/reference/post_calc_merge_features.md)
  : Merge input data.frame objects
- [`post_calc_pca()`](https://niehs.github.io/beethoven/reference/post_calc_pca.md)
  : Post-calculation Principal Component Analysis
- [`post_calc_unify_timecols()`](https://niehs.github.io/beethoven/reference/post_calc_unify_timecols.md)
  : Change time column name
- [`post_calc_year_expand()`](https://niehs.github.io/beethoven/reference/post_calc_year_expand.md)
  : Map the available raw data years over the given period
- [`reduce_merge()`](https://niehs.github.io/beethoven/reference/reduce_merge.md)
  : Reduce and merge a list of data tables
- [`reduce_merge_iter()`](https://niehs.github.io/beethoven/reference/reduce_merge_iter.md)
  : Reduce and merge a list of data tables

### Base learner

Fitting base learner and helper functions

- [`assign_learner_cv()`](https://niehs.github.io/beethoven/reference/assign_learner_cv.md)
  : Shuffle cross-validation mode for each learner type

- [`convert_cv_index_rset()`](https://niehs.github.io/beethoven/reference/convert_cv_index_rset.md)
  : Generate manual rset object from spatiotemporal cross-validation
  indices

- [`fit_base_learner()`](https://niehs.github.io/beethoven/reference/fit_base_learner.md)
  : Base learner: tune hyperparameters and retrieve the best model

- [`fit_base_tune()`](https://niehs.github.io/beethoven/reference/fit_base_tune.md)
  : Tune base learner

- [`fit_prediction()`](https://niehs.github.io/beethoven/reference/fit_prediction.md)
  : Predict base learners at for cross validation folds.

- [`fit_xgb_spark()`](https://niehs.github.io/beethoven/reference/fit_xgb_spark.md)
  : Fit and evaluate XGBoost regression models on Spark

- [`generate_cv_index_sp()`](https://niehs.github.io/beethoven/reference/generate_cv_index_sp.md)
  : Prepare spatial and spatiotemporal cross validation sets

- [`generate_cv_index_spt()`](https://niehs.github.io/beethoven/reference/generate_cv_index_spt.md)
  :

  Generate spatio-temporal cross-validation index with
  [`spatialsample::spatial_block_cv`](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html)
  and year-based temporal folds

- [`generate_cv_index_ts()`](https://niehs.github.io/beethoven/reference/generate_cv_index_ts.md)
  : Generate temporal cross-validation index

- [`make_subdata()`](https://niehs.github.io/beethoven/reference/make_subdata.md)
  : Make sampled subdataframes for base learners

- [`switch_generate_cv_rset()`](https://niehs.github.io/beethoven/reference/switch_generate_cv_rset.md)
  : Choose cross-validation strategy for the base learner

- [`switch_model()`](https://niehs.github.io/beethoven/reference/switch_model.md)
  : Define a base learner model based on parsnip and tune

- [`vis_spt_rset()`](https://niehs.github.io/beethoven/reference/vis_spt_rset.md)
  : Visualize the spatio-temporal cross-validation index

### Meta learner

Fitting meta learner

- [`fit_meta_learner()`](https://niehs.github.io/beethoven/reference/fit_meta_learner.md)
  : Fit meta learner
- [`predict_meta_learner()`](https://niehs.github.io/beethoven/reference/predict_meta_learner.md)
  : Predict meta learner

### Miscellaneous

Miscellaneous functions for general use

- [`cov()`](https://niehs.github.io/beethoven/reference/cov.md) :

  Calculate code coverage of the `beethoven` package with the
  `container_models.sif` container.

- [`divisor()`](https://niehs.github.io/beethoven/reference/divisor.md)
  : Get Divisors

- [`interactive()`](https://niehs.github.io/beethoven/reference/interactive.md)
  :

  Open interactive session with `container.sif` container.

- [`scancel()`](https://niehs.github.io/beethoven/reference/scancel.md)
  : Cancel current workflow.

- [`test()`](https://niehs.github.io/beethoven/reference/test.md) :

  Run all tests within a single file from `tests/testthat/` directory
  with the `container_models.sif` container.
