# Building an Extensible, rEproducible, Test-driven, Harmonized, Open-source, Versioned, ENsemble model for air quality <a href="https://niehs.github.io/beethoven"><img align="right" src="man/figures/beethoven-logo.png" width="168px" alt="two hexagons with distributed tan, orange, and teal with geometric symbols placed. Two hexagons are diagonally placed from the top left to the bottom right" /><a>
 
[![R-CMD-check](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml)
[![cov](https://NIEHS.github.io/beethoven/badges/coverage.svg)](https://github.com/NIEHS/beethoven/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Group Project for the Spatiotemporal Exposures and Toxicology group with help from friends :smiley: :cowboy_hat_face: :earth_americas: 

> [!WARNING]
> **Breaking changes are in progress. Do not use this branch to build `beethoven`.**
> The Snakemake pipeline and directory structure are undergoing breaking changes. This branch remains under development and is not ready for installation or production pipeline execution.

<p>
</p>

## Workflow

`beethoven` develops an ensemble air quality modeling workflow: download source data, calculate covariates at monitoring sites and prediction locations, fit base and meta learners, and generate predictions.

The root `Snakefile` loads `config/config.yaml` and the rules in `inst/rules/`; those rules dispatch R scripts from `inst/scripts/` through Apptainer containers or the named `beethoven` Conda environment.

The calculation tier uses hourly AQS observations, previously downloaded GEOS-CF files, and daily HMS smoke polygons. SedonaDB writes GEOS-CF and HMS results directly to Parquet batch files, with their paths saved as character vectors in `.qs` files. The modelling tier defines both Spark XGBoost base and meta learners, with repeated spatial block cross-validation. Grid preparation retains the `e` tier and model prediction the `f` tier; those branches still contain older dependencies described below.

The diagram below shows the broader ensemble design. The current fitting rules use Spark XGBoost for both base and meta learners.

![`beethoven` workflow](man/figures/pipeline-outline.png)

## Directory structure

The project combines R package functions with Snakemake rules and stage-specific scripts. The compact outline focuses on workflow files and main package directories; supporting directories are described in the table below. Each rule file and R workflow script has a short description.

```text
beethoven/
├── Snakefile                        # Configuration and rule includes
├── run.sh                           # SLURM and Apptainer launcher
├── .github/
│   └── workflows/
├── R/                               # Functions
├── config/
│   └── config.yaml                  # Dates, paths, grid resolution
├── inst/
│   ├── conda/
│   │   └── beethoven.yml            # Geospatial and Spark environment
│   ├── devel/                      # Spark development examples
│   ├── rules/
│   │   ├── a01_initiate.smk         # Initialize dates and years
│   │   ├── b01_download.smk         # Download hourly AQS and HMS
│   │   ├── c01_calculate.smk        # Calculate monitoring-site covariates
│   │   ├── d01_model.smk            # Spatial CV and Spark models
│   │   ├── e01_grid.smk             # Prepare grid and covariates
│   │   └── f01_predict.smk          # Predict with base learners
│   └── scripts/
│       ├── a01_initiate.R           # Generate dates and chunks
│       ├── b01_aqs.R                # Download hourly AQS observations
│       ├── b02_narr.R               # Retained NARR download script
│       ├── b03_hms.R                # Download HMS smoke polygons
│       ├── c01_aqs.R                # Prepare hourly monitoring-site observations
│       ├── c02_geos.R               # Write GEOS-CF Parquet batches
│       ├── c02_narr.R               # Retained NARR extraction script
│       ├── c03_hms.R                # Write HMS Parquet batches
│       ├── c99_merge.R              # Merge features to Parquet
│       ├── d01_cvbase.R             # Generate base spatial CV indices
│       ├── d02_base.R               # Fit Spark XGBoost base learners
│       ├── d03_cvmeta.R             # Generate meta spatial CV indices
│       ├── d04_meta.R               # Fit Spark XGBoost meta learners
│       ├── e01_grid.R               # Generate H3 grid polygons
│       ├── e02_narr.R               # Extract grid meteorology
│       ├── e03_hms.R                # Calculate grid smoke
│       ├── e99_merge.R              # Merge grid covariates
│       └── f01_base.R               # Predict base learners on grid
├── man/                             # Function documentation (generated by roxygen2)
├── tests/
│   ├── testdata/                    # Fixture test data
│   └── testthat/                    # Unit and integration tests
└── vignettes/                       # Educational material and tutorials
```

| Directory | Purpose |
| --- | --- |
| `R/` | Package functions for data loading, covariate calculation, processing, base and meta learners, prediction, and workflow helpers. |
| `config/` | Shared date range, download directory, buffer radii, H3 resolution, and base/meta repeat counts. |
| `inst/rules/` | Snakemake dependencies, outputs, parameters, container assignments, and script dispatch. |
| `inst/scripts/` | R workflow steps grouped by the `a` through `f` tier prefixes. |
| `inst/conda/` | Environment specification for the named `beethoven` Conda environment used by AQS processing, SedonaDB extraction, and modelling. |
| `inst/devel/` | Spark, Parquet, and database development examples; these scripts are not dispatched by the current rules. |
| `inst/archive/` | Earlier code, documentation, shell scripts, and development material. |
| `inst/container/` | Apptainer definitions in `def/`, build scripts in `sh/`, and local container images in `sif/`. |
| `inst/extdata/` | Supporting data distributed with the package, including the covariate list. |
| `tests/` | Unit and integration tests in `testthat/`, with supporting data grouped by function area in `testdata/`. |
| `man/` | Generated function documentation and figures. Function documentation should be updated through the roxygen comments in `R/`. |
| `vignettes/` | Narrative documentation and examples used for the package website's articles. |
| `.github/workflows/` | GitHub workflows for package checks, test coverage, linting, and other automation. |


## Pipeline

| Tier | Script Prefix | Rules | Behavior and principal outputs |
| --- | --- | --- | --- |
| initiate | `a` | `initiate` | Generates `chr_init_dates.qs`, `int_init_years.qs`, and the calendar/Julian date chunks in `list_init_dates.qs` and `list_init_datesj.qs`. |
| download | `b` | `download_aqs`, `download_hms` | Downloads hourly AQS CSV data and HMS shapefiles, saving `list_dl_aqs.qs` and `list_dl_hms.qs`. |
| calculate | `c` | `calculate_aqs` | Saves unique monitoring locations in `sf_feat_aqs_sp.qs` and hourly `Sample.Measurement` observations in `dt_feat_aqs_sptmpl.parquet`. |
| calculate | `c` | `calculate_geos` | Extracts hourly GEOS-CF covariates with SedonaDB into per-variable Parquet batches under `output/geos/`. Saves the absolute batch paths in `chr_feat_geos_sptmpl.qs`. |
| calculate | `c` | `calculate_hms` | Uses SedonaDB spatial joins to write daily light, medium, and heavy smoke indicators in Parquet batches under `output/hms/`. Saves the absolute batch paths in `chr_feat_hms_sptmpl.qs`. |
| calculate | `c` | `calculate_merge` | Declares AQS data and GEOS-CF/HMS path-list inputs, with `dt_feat_merge_sptmpl.parquet` as its output. The input filenames, script input names, and batch-reading logic remain incomplete, as noted below. |
| model | `d` | `model_cvbase` | Creates common parameters in `list_base_params_static.qs` and repeated spatial CV site-ID lists in `list_base_cv_idx.qs`. |
| model | `d` | `model_base` | Fits Spark XGBoost base learners by repeat and fold. Writes out-of-fold predictions to `dt_base_xgb_pred.parquet` and evaluation metrics to `dt_base_xgb_eval.qs`. |
| model | `d` | `model_cvmeta` | Generates separate repeated spatial CV site-ID lists in `list_meta_cv_idx.qs`. |
| model | `d` | `model_meta` | Fits Spark XGBoost meta learners using base predictions and coordinates. Writes `dt_meta_xgb_pred.parquet` and `dt_meta_xgb_eval.qs`. |
| grid | `e` | `prediction_grid`, `prediction_narr`, `prediction_hms`, `prediction_merge` | Retains the H3 grid and NARR/HMS covariate branch, producing `sf_pred_grid_sp.qs` and `dt_pred_merge_sptmpl.qs` when its older dependencies are available. |
| predict | `f` | `predict_base` | Still expects saved elastic-net workflows in `list_base_fit_elnet.qs` and declares `dt_pred_base_sptmpl.qs`; it has not yet been connected to the Spark fitting outputs. |

Outputs are written under `output/` relative to the project root. AQS observations, GEOS-CF/HMS batches, merged features, and Spark prediction tables use Parquet. Initialization objects, spatial locations, CV lists, parameters, evaluation results, and the GEOS-CF/HMS path vectors use `.qs`. The `chr_feat_*` files contain file paths, while the corresponding tabular data remain in the referenced Parquet batches. The retained grid branch also uses `.qs`.

Initialization supplies dates and years to the download and calculation rules. AQS processing supplies monitoring locations to both GEOS-CF and HMS extraction. The modelling branch depends on the merged features; base predictions then feed meta-learner fitting. GEOS-CF files must already exist in the shared data store because there is no GEOS-CF download rule.

`b02_narr.R` and `c02_narr.R` remain in the repository but have no active download or monitoring-site calculation rule. The grid branch still calls its NARR script. Grid generation continues to use the North Carolina county example bundled with `sf`, storing H3 cell IDs as `site_id` at the configured resolution.

The current rule definitions retain several dependencies and schemas that need updating:

- `calculate_aqs` still requires `list_dl_narr.qs`, although its script no longer reads that list and no included rule produces it. AQS processing therefore needs that existing file until the dependency is updated. The grid NARR rule likewise requires `chr_iter_narr.qs`, which has no current producer.
- `calculate_merge` now uses the correct AQS Parquet input. Its named GEOS-CF/HMS inputs use the `chr_feat_*` keys, but their filenames still start with `dt_feat_*`, unlike the upstream `chr_feat_*.qs` outputs. `c99_merge.R` also still looks up the former `dt_feat_*` input keys and calls `arrow::read_parquet()` directly. It has not yet been updated to read the `.qs` character vectors and combine the referenced batch files.
- HMS batches contain a daily `date` column; the merge script expects `time` alongside the hourly AQS/GEOS-CF data. The merge also retains the `Event_Type` removal from the earlier observation schema.
- `prediction_grid` still requests and reads `dt_feat_merge_sptmpl.qs`, while the merge rule writes Parquet. `predict_base` still requires `list_base_fit_elnet.qs`, which the current modelling rules do not produce. The grid covariates also retain their earlier column naming and NARR source.

## SedonaDB covariates

`c02_geos.R` and `c03_hms.R` source `R/sedona.R` and run in the named `beethoven` Conda environment. Both explicitly load `$HOME/.conda/envs/beethoven/lib/libgdal.so.39` before extraction. The helpers accept separate `input` and `output` directories, register the AQS locations once per call, compute daily or hourly results, and write each batch directly with `sedonadb::sd_write_parquet(..., single_file_output = TRUE)`. They return character vectors of absolute file paths for serialization. These scripts do not start mirai pools.

For GEOS-CF, `input/.hidden/chr_group_store.txt` supplies the shared storage root. Files are read from `input/geos/<asset>/` beneath that root, independently of `chr_config_dir`. The shared-storage text file and raw GEOS-CF files are read directly by the script rather than declared as Snakemake inputs. The script defines variables from the `aqc_tavg_1hr_g1440x721_v1` and `chm_tavg_1hr_g1440x721_v1` assets, but its development selection is currently `list_iter_geos[1:2]`: `CO` and `NO2` from the AQC asset.

`sedona_geos()` selects files for the configured dates, derives hourly UTC timestamps from their filenames, and reads the requested NetCDF variable through GDAL. At the AQS points it uses SedonaDB raster-value extraction. Each variable is processed in batches of up to 240 hourly files. Each Parquet batch contains `site_id`, `time`, and one variable column; the script flattens the returned paths across variables into `chr_feat_geos_sptmpl` without merging their tables. The helper also supports polygon zonal statistics, but the current rule supplies monitoring points.

`sedona_hms()` processes available daily smoke shapefiles in batches of up to 50. Each Parquet batch contains `site_id`, `date`, and binary `light`, `medium`, and `heavy` indicators, including zero indicators for locations without intersecting smoke on a processed date. It does not expand daily results to hourly rows. The returned paths are saved as `chr_feat_hms_sptmpl`.

| Dataset | Batch file pattern | Saved path vector |
| --- | --- | --- |
| GEOS-CF | `output/geos/geos_<variable>_<start>_<end>.parquet`, with `YYYYMMDDHHMM` timestamps. | `output/chr_feat_geos_sptmpl.qs` |
| HMS | `output/hms/hms_<start>_<end>.parquet`, with `YYYYMMDD` dates. | `output/chr_feat_hms_sptmpl.qs` |

Batch filenames record the minimum and maximum input timestamps or dates within the batch. Both helpers create the output directory when needed and skip batch files that already exist, including their paths in the returned vector. After a new batch is written, its daily or hourly database views are dropped. The helpers keep completed batches on disk rather than collecting the complete period into R.

Snakemake declares the `.qs` path vectors as rule outputs; the individual Parquet batch files are not declared outputs. Removing a batch file alone therefore does not make its rule out of date, and rerunning a helper reuses any batch paths that still exist.

## Spark modelling

The modelling branch now defines both base and meta learners. `d01_cvbase.R` reads the merged Parquet data, sets `Sample_Measurement` as the outcome, and prepares common parameters. The merge script changes periods in column names to underscores and currently omits incomplete rows; imputation remains planned.

Both CV scripts project AQS locations to CRS 5070 and call `spatialsample::spatial_block_cv()` with five folds, square 100,000-meter cells, and a 50,000-meter buffer. `int_base_iter = 100` requests 100 base repeats, and `int_meta_iter = 25` requests 25 meta repeats. Each output is a nested list of analysis and assessment `site_id` values by repeat and fold. Splits are spatial: Spark selects all available times for the sites assigned to each fold.

`d02_base.R` and `d04_meta.R` source `R/spark.R` and call its shared `fit_xgb_spark()` helper. Base learners use the feature columns named in the common parameters. Meta learners read the base prediction Parquet file and use `lon`, `lat`, and the `base_` prediction columns as predictors. The helper processes repeats and folds sequentially, training a CPU XGBoost regressor on analysis sites and predicting assessment sites.

Spark reads Parquet, checks that the numeric feature names and order match the supplied predictors, aliases them as `feature_####`, and assembles dense vectors to preserve explicit zero values. It caches the prepared features and fold data, selecting training and assessment rows using the supplied spatial CV site lists.

Predictions retain `site_id`, `time`, `lon`, `lat`, and `Sample_Measurement`. Fold predictions are stacked within each repeat and joined across repeats into columns named `base_001`, `base_002`, and so on, or `meta_001`, `meta_002`, and so on. Evaluation outputs record repeat, fold, RMSE, MSE, R², MAE, and fit elapsed time. The helper returns predictions and evaluations; fitted Spark models are not saved as pipeline outputs.

Both fitting scripts read the Spark master address from `input/.hidden/chr_spark_master.txt` and the installation path from `input/.hidden/chr_spark_home.txt`. They connect to an existing cluster; `run.sh` does not start it. These files are read directly by the scripts. `XGBOOST_JARS`, when set, supplies JAR paths; otherwise Spark requests the package named by `XGBOOST_MAVEN_PACKAGE`.

| Spark setting | Current value or default | Use |
| --- | --- | --- |
| `SPARK_CORES_MAX` | `128`, set by both fitting scripts | Maximum cores requested for the Spark application. |
| `SPARK_EXECUTOR_CORES` | `2` | Executor cores, read by the shared helper. |
| `SPARK_EXECUTOR_MEMORY` | `2g` by default; meta script sets `4g` | Memory per executor. |
| `SPARK_SHUFFLE_PARTITIONS` | `32` | Spark SQL shuffle partitions. |
| `XGBOOST_MAVEN_PACKAGE` | `ml.dmlc:xgboost4j-spark_2.13:3.4.0` | Package requested when `XGBOOST_JARS` is unset. |
| `XGB_NUM_ROUND` | `50` | Boosting rounds per fold fit. |
| `XGB_SEED` | `20260904` | XGBoost estimator seed; it is not passed to the spatial CV generators. |
| `XGB_MAX_ROWS` | `0` | Optional SQL row limit; zero uses all rows. |

The helper still fixes the XGBoost estimator to one worker and one thread, overriding the worker/thread values read from the environment. Its objective is `reg:squarederror`, evaluation metric is `rmse`, tree method is `hist`, maximum depth is 8, learning rate is 0.1, and row/column sampling fractions are 0.8. The stored `normalize` and `metric` fields in the common parameter list are not passed into this helper.

## Configuration and execution

| Setting | Current value | Use |
| --- | --- | --- |
| `chr_config_daterange` | `2018-01-01` through `2019-12-31` | Inclusive dates for downloading and processing data. |
| `chr_config_dir` | `beethoven/beethoven/input/` | AQS/HMS download directory, resolved relative to `$HOME` by the rules. GEOS-CF uses the separate shared-storage path described above. |
| `int_config_radii` | `1000`, `5000`, `10000` | Configured buffer radii in meters; not consumed by the current rules. HMS monitoring-site calculation explicitly passes `radius = 0`. |
| `int_grid_res` | `5` | H3 resolution for the retained grid branch. |
| `int_base_iter` | `100` | Number of repeated spatial CV partitions for base learners. |
| `int_meta_iter` | `25` | Number of repeated spatial CV partitions for meta learners. |

`run.sh` enables both `--use-apptainer` and `--use-conda`. Snakemake dispatches scripts through the environment declared by each rule:

| Execution environment | Rules |
| --- | --- |
| `inst/container/sif/container_covariates.sif` | `initiate`, `download_aqs`, `download_hms`, `calculate_merge`, and the four `prediction_*` grid rules. |
| Named Conda environment `beethoven` | `calculate_aqs`, `calculate_geos`, `calculate_hms`, `model_cvbase`, `model_base`, `model_cvmeta`, and `model_meta`, all with `container: None`. |
| `inst/container/sif/container_models.sif` | The retained `predict_base` rule. Current base/meta fitting uses Conda. |

The Conda rules name an existing environment rather than a YAML file for automatic creation. `inst/conda/beethoven.yml` records R 4.6, Java 17, native geospatial libraries, and build tools, with notes for manual R package installation. The scripts require the corresponding R packages, including `amadeus`, `beethoven`, `sedonadb`, `arrow`, `qs2`, `spatialsample`, `sparklyr`, and `DBI`.

The container definitions and build scripts remain under `inst/container/def/` and `inst/container/sh/`. Both main definitions install `beethoven` from `NIEHS/beethoven@mm-1013`, `sparklyr`, Spark, and `arrow`. The covariates definition installs `amadeus` from `NIEHS/amadeus@mm-aqs-1019` for hourly AQS support. The models image includes CUDA support; current Spark fitting specifies the CPU.

`run.sh` requests one SLURM allocation on the `geo` partition with 20 CPUs and 100 GB of memory. Logs are written to `slurm/beethoven_<jobid>.out` and `.err`. The Snakemake core limit comes from the first script argument, then `SLURM_CPUS_PER_TASK`, then a fallback of `1`. The launcher does not request GPUs or pass `--nv`. Spark fitting connects from this allocation to the separately configured cluster, whose executor settings are independent of the launcher allocation.

HMS downloading starts 100 local mirai workers. The retained grid NARR/HMS scripts also start 100 workers each; the unused monitoring-site NARR download/extraction scripts retain pools of 50/100. SedonaDB extraction, spatial CV preparation, and Spark fitting do not start mirai pools. The rules declare no thread resources corresponding to those local pools, so their sizes must be accounted for separately from Snakemake's core limit. Inputs, outputs, runtime state, logs, Spark warehouse files, and container images are ignored by Git.


## Contribution

To contribute developments or modifications, open a [Pull request](https://github.com/NIEHS/beethoven/pulls) into the `main` branch with a detailed description of the proposed changes. Pull requests must pass all status checks, and then will be approved or rejected by `beethoven`'s authors.

Utilize [Issues](https://github.com/NIEHS/beethoven/issues) to notify the authors of bugs, questions, or recommendations. Identify each issue with the appropriate label to help ensure a timely response.
