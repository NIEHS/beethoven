# Building an Extensible, rEproducible, Test-driven, Harmonized, Open-source, Versioned, ENsemble model for air quality <a href="https://niehs.github.io/beethoven"><img align="right" src="man/figures/beethoven-logo.png" width="168px" alt="two hexagons with distributed tan, orange, and teal with geometric symbols placed. Two hexagons are diagonally placed from the top left to the bottom right" /><a>
 
 [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Package version](https://img.shields.io/badge/dynamic/regex?url=https%3A%2F%2Fraw.githubusercontent.com%2FNIEHS%2Fbeethoven%2FHEAD%2FDESCRIPTION&search=Version%3A%5Cs%2B%28%5CS%2B%29&replace=%241&label=version&color=blue)](https://github.com/NIEHS/beethoven)
[![R-CMD-check](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml)
[![lint](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml)
[![cov](https://NIEHS.github.io/beethoven/badges/coverage.svg)](https://github.com/NIEHS/beethoven/actions/workflows/test-coverage.yaml)

Group Project for the Spatiotemporal Exposures and Toxicology group with help from friends :smiley: :cowboy_hat_face: :earth_americas: 

> [!WARNING]
> **Breaking changes are in progress. Do not use this branch to build `beethoven`.**
> The Snakemake pipeline and directory structure are undergoing breaking changes. This branch remains under development and is not ready for installation or production pipeline execution.

<p>
</p>

## Workflow

`beethoven` develops an ensemble air quality modeling workflow: download source data, calculate covariates at monitoring sites and prediction locations, fit base and meta learners, and generate predictions.

The root `Snakefile` loads `config/config.yaml` and the rules in `inst/rules/`; those rules dispatch R scripts from `inst/scripts/` through Apptainer containers or the named `beethoven` Conda environment.

The default `rule all` targets `output/base/list_base_cv_idx.qs`. Its dependencies cover initialization, AQS/HMS downloads, monitoring-site covariate calculation and merging, and base spatial CV preparation. Model fitting, meta CV preparation, grid preparation, and prediction require explicit targets; they are not part of the default run.

The calculation tier uses hourly AQS observations, previously downloaded GEOS-CF files, and daily HMS smoke polygons. The `amadeusdb` package performs SedonaDB extraction and joins, writing Parquet data with paths saved in `.qs` files. The modelling tier defines Spark XGBoost base and meta learners. Grid GEOS and HMS scripts now call SedonaDB, and the grid merge calls `amadeusdb::join_covariates()`. The grid HMS and merge scripts still have input/output mismatches, and the final prediction branch retains the earlier model interface, as described below.

The diagram below shows the broader ensemble design. The current fitting rules use Spark XGBoost for both base and meta learners.

![`beethoven` workflow](man/figures/pipeline-outline.png)

## Directory structure

The project combines R package functions with Snakemake rules and stage-specific scripts. The compact outline shows the current workflow files and main package directories. Pipeline configuration, rules, scripts, and containers are excluded from the built R package by `.Rbuildignore`; run the workflow from a repository checkout.

```text
beethoven/
├── Snakefile                        # Configuration and rule includes
├── run.sh                           # SLURM, Apptainer, and Conda launcher
├── .github/
│   └── workflows/
├── R/                               # Functions
├── config/
│   └── config.yaml                  # Dates, paths, grid resolution
├── inst/
│   ├── conda/
│   │   └── beethoven.yml            # Geospatial and Spark environment
│   ├── devel/                       # Spark development examples
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
│       ├── c02_geos.R               # Extract and join GEOS-CF Parquet batches
│       ├── c03_hms.R                # Write HMS Parquet batches
│       ├── c99_merge.R              # Merge features to Parquet
│       ├── d01_cvbase.R             # Generate base spatial CV indices
│       ├── d02_base.R               # Fit Spark XGBoost base learners
│       ├── d03_cvmeta.R             # Generate meta spatial CV indices
│       ├── d04_meta.R               # Fit Spark XGBoost meta learners
│       ├── e01_grid.R               # Generate H3 grid polygons
│       ├── e02_geos.R               # Extract and join grid GEOS-CF Parquet batches
│       ├── e03_hms.R                # SedonaDB grid HMS; variable/output fixes pending
│       ├── e99_merge.R              # amadeusdb grid join; input/output fixes pending
│       └── f01_base.R               # Earlier elastic-net grid prediction
├── man/                             # Function documentation (generated by roxygen2)
├── tests/
│   ├── testdata/                    # Fixture test data
│   └── testthat/                    # Unit and integration tests
└── vignettes/                       # Educational material and tutorials
```

## Pipeline

| Tier | Script Prefix | Rules | Behavior and principal outputs |
| --- | --- | --- | --- |
| initiate | `a` | `initiate` | Generates `chr_init_dates.qs`, `int_init_years.qs`, and the calendar/Julian date chunks in `list_init_dates.qs` and `list_init_datesj.qs`. |
| download | `b` | `download_aqs`, `download_hms` | Downloads hourly AQS CSV data and HMS shapefiles, saving `list_dl_aqs.qs` and `list_dl_hms.qs`. |
| calculate | `c` | `calculate_aqs` | Saves unique monitoring locations in `output/sf_feat_aqs_sp.qs` and hourly `Sample.Measurement` observations in `output/feat/db_feat_aqs_sptmpl.parquet`. |
| calculate | `c` | `calculate_geos` | Calls `amadeusdb::sedona_geos()` to write batches under `output/feat/geos/`, then joins variables under `output/feat/geos/join/`. Saves the resulting paths in `output/feat/db_feat_geos_sptmpl.qs`. |
| calculate | `c` | `calculate_hms` | Calls `amadeusdb::sedona_hms()` to write daily smoke indicators under `output/feat/hms/`. Saves the paths in `output/feat/db_feat_hms_sptmpl.qs`. |
| calculate | `c` | `calculate_merge` | Calls `amadeusdb::join_covariates()` with AQS observations, hourly GEOS, and daily HMS. Writes joined Parquet data under `output/feat/join/` and saves the paths in `output/feat/db_feat_merge_sptmpl.qs`. |
| model | `d` | `model_cvbase` | Creates `output/base/list_base_params_static.qs` and repeated spatial CV site-ID lists in `output/base/list_base_cv_idx.qs`, the default pipeline endpoint. |
| model | `d` | `model_base` | Calls the Spark XGBoost helper to write fold predictions under `output/base/pred/`. Saves prediction paths in `output/base/db_base_xgb_pred.qs` and metrics in `output/base/dt_base_xgb_eval.qs`; script limitations are listed below. |
| model | `d` | `model_cvmeta` | Generates separate repeated spatial CV site-ID lists in `output/meta/list_meta_cv_idx.qs`. |
| model | `d` | `model_meta` | Declares `output/meta/dt_meta_xgb_pred.parquet` and `output/meta/dt_meta_xgb_eval.qs`. Its base-prediction input and helper call still need updating. |
| grid | `e` | `prediction_grid` | Generates H3 polygons in `output/sf_pred_grid_sp.qs`, but still requires an older merged-data input. |
| grid | `e` | `prediction_geos` | Calls `amadeusdb::sedona_geos()` for H3 polygons, writes batches under `output/grid/geos/`, and joins variables under `output/grid/geos/join/`. Saves the paths in `output/grid/db_pred_geos_sptmpl.qs`. |
| grid | `e` | `prediction_hms` | Calls `amadeusdb::sedona_hms()` with `output/grid/hms/` as its data directory and declares `output/grid/db_pred_hms_sptmpl.qs`. The location variable and save call still need correction. |
| grid | `e` | `prediction_merge` | Calls `amadeusdb::join_covariates()` with `output/grid/join/` as its data directory and declares `output/pred/db_pred_merge_sptmpl.qs`. The rule's input declaration and the script's input/output names remain incomplete. |
| predict | `f` | `predict_base` | Still expects saved elastic-net workflows in `list_base_fit_elnet.qs` and declares `dt_pred_base_sptmpl.qs`; it has not yet been connected to the Spark fitting outputs. |

Outputs are written under `output/` relative to the project root. The initialization and download filenames above are directly under that directory. AQS observations, GEOS-CF/HMS batches, joined features, and Spark fold predictions use Parquet. The `db_feat_geos_sptmpl.qs`, `db_feat_hms_sptmpl.qs`, `db_feat_merge_sptmpl.qs`, `db_pred_geos_sptmpl.qs`, and `db_base_xgb_pred.qs` files contain paths to those data. Other `.qs` files store dates, spatial locations, CV lists, parameters, or evaluation tables. The grid HMS and merge rules also declare `db_pred_*` path files, but their scripts do not yet save those outputs correctly.

Initialization supplies dates and years to the download and calculation rules. AQS processing supplies monitoring locations to GEOS-CF and HMS extraction. `c02_geos.R` currently selects the first ten configured GEOS variables, processes batches of up to 240 files, and calls `amadeusdb:::join_variables()` to join the results by `site_id` and `time`. HMS uses batches of up to 50 files. `c99_merge.R` reads the saved paths and passes GEOS as hourly data and HMS as daily data to `amadeusdb::join_covariates()`.

GEOS-CF files must already exist: there is no GEOS download rule. `input/.hidden/chr_group_store.txt` supplies the storage root, with data beneath `input/geos/<asset>/`. The monitoring-site and grid extraction scripts load the Conda environment's `libgdal.so.39` before calling `amadeusdb`. Snakemake tracks the `.qs` path files as outputs; the referenced GEOS/HMS, joined-feature, and base-prediction Parquet files are not individually declared outputs.

`b02_narr.R` remains in the repository without a corresponding download rule. There is no current NARR calculation rule. Grid generation uses the North Carolina county example bundled with `sf`, storing H3 cell IDs as `site_id` at the configured resolution. `e02_geos.R` extracts the first ten configured GEOS variables at those polygons and joins the results, using the same variable selection and batch size as monitoring-site GEOS extraction. The grid HMS and merge interfaces are still being migrated.

The current rules and scripts have the following unresolved dependencies and interfaces:

- `calculate_aqs` requires `output/list_dl_narr.qs`, although its script no longer reads that list and no included rule produces it. A fresh run needs that existing file until the dependency is updated.
- Both fitting scripts source `R/devel.R`, which is absent. `model_base` produces a `.qs` vector of fold-prediction paths, while `model_meta` still requires `output/base/dt_base_xgb_pred.parquet`; no rule assembles that table. `d04_meta.R` also passes `path=` instead of `input=`, omits the required `output=` argument, and treats the helper's returned paths as a prediction table.
- `prediction_grid` still requests and reads `output/dt_feat_merge_sptmpl.qs`; the current feature merge produces `output/feat/db_feat_merge_sptmpl.qs` containing Parquet paths.
- The grid-rule migration is incomplete. `prediction_merge` contains an empty `sf_pred_grid_sp` input without a separating comma; that declaration must be completed before the included rule file can be parsed.
- `e03_hms.R` reads `sf_pred_grid_sp` but passes the undefined `sf_feat_aqs_sp` to `sedona_hms()`. It stores the result in `db_pred_hms_sptmpl`, then attempts to save the undefined `dt_pred_hms_sptmpl` through the old `dt_pred_hms_sptmpl` output key. The rule declares `db_pred_hms_sptmpl`; the script must save the returned paths under that key.
- `e99_merge.R` requests a `db_pred_narr_sptmpl` input that its rule does not declare, then passes undefined `db_feat_aqs_sptmpl` and `db_feat_geos_sptmpl` variables to `join_covariates()`. Its save call requests `db_feat_merge_sptmpl`, while the rule declares `db_pred_merge_sptmpl`. The script still needs a grid/time input for the join and consistent GEOS/HMS input and output names.
- `predict_base` still requires `output/dt_pred_merge_sptmpl.qs` and `output/list_base_fit_elnet.qs`. Neither is produced by the current rules; the new grid merge declares `output/pred/db_pred_merge_sptmpl.qs`, and the modelling branch uses Spark XGBoost rather than saved elastic-net workflows.

## Spark modelling

The modelling branch defines both base and meta learners, with the remaining integration work listed above. `d01_cvbase.R` loads the merged-data paths from `.qs` and inspects the Parquet schema through SedonaDB. It sets `Sample_Measurement` as the outcome and uses the remaining columns, excluding `site_id` and `time`, as predictors.

Both CV scripts project AQS locations to CRS 5070 and call `spatialsample::spatial_block_cv()` with five folds, square 100,000-meter cells, and a 50,000-meter buffer. The current configuration requests two base repeats (`int_base_iter = 2`) and one meta repeat (`int_meta_iter = 1`). Each output is a nested list of analysis and assessment `site_id` values by repeat and fold. Splits are spatial: Spark selects all available times for the sites assigned to each fold.

`d02_base.R` and `d04_meta.R` source `R/spark.R` and call its shared `fit_xgb_spark()` helper. Base learners use the feature columns named in the common parameters. The meta script expects an assembled base-prediction table with `lon`, `lat`, and `base_` prediction columns, but that assembly step and the meta helper call still need updating. The helper processes repeats and folds sequentially, training a distributed CPU XGBoost regressor on analysis sites and predicting assessment sites.

Spark reads Parquet, checks that the numeric feature names and order match the supplied predictors, aliases them as `feature_####`, and assembles dense vectors to preserve explicit zero values. It caches the prepared features and fold data, selecting training and assessment rows using the supplied spatial CV site lists.

Each fold writes a separate Parquet dataset named `rset_NNN_fold_NN.parquet` beneath the supplied output directory; base fitting uses `output/base/pred/`. Predictions retain `site_id`, `time`, `lon`, `lat`, and the original outcome name, plus a column such as `base_001` or `meta_001` for that repeat. The helper returns newly written Parquet part-file paths in `pred` and an evaluation table in `eval`. It does not stack folds or join repeat columns. Evaluation records contain repeat, fold, RMSE, MSE, R², MAE, and fit elapsed time. Fitted Spark models are not saved as pipeline outputs.

Both fitting scripts read the Spark master address from `input/.hidden/chr_spark_master.txt` and the installation path from `input/.hidden/chr_spark_home.txt`. They connect to an existing cluster; `run.sh` does not start it. These files are read directly by the scripts. `XGBOOST_JARS`, when set, supplies JAR paths; otherwise Spark requests the package named by `XGBOOST_MAVEN_PACKAGE`.

| Spark setting | Current value or default | Use |
| --- | --- | --- |
| `SPARK_CORES_MAX` | `64` by default; both fitting scripts set `128` | Maximum cores requested for the Spark application. |
| `SPARK_EXECUTOR_CORES` | `2` | Executor cores, read by the shared helper. |
| `SPARK_EXECUTOR_MEMORY` | `2g` by default; base script sets `8g`, meta script sets `4g` | Memory per executor. |
| `SPARK_SHUFFLE_PARTITIONS` | `32` | Spark SQL shuffle partitions. |
| `XGB_NUM_WORKERS` | Maximum cores divided by executor cores, rounded down, with a minimum of 1 | XGBoost workers; defaults to 64 with the fitting scripts' 128 cores and 2 cores per executor. |
| `XGB_NTHREAD` | Executor cores (`2` by default) | Threads per XGBoost worker; also sets `spark.task.cpus`. |
| `XGBOOST_MAVEN_PACKAGE` | `ml.dmlc:xgboost4j-spark_2.13:3.4.0` | Package requested when `XGBOOST_JARS` is unset. |
| `XGB_NUM_ROUND` | `50` | Boosting rounds per fold fit. |
| `XGB_SEED` | `20260904` | XGBoost estimator seed; it is not passed to the spatial CV generators. |
| `XGB_MAX_ROWS` | `0` | Optional SQL row limit; zero uses all rows. |

`spark_environment()` reads the settings into a list used explicitly by `fit_xgb_spark()`. Worker and thread overrides are applied to the estimator. Validation requires executor cores not to exceed maximum cores, threads per worker not to exceed executor cores, and workers times threads not to exceed maximum cores. The objective is `reg:squarederror`, evaluation metric is `rmse`, tree method is `hist`, maximum depth is 8, learning rate is 0.1, and row/column sampling fractions are 0.8. The stored `normalize` and `metric` fields in the common parameter list are not passed into this helper.

## Configuration and execution

| Setting | Current value | Use |
| --- | --- | --- |
| `chr_config_daterange` | `2018-01-01` through `2019-12-31` | Inclusive dates for downloading and processing data. |
| `chr_config_dir` | `beethoven/beethoven/input/` | AQS/HMS download directory, resolved relative to `$HOME` by the rules. GEOS-CF uses the separate shared-storage path described above. |
| `int_config_radii` | `1000`, `5000`, `10000` | Configured buffer radii in meters; not consumed by the current rules. HMS monitoring-site calculation explicitly passes `radius = 0`. |
| `int_grid_res` | `5` | H3 resolution for grid preparation. |
| `int_base_iter` | `2` | Number of repeated spatial CV partitions for base learners. |
| `int_meta_iter` | `1` | Number of repeated spatial CV partitions for meta learners. |

`run.sh` enables both `--use-apptainer` and `--use-conda`. Snakemake dispatches scripts through the environment declared by each rule:

| Execution environment | Rules |
| --- | --- |
| `inst/container/sif/container_covariates.sif` | `initiate`, `download_aqs`, `download_hms`, and `prediction_grid`. |
| Named Conda environment `beethoven` | All four `calculate_*` rules, all four `model_*` rules, and `prediction_geos`, `prediction_hms`, and `prediction_merge`. |
| `inst/container/sif/container_models.sif` | The retained `predict_base` rule. Current base/meta fitting uses Conda. |

The Conda rules name an existing environment rather than a YAML file for automatic creation. `inst/conda/beethoven.yml` records R 4.6, Java 17, native geospatial libraries, and build tools, with notes for manual R package installation. The scripts require the corresponding R packages, including `amadeus`, `amadeusdb`, `beethoven`, `sedonadb`, `arrow`, `qs2`, `spatialsample`, `sparklyr`, and `DBI`. HMS downloading also requires mirai; grid generation requires `h3jsr`.

The container definitions and build scripts remain under `inst/container/def/` and `inst/container/sh/`. Both main definitions install `beethoven` from `NIEHS/beethoven@mm-1013`, `sparklyr`, Spark, and `arrow`. The covariates definition installs `amadeus` from `NIEHS/amadeus@mm-aqs-1019` for hourly AQS support. The models image includes CUDA support; current Spark fitting specifies the CPU.

`run.sh` requests one SLURM allocation on the `geo` partition with 20 CPUs and 100 GB of memory. Logs are written to `slurm/beethoven_<jobid>.out` and `.err`. The Snakemake core limit comes from the first script argument, then `SLURM_CPUS_PER_TASK`, then a fallback of `1`. The launcher does not request GPUs or pass `--nv`. Spark fitting connects from this allocation to the separately configured cluster, whose executor settings are independent of the launcher allocation.

The HMS download script starts 100 local mirai workers; the unused NARR download script starts 50. Both source the download helpers in `R/amadeus.R`. No current calculation or grid script calls `calculate_narr_mirai()` or `calculate_hms_map()`: the GEOS/HMS extraction scripts use `amadeusdb` and do not start mirai pools. Spatial CV preparation and Spark fitting also do not start mirai pools. The download rules declare no thread resources corresponding to those local pools, so their sizes must be accounted for separately from Snakemake's core limit. Inputs, outputs, runtime state, logs, Spark warehouse files, and container images are ignored by Git.


## Contribution

To contribute developments or modifications, open a [Pull request](https://github.com/NIEHS/beethoven/pulls) into the `main` branch with a detailed description of the proposed changes. Pull requests must pass all status checks, and then will be approved or rejected by `beethoven`'s authors.

Utilize [Issues](https://github.com/NIEHS/beethoven/issues) to notify the authors of bugs, questions, or recommendations. Identify each issue with the appropriate label to help ensure a timely response.
