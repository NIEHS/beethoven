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

The Snakemake implementation now resides in this project. The root `Snakefile` loads `config/config.yaml` and the rules in `inst/rules/`; those rules dispatch R scripts from `inst/scripts/` through Apptainer containers or the named `sdamadeus` Conda environment. The default output is now `output/dt_base_xgb_pred.qs`, containing observed labels and Spark XGBoost predictions for a held-out test split of the monitoring-site data.

The default workflow downloads AQS observations and NARR/HMS covariates, prepares monitoring-site features as Parquet, generates common modelling settings and CV resamples, and fits an XGBoost regressor through Spark. The elastic-net fitting and H3 grid prediction rules remain available as separate branches. Grid preparation uses the `e` tier (previously `f`), and model prediction uses the `f` tier (previously `g`). Meta-learner fitting remains a placeholder in `d04_meta.R`.

The diagram below shows the broader ensemble design; the current fitting rules implement Spark XGBoost and elastic-net base learners.

![`beethoven` workflow](man/figures/pipeline-outline.png)

## Directory structure

The project combines R package functions with Snakemake rules and stage-specific scripts. The compact outline focuses on workflow files and main package directories; supporting directories are described in the table below. Each rule file and R workflow script has a short description.

```text
beethoven/
├── Snakefile                        # Workflow includes and endpoint
├── run.sh                           # SLURM and Apptainer launcher
├── .github/
│   └── workflows/
├── R/                               # Functions
├── config/
│   └── config.yaml                  # Dates, paths, grid resolution
├── inst/
│   ├── conda/
│   │   └── sdamadeus.yml            # Spark client environment specification
│   ├── devel/                      # Spark development examples
│   ├── rules/
│   │   ├── a01_initiate.smk         # Initialize dates and years
│   │   ├── b01_download.smk         # Download AQS, NARR, HMS
│   │   ├── c01_calculate.smk        # Calculate monitoring-site covariates
│   │   ├── d01_model.smk            # CV, Spark, elastic-net models
│   │   ├── e01_grid.smk             # Prepare grid and covariates
│   │   └── f01_predict.smk          # Predict with base learners
│   └── scripts/
│       ├── a01_initiate.R           # Generate dates and chunks
│       ├── b01_aqs.R                # Download AQS observations
│       ├── b02_narr.R               # Download NARR meteorology
│       ├── b03_hms.R                # Download HMS smoke polygons
│       ├── c01_aqs.R                # Prepare monitoring-site observations
│       ├── c02_narr.R               # Extract monitoring-site meteorology
│       ├── c03_hms.R                # Calculate monitoring-site smoke
│       ├── c99_merge.R              # Merge features to Parquet
│       ├── d01_cvrset.R             # Generate spatiotemporal CV resamples
│       ├── d02_base.R               # Fit elastic-net base learners
│       ├── d02_spark.R              # Fit Spark XGBoost regressor
│       ├── d04_meta.R               # Meta-learner placeholder
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
| `config/` | Shared date range, download directory, buffer radii, and H3 resolution settings. |
| `inst/rules/` | Snakemake dependencies, outputs, parameters, container assignments, and script dispatch. |
| `inst/scripts/` | R workflow steps grouped by the `a` through `f` tier prefixes. |
| `inst/conda/` | Environment specification for the named `sdamadeus` Conda environment used by the Spark rule. |
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
| initiate | `a` | `initiate` | Generates the configured date sequence, included years, and date chunks in `chr_init_dates.qs`, `int_init_years.qs`, `list_init_dates.qs`, and `list_init_datesj.qs`. |
| download | `b` | `download_aqs`, `download_narr`, `download_hms` | Downloads source files and saves `list_dl_aqs.qs`, `list_dl_narr.qs`, and `list_dl_hms.qs`; also saves the 24 selected NARR variables in `chr_iter_narr.qs`. |
| calculate | `c` | `calculate_aqs`, `calculate_narr`, `calculate_hms`, `calculate_merge` | Prepares AQS monitoring locations and PM2.5 observations, calculates NARR/HMS covariates, and merges the feature tables by `site_id` and `time`. Cleans column names and incomplete rows, then writes `dt_feat_merge_sptmpl.parquet`. |
| model | `d` | `model_cvrset` | Reads the merged Parquet data and creates common settings in `list_base_params_static.qs` and spatiotemporal CV resamples in `list_base_cv_rset.qs`. |
| model | `d` | `model_spark` | Reads the common settings and Parquet data, fits CPU XGBoost through Spark using an 80/20 random training/test split, reports test RMSE, and saves `label` and `prediction` columns in `dt_base_xgb_pred.qs`. This is the default endpoint. |
| model | `d` | `model_base` | Fits elastic-net models using the CV resamples, saves `list_base_fit_elnet.qs`, and joins out-of-sample predictions to the training data in `dt_base_aqs_sptmpl.qs`. Runs when its outputs or the grid prediction endpoint are requested. |
| grid | `e` | `prediction_grid`, `prediction_narr`, `prediction_hms`, `prediction_merge` | Builds `sf_pred_grid_sp.qs`, calculates NARR/HMS grid covariates, and merges them into `dt_pred_merge_sptmpl.qs`. |
| predict | `f` | `predict_base` | Applies the fitted base learners to grid covariates and declares `dt_pred_base_sptmpl.qs` as its output, with location/time fields and one `pred_###` column per model. |

All serialized outputs are written under `output/` relative to the project root. Most use `.qs`; the monitoring-site feature merge now uses `.parquet`. The `all` rule requests `dt_base_xgb_pred.qs` and has no processing tier. The default dependency chain continues from `calculate_merge` through `model_cvrset` to `model_spark`. Spark uses the common parameters but creates its own random split; it does not consume `list_base_cv_rset.qs`.

The separate `model_base` branch consumes the common parameters and CV resamples. `predict_base` combines its fitted elastic-net workflows with grid covariates to produce `dt_pred_base_sptmpl.qs`; neither this endpoint nor grid preparation is part of the default Spark run. There is no active meta-learner rule.

`calculate_merge` replaces periods in all column names with underscores, omits incomplete rows, and removes `Event_Type` before writing Parquet. The outcome is consequently named `Arithmetic_Mean`. Imputation remains planned. `model_cvrset` reads this prepared data, shares it with `mori`, and currently generates one spatiotemporal CV partition with five folds, CRS 5070, and 250,000-meter spatial cells. `model_base` tunes the CPU `glmnet` engine's mixture and penalty with a grid size of three, normalization enabled, and the `mae` metric. Out-of-sample predictions are joined back to the training data using `.row` identifiers.

Grid generation uses the North Carolina county example bundled with `sf`, unions its geometry, and generates H3 cell polygons at the configured resolution. The cell IDs are stored as `site_id` in `sf_pred_grid_sp`. The grid merge joins NARR and HMS tables by `site_id` and character-valued `time`.

The earlier identifier mismatches are resolved: `e02_narr.R` merges by `site_id` and `time`, and `f01_base.R` derives cell coordinates from `site_id`. The optional grid branch still needs its input format updated: `prediction_grid` requests `dt_feat_merge_sptmpl.qs` and `e01_grid.R` reads it with `qs2`, while `calculate_merge` now produces only `dt_feat_merge_sptmpl.parquet`. A clean run cannot supply that `.qs` dependency. The grid branch also does not yet apply the column-name cleanup used by the modelling data.

## Spark modelling

`model_spark` dispatches `d02_spark.R` with `container: None` and `conda: "sdamadeus"`. The rule uses an existing named environment; it does not reference the YAML specification as a rule-managed environment. `inst/conda/sdamadeus.yml` records the environment specification, including R 4.6 and Java 17. The R client needs `DBI`, `dplyr`, `sparklyr`, and `qs2` available in that environment.

The script reads the Spark master address from `input/.hidden/chr_spark_master.txt` and the Spark installation path from `input/.hidden/chr_spark_home.txt`. It connects to that existing cluster; `run.sh` does not start the cluster. These connection files are read directly by the script rather than declared as rule inputs. `R/spark.R` provides the environment-variable and SQL-quoting helpers. `XGBOOST_JARS`, when set, supplies the XGBoost JAR paths; otherwise the client requests the package named by `XGBOOST_MAVEN_PACKAGE`.

Spark reads the Parquet table, selects numeric predictors, and checks that their names and order match `list_base_params_static$xvar`. Predictors are aliased as `feature_####` and assembled into dense feature vectors so zero values remain explicit. The script makes an 80/20 random split with seed `12345`, persists both splits, fits a CPU histogram-based XGBoost regressor, and reports test RMSE. The saved output contains only `label` and `prediction`; location/time identifiers and the fitted Spark model are not serialized.

| Spark setting | Default | Use |
| --- | --- | --- |
| `XGBOOST_MAVEN_PACKAGE` | `ml.dmlc:xgboost4j-spark_2.13:3.4.0` | XGBoost package requested when `XGBOOST_JARS` is unset. |
| `XGB_NUM_ROUND` | `50` | Number of boosting rounds. |
| `XGB_SEED` | `20260904` | Estimator seed; the training/test split separately uses `12345`. |
| `XGB_MAX_ROWS` | `0` | Optional SQL row limit; zero uses all rows. |

The estimator currently fixes one worker and one thread in `d02_spark.R`, overriding the worker/thread values read by `R/spark.R`. Its objective is `reg:squarederror`, evaluation metric is `rmse`, maximum depth is 8, learning rate is 0.1, and row/column sampling fractions are 0.8. The Spark path does not apply the elastic-net normalization or tuning-grid settings.

## Configuration and execution

| Setting | Current value | Use |
| --- | --- | --- |
| `chr_config_daterange` | `2021-01-01` through `2022-12-31` | Inclusive dates for downloading and processing data. |
| `chr_config_dir` | `beethoven/beethoven/input/` | Download directory, resolved relative to `$HOME` by the rules. |
| `int_config_radii` | `1000`, `5000`, `10000` | Configured buffer radii in meters; current NARR/HMS scripts explicitly use `radius = 0`. |
| `int_grid_res` | `5` | H3 resolution for the development grid. |

`run.sh` enables both `--use-apptainer` and `--use-conda`. Snakemake dispatches scripts through the environment declared by each rule:

| Execution environment | Rules and tiers |
| --- | --- |
| `inst/container/sif/container_covariates.sif` | `a` initiate, `b` download, `c` calculate, and `e` grid. |
| `inst/container/sif/container_models.sif` | `model_cvrset` and `model_base` in `d`, plus `predict_base` in `f`. |
| Named Conda environment `sdamadeus` | `model_spark` in `d`, with no Apptainer image. |

The container definitions and build scripts reside under `inst/container/def/` and `inst/container/sh/`. Both image definitions install the `beethoven` package from `NIEHS/beethoven@mm-1013`, along with `sparklyr`, a Spark installation, and `arrow`. The Spark modelling rule nevertheless uses the named Conda environment. NARR/HMS scripts source the local mirai-enabled helpers in `R/amadeus.R`. The models image includes CUDA support; the current elastic-net and Spark XGBoost specifications both use the CPU.

`run.sh` launches Snakemake within one SLURM allocation on the `geo` partition, requesting 20 CPUs and 10 GB of memory. It writes `slurm/beethoven_<jobid>.out` and `.err`. Its core limit comes from the first script argument, then `SLURM_CPUS_PER_TASK`, then a fallback of `1`; it does not request GPUs or pass `--nv`. The Spark rule connects from this allocation to the separately configured cluster.

NARR downloads start 50 local mirai workers; HMS downloads and NARR/HMS calculations start 100. CV preparation and elastic-net fitting currently start one mirai worker because `n_models = 1L`. Spark modelling uses Spark's estimator worker/thread settings and does not start a mirai pool. Grid prediction applies the fitted workflows sequentially. These worker counts are set in the scripts independently of Snakemake's core limit, and the rules declare no corresponding thread resources. The local worker pools therefore need to be accounted for when sizing the SLURM allocation. Downloads, serialized outputs, runtime state, logs, and container images are local artifacts ignored by Git.


## Contribution

To contribute developments or modifications, open a [Pull request](https://github.com/NIEHS/beethoven/pulls) into the `main` branch with a detailed description of the proposed changes. Pull requests must pass all status checks, and then will be approved or rejected by `beethoven`'s authors.

Utilize [Issues](https://github.com/NIEHS/beethoven/issues) to notify the authors of bugs, questions, or recommendations. Identify each issue with the appropriate label to help ensure a timely response.
