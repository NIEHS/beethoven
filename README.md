# Building an Extensible, rEproducible, Test-driven, Harmonized, Open-source, Versioned, ENsemble model for air quality <a href="https://niehs.github.io/beethoven"><img align="right" src="man/figures/beethoven-logo.png" width="168px" alt="two hexagons with distributed tan, orange, and teal with geometric symbols placed. Two hexagons are diagonally placed from the top left to the bottom right" /><a>

<p>
 
[![R-CMD-check](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml)
[![cov](https://NIEHS.github.io/beethoven/badges/coverage.svg)](https://github.com/NIEHS/beethoven/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Group Project for the Spatiotemporal Exposures and Toxicology group with help from friends :smiley: :cowboy_hat_face: :earth_americas: 

</p>

## Installation

```r
remotes::install_github("NIEHS/beethoven")
```

## Workflow

`beethoven` is a [targets](https://books.ropensci.org/targets/) reproducible analysis pipeline with the following workflow.

```mermaid
graph TD

    subgraph AQS data with `amadeus`
        AQS[PM2.5 download]-->AQS2[PM2.5 Process]
    end

    AQS2 --> Cov1
    AQS2 --> Cov2
    AQS2 --> Cov3
    AQS2 --> Cov4
    subgraph Covariate Calculation with `amadeus`
        Cov1[Meterology]
        Cov2[NLCD]
        Cov3[...]
        Cov4[MERRA2]
    end

    subgraph Processed Covariates
        PC[Baselearner Input]
    end

    Cov1 --> PC
    Cov2 --> PC
    Cov3 --> PC
    Cov4 --> PC

    PC --> A1
    PC --> A2
    PC --> A3
    subgraph MLP Baselearner   
        A1[ M_i is a 30% random sample of N] --> B1A[Spatial CV]
        A1[ M_i is a 30% random sample of N] --> B1B[Temporal CV]
        A1[ M_i is a 30% random sample of N] --> B1C[Space/Time CV]
        B1A --> C1[3. M_i is fit with a MLP model]
        B1B --> C1
        B1C --> C1
    end

    subgraph LightGBM Baselearner
        A2[ M_i is a 30% random sample of N] --> B2A[Spatial CV]
        A2[ M_i is a 30% random sample of N] --> B2B[Temporal CV]
        A2[ M_i is a 30% random sample of N] --> B2C[Space/Time CV]
        B2A --> C2[3. M_i is fit with a LightGBM model]
        B2B --> C2
        B2C --> C2
    end
    subgraph Elastic-Net Baselearner
        A3[ M_i is a 30% random sample of N] --> B3A[Spatial CV]
        A3[ M_i is a 30% random sample of N] --> B3B[Temporal CV]
        A3[ M_i is a 30% random sample of N] --> B3C[Space/Time CV]
        B3A --> C3[3. M_i is fit with a glmnet model]
        B3B --> C3
        B3C --> C3
    end
    C1 --> D1[Elastic-Net Meta-Learner]
    C2 --> D1[Elastic-Net Meta-Learner]
    C3 --> D1[Elastic-Net Meta-Learner]

    subgraph Meta-Learner Phase
        D1 --> E1[Perform 50% column-wise subsampling K times]
        E1 --> E1b[Proper Scoring CRPS CV with 1 of 3 categories with equal probability, Spatial, Temporal, or Space/Time]
        E1b --> M1[Elastic-Net Model 1]
        E1b --> M2[Elastic-Net Model 2]
        E1b --> M3[Elastic-Net Model 3]
        E1b --> M4[Elastic-Net Model K-1]
        E1b --> M5[Elastic-Net Model K]
    end


    subgraph Posterior Summary
        M1 --> P1[Complete Posterior Summary at daily, 1-km]
        M2 --> P1
        M3 --> P1
        M4 --> P1
        M5 --> P1
        P1 --> P5[Version and Deploy with Vetiver]
        P1 --> P2[Spatial and Temporal Average Summaries]
        P2 --> P5
    end

   style A1 fill:#d3d3d3,stroke:#000,stroke-width:2px
    style B1A fill:#d3d3d3,stroke:#000,stroke-width:2px
    style B1B fill:#d3d3d3,stroke:#000,stroke-width:2px
    style B1C fill:#d3d3d3,stroke:#000,stroke-width:2px
    style C1 fill:#d3d3d3,stroke:#000,stroke-width:2px     

    style A2 fill:#62C6F2,stroke:#000,stroke-width:2px
    style B2A fill:#62C6F2,stroke:#000,stroke-width:2px
    style B2B fill:#62C6F2,stroke:#000,stroke-width:2px
    style B2C fill:#62C6F2,stroke:#000,stroke-width:2px
    style C2 fill:#62C6F2,stroke:#000,stroke-width:2px     

    style A3 fill:#ffb09c,stroke:#000,stroke-width:2px
    style B3A fill:#ffb09c,stroke:#000,stroke-width:2px
    style B3B fill:#ffb09c,stroke:#000,stroke-width:2px
    style B3C fill:#ffb09c,stroke:#000,stroke-width:2px
    style C3 fill:#ffb09c,stroke:#000,stroke-width:2px     

    style P1 fill:#abf7b1,stroke:#000,stroke-width:2px
    style P2 fill:#abf7b1,stroke:#000,stroke-width:2px
    style P5 fill:#abf7b1,stroke:#000,stroke-width:2px      

```

Version 0.4.4 of `beethoven` has stable `targets` for downloading data files, calculating features at AQS sites, and merging to a base learner-ready `data.table` (`dt_feat_calc_xyt`). Ongoing changes relate to calculating features for the prediction grid, computationally managing prediction grid, base learner hyperparameter tuning, and meta learner function development.

```r
targets::tar_visnetwork()
```
![](man/figures/visnetwork_20250128.png)

## Organization

Here, we describe the structure of the repository, important files, and the `targets` object naming conventions.

### Folder Structure

- `R/` is where the `beethoven` functions are stored. Only ".R" files should be in this folder (ie. `targets` helpers, post-processing, model fitting functions).
- `inst/` is a directory for arbitrary files outside of the main `R/` directory
     - `targets/` is a sub-directory within `inst/` which contains the pipeline files (ie. "targets_aqs.R"). These files declare the `targets::tar_target` objects which constitute the `beethoven` pipeline.
- `tests/` stores unit and integration tests (`testthat/`) and test data (`testdata/`) according to the [testthat](https://testthat.r-lib.org/) package's standard structure. for unit testing.
    - `testthat.R` is created and maintained by `testthat`, and is not to be edited manually.
- `container/` stores definition files and build scripts to build covariate- and model-specific Apptainer container images (`container_covariates.def` and `container_models.def`).
- `man/` contains function documentation files (".Rd") which are by the [roxygen2](https://roxygen2.r-lib.org/) package. These files are not to be edited manually.
- `vignettes/` contains ".Rmd" narrative text and code files. These are rendered by [pkgdown](https://pkgdown.r-lib.org/) into the [Articles](https://niehs.github.io/beethoven/articles/index.html) section of the `beethoven` webpage.
- `.github/workflows/` is a hidden directory which stores the GitHub CI/CD "yaml" files.
- `tools/` is dedicated to educational or demonstration material (e.g. Rshiny), but is not excluded from the package build.

### Important Files
- `_targets.R` configures `targets` settings, creates computational resource controllers, and structures the `beethoven` pipeline.
  - To run `beethoven`, users must review and update the following parameters for their user profile and computing system:
    - `controller_*` Ensure the local controllers do not request more CPUs than are available on your machine or high performance system.
    - `#SBATCH --partition` Utilization of NVIDIA GPUs (within `glue::glue` command)
    - `--bind /USER_PATH_TO_INPUT/input:/input` (within `glue::glue` command)
- `_targets.yaml` is created and updated by running `targets::tar_make` and is not to be edited manually.
- `run.sh` allocates computational resources with SLURM and submits the `beethoven` pipeline to run on high performance computing system.
  - To run `beethoven`, users must review and update the following parameters for their user profile and computing system:
    - `#SBATCH --mail-user`
    - `#SBATCH --partition`
    - `#SBATCH --mem`
    - `#SBATCH --cpus-per-task`
    - `--bind /USER_PATH_TO_INPUT/input:/input`
    - `--bind /USER_PATH_TO_SLURM/slurm:/USER_PATH_TO_SLURM/slurm`

### Running `beethoven` Pipeline

#### User settings
`beethoven` pipeline is configured for SLURM with defaults for NIEHS HPC settings. For adapting the settings to users' environment, consult with the documentation of your platform and edit the requested resources in `run.sh` (lines 3-11) and `_targets.R` (lines 41-45; individual `crew` and `crew.cluster` controller workers).

#### Critical `targets`
There are 5 "critical" `targets` that users may want to change to run `beethoven`.

  - `chr_daterange`
    - Controls all time-related targets for the entire pipeline. This is the only `target` that needs to be changed to update the pipeline with a new temopral range. Month and year specific arguments are derived from the time range defined by `chr_daterange`.
  - `chr_nasa_token`
    - Sets the file path to the user's NASA Earthdata account credentials. These credentials expire at ~90 day intervals and therefore must be updated regularly.
  - `chr_mod06_links`
    - The file path to the MOD06 links file. These links must be manually downloaded per the `amadeus::download_modis` function. The links are then stored in a CSV file that is read by the function. The new file with links must be updated to match the new date range.
  - `chr_input_dir`
    - The file path to the input directory. This target controls where the raw data files are downloaded to and imported from. This file path must be mounted to the container at run time in the `run.sh` script.
  - `num_dates_split`
    - Controls the size of temporal splits. Splitting the temporal range into smaller chunks allows for parallel processing across multiple workers. It also allows for dispatching new dynamic branches when the temporal range is updated.

#### `Apptainer`
Current implementation of `beethoven` utilizes `Apptainer` images to run the pipeline with consistent package versions and custom installations. Users must build these images before runnning beethoven.

```sh
cd container/ # must be working in the `container/` directory
sh build_container_covariates.sh # build "covariates" stage image
sh build_container_models.sh # build "models" image
mv *sif ../ # move images to `beethoven/` root directory
```

> [!NOTE]
> `.sif` files are omitted from GitHub due to size (>5 Gb each)

#### Run
After switching back to the project root directory, users can run the pipeline with the `run.sh` shell script. The following lines of `run.sh` must be updated with user-specific settings before running the pipeline

```sh
#SBATCH --mail-user=[USER_EMAIL]      # email address for job notifications
#SBATCH --partition=[PARTITION_NAME]  # HPC partition to run on
#SBATCH --mem=[###G]                  # Total memory for the job
#SBATCH --cpus-per-task=[###]         # Total CPUs for the job
...
  --bind [USER_INPUT_DIRECTORY]/input:/input \
...
  --bind [USER_SYSTEM_PATH/munge]:/run/munge \
  --bind [USER_SYSTEM_PATH/slurm]:[USER_SYSTEM_PATH/slurm] \
```

Once configured, the pipeline can be run with a `SLRUM` batch job.

```sh
cd ../ # assuming still in the `container/` directory
sbatch run.sh
```

The SLURM batch job can also be submitted `R` session with the `batch` helper function.

```r
source("R/helpers.R")
batch()
```

# Contribution
The [Developer's Guide](https://github.com/NIEHS/beethoven/tree/main/inst/targets#developers-guide) provides detailed instructions for how to develop or update `beethoven` settings or individual `targets` objecdts

To contribute developments or modifications, open a [Pull request](https://github.com/NIEHS/beethoven/pulls) into the `dev` branch with a detailed description of the proposed changes. Pull requests must pass all status checks, and then will be approved or rejected by `beethoven`'s authors.

Utilize [Issues](https://github.com/NIEHS/beethoven/issues) to notify the authors of bugs, questions, or recommendations. Identify each issue with the appropriate label to help ensure a timely response.
