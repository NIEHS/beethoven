# Building an Extensible, rEproducible, Test-driven, Harmonized, Open-source, Versioned, ENsemble model for air quality <a href="https://niehs.github.io/beethoven"><img align="right" src="man/figures/beethoven-logo.png" width="168px" alt="two hexagons with distributed tan, orange, and teal with geometric symbols placed. Two hexagons are diagonally placed from the top left to the bottom right" /><a>
 
[![R-CMD-check](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml)
[![cov](https://NIEHS.github.io/beethoven/badges/coverage.svg)](https://github.com/NIEHS/beethoven/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Group Project for the Spatiotemporal Exposures and Toxicology group with help from friends :smiley: :cowboy_hat_face: :earth_americas: 

> [!WARNING]
> **Breaking changes are in progress. Do not use this branch to build `beethoven`.**
> The project is transitioning from `targets` to Snakemake. The pipeline and directory structure are being reorganized as part of this migration, and this branch is not ready for installation or pipeline execution.

<p>
</p>

## Workflow

`beethoven` develops an ensemble air quality modeling workflow: download source data, calculate covariates at monitoring sites and prediction locations, fit base and meta learners, and generate predictions.

![`beethoven` workflow](man/figures/pipeline-outline.png)

## Directory structure

The `Snakemake` implementation of `beethoven` is currently being developed. This project retains the R pacakge function, as well as the deprecated `targets` definitions and archived workflow support files in the `archive/` folder. The directory structure below reflects the current checkout and will change frequently as the migration proceeds.

```text
beethoven/
├── .github/
│   └── workflows/
├── R/
├── _targets/
│   ├── meta/
│   ├── objects/
│   └── workspaces/
├── inst/
│   ├── archive/
│   │   ├── R/
│   │   ├── doc/
│   │   ├── sh/
│   │   ├── targets/
│   │   └── tools/
│   ├── container/
│   │   ├── def/
│   │   ├── sh/
│   │   └── sif/
│   ├── extdata/
│   └── targets/
├── man/
│   └── figures/
├── tests/
│   ├── testdata/
│   │   ├── base/
│   │   ├── calculate/
│   │   ├── generate/
│   │   ├── injection/
│   │   ├── load/
│   │   ├── meta/
│   │   └── postprocessing/
│   └── testthat/
└── vignettes/
```

| Directory | Purpose |
| --- | --- |
| `R/` | Package functions for data loading, covariate calculation, processing, base and meta learners, prediction, and workflow helpers. |
| `inst/targets/` | Legacy pipeline definitions using `targets`, with the associated developer guide. |
| `inst/archive/` | Earlier R code, documentation, shell scripts, `targets` configuration, and educational or demonstration tools. |
| `inst/container/` | Apptainer definitions in `def/`, build scripts in `sh/`, and local container images in `sif/`. |
| `inst/extdata/` | Supporting data distributed with the package, including the covariate list. |
| `tests/` | Unit and integration tests in `testthat/`, with supporting data grouped by function area in `testdata/`. |
| `man/` | Generated function documentation and figures. Function documentation should be updated through the roxygen comments in `R/`. |
| `vignettes/` | Narrative documentation and examples used for the package website's articles. |
| `.github/workflows/` | GitHub workflows for package checks, test coverage, linting, and other automation. |


## `targets` Deprecation

The [targets Developer's Guide](inst/targets/README.md) documents the earlier workflow and remains a reference for the migration. The former top-level `targets` configuration is now in `inst/archive/targets/`, and the stage launch scripts are in `inst/archive/sh/`. Historical setup and run instructions should not be used to build or run this branch.


## Contribution

To contribute developments or modifications, open a [Pull request](https://github.com/NIEHS/beethoven/pulls) into the `main` branch with a detailed description of the proposed changes. Pull requests must pass all status checks, and then will be approved or rejected by `beethoven`'s authors.

Utilize [Issues](https://github.com/NIEHS/beethoven/issues) to notify the authors of bugs, questions, or recommendations. Identify each issue with the appropriate label to help ensure a timely response.
