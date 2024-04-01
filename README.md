[![R-CMD-check](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/check-standard.yaml)
[![cov](https://NIEHS.github.io/beethoven/badges/coverage.svg)](https://github.com/NIEHS/beethoven/actions)
[![lint](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/beethoven/actions/workflows/lint.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

# Building an Extensible, rEproducible, Test-driven, Harmonized, Open-source, Versioned, ENsemble model for air quality
Group Project for the Spatiotemporal Exposures and Toxicology group with help from friends :smiley: :cowboy_hat_face: :earth_americas:

## Installation

```r
remotes::install_github("NIEHS/beethoven")
```

## Getting Started

```r
TODO 
```

## Overall Project Workflow

Targets: Make-like Reproducible Analysis Pipeline
 1) AQS Data
 2) Generate Covariates
 3) Fit Base Learners
 4) Fit Meta Learners
 5) Predictions
 6) Summary Stats
 
**Placeholder for up-to-date rendering of targets**

```r
tar_visnetwork(targets)
```
    

## Project Organization

Here, we describe the structure of the project and the naming conventions used. The most up to date file paths and names are recorded here for reference.

### File Structure

#### Folder Structure

- `R/` This is where the main R code (e.g. .R files) lives. Nothing else but .R files should be in here. i.e. Target helper functions, model fitting and post-processing, plotting and summary functions. 
- `tests/` This is where the unit and integration tests reside. The structure is based off the standard practices of the [testthat](https://testthat.r-lib.org/) R package for unit testing.
    - `testthat` Unit and integration tests for CI/CD reside here
    - `testdata` Small test datasets including our small (in size) complete pipeline testing. 
    - `testthat.R` Special script created and maintained by testthat
- `man/` This sub-directory contains .Rd and othe files created by the [roxygen2](https://roxygen2.r-lib.org/) package for assisted documentation of R packages
- `vignettes/` Rmd (and potentially Qmd) narrative text and code files. These are rendered into the **Articles** for the package website created by [pkgdown](https://pkgdown.r-lib.org/) 
- `inst/` Is a sub-directory for arbitrary files outside of the main `R/` directory
     - `targets` which include the important pipeline file `_targets.R`
     - `lookup` Is a subdirectory for text file lookup table used in the pipeline to synchronize paths, names, abbreviations, etc.
- `.github/workflows/` This hidden directory is where the GitHub CI/CD yaml files reside

##### The following sub-directories are not including the package build and included only in the source code here
- `tools/` This sub-directory is dedicated to educational or demonstration material (e.g. Rshiny). 
- `input/` ***warning soon to be deprecated*** This sub-directory contains data used during the analysis. It is going to be superceded by the use of `targets`
- `output/` ***warning: soon to be deprecated*** This sub-directory contains data used during the analysis. It is going to be superceded by the use of `targets`
Currently, as of 3/29/24, the output folder contains .rds files for each
of the covariates/features for model development. e.g.:

- NRTAP_Covars_NLCD.rds
- NRTAP_Covars_TRI.rds 
  
#### Relevant files 

- LICENSE
- DESCRIPTION
- NAMESPACE 
- README.md

### Naming Conventions

Naming things is hard and somewhat subjective. Nonetheless, consistent naming conventions make for better reproducibility, interpretability, and future extensibility. 
Here, we provide the `beethoven` naming conventions for objects as used in `targets` and for naming functions within the package (i.e. **R/**). 
For `tar_target` functions, we use the following naming conventions:


Naming conventions for `tar` objects. We are motivated by the [Compositional Forecast](https://cfconventions.org/Data/cf-standard-names/docs/guidelines.html) (CF) model naming conventions:

e.g. [surface] [component] standard_name [at surface] [in medium] [due to process] [assuming condition]
In CF, the entire process can be known from the required and optional naming pieces. 

Here, we use the following naming convention:

**[R object type]\_[role-suffix]\_[stage]\_[source]\_[spacetime]**

 Each section is in the brackets [] and appears in this order. For some objects, not all naming sections are required. If two keywords in a section apply, then they are appended with a `-`

Examples: 1) `sf_PM25_log10-fit_AQS_siteid` is an `sf` object for `PM25` data that is log-transformed and ready for base-learner fitting, derived from AQS data and located at the siteid locations. 
2) `SpatRast_process_MODIS` is a terra `SpatRast` object that has been processed from MODIS.


#### Naming section definitions:

- **R object type**: sf, datatable, tibble, SpatRaster, SpatVector

- **role:** Detailed description of the role of the object in the pipeline. Allowable keywords:

  - PM25
  - feature (i.e. geographic covariate) 
  - base_model
    - base_model suffix types: linear, random_forest, xgboost, neural_net etc.
  - meta_model 
  - prediction
  - plot
    -plot suffix types: scatter, map, time_series, histogram, density etc. 
  
- **stage**: the stage of the pipeline the object is used in. Object transformations
are also articulated here. Allowable keywords: 

  - raw
  - process
  - calc
  - fit: Ready for base/meta learner fitting
  - result: Final result
  - log
  - log10 

- **source:** the original data source


  - AQS
  - MODIS
  - GMTED 
  - NLCD
  - NARR
  - GEOSCF
  - TRI
  - KOPPENGEIGER
  - MERRA2
  - HMS
  - gROADS
  - POPULATION
  - [Note, we can add and/or update these sources as needed] 

- **spacetime:** relevant spatial or temporal information 

  - spatial: 
    - siteid
    - censustract
    - grid
  - time: 
    - daily  [optional YYYYMMDD]
    - annual  [optional YYYY]



Short Verion: 

A shortened version for filenames is available on the punchcard at `tools/pipeline/punchcard.csv`

### Function Naming Convenctions 

We have adopted naming conventions in functions in this package as well as `amadeus` which is a key input package. 

**[High-Level-Process]\_[Source]\_[Object]**

- **High-Level-Process**
     - download
     - process
     - calc

- **source:** the original data source. Same as source section for tar objects

- **Object** An object that the function may be acting on
     - base_model
     - meta_model
     - feature 


 
### Punchcard 

The punchard is a single list of paths, variables, and functions that are used throughout the pipeline. 

If a path is used in multiple places, it is only listed once. Then updates only require changing the path in one place.

`tools/pipeline/punchcard.csv`



