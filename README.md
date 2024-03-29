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
Root Directory
- R/ 
- input/
- output/
- tests/
- inst/
- docs/
- tools/
- man/
- vignettes/

#### input/ 

#### output/ 

Currently, as of 3/29/24, the output folder contains .rds files for each
of the covariates/features for model development. e.g.:

- NRTAP_Covars_NLCD.rds
- NRTAP_Covars_TRI.rds 


#### tests/ 

##### testthat/

##### testdata/
  
#### Relevant files 

- LICENSE
- DESCRIPTION
- NAMESPACE 
- README.md

### Naming Conventions

For `tar_target` functions, we use the following naming conventions:

Example from CF conventions: 
[surface] [component] standard_name [at surface] [in medium] [due to process] [assuming condition]

Naming conventions for tar objects: 

Long Version:
[R object type]_[role]_[stage]_[source]_[spacetime]

- R object type: sf, datatable, tibble, SpatRaster, SpatVector

- role : Detailed description of the role of the object in the pipeline. Allowable keywords:

  - PM25
  - feature (i.e. geographic covariate) 
  - base_model
    - base_model suffix types: linear, random_forest, xgboost, neural_net etc.
  - meta_model 
  - prediction
  - plot
    -plot suffix types: scatter, map, time_series, histogram, density etc. 
  
- stage: the stage of the pipeline the object is used in. Object transformations
are also articulated here. Allowable keywords: 

  - raw
  - process
  - fit: Ready for base/meta learner fitting
  - result: Final result
  - log
  - log10 

- source: the original data source

  - AQS
  - MODIS
  - GMTED 
  - NLCD
  - NARR
  - GEOSCF
  - TRI

  
- spacetime: relevant spatial or temporal information 

  - spatial: 
    - site_id
    - census_tract
    - grid
  - time: 
    - daily  [optional YYYYMMDD]
    - annual  [optional YYYY]



Short Verion: 

Cross-walk lives on the punchcard.

### Function Naming Convenctions 

TBD

  - download
  - calc

### Punchcard 

The punchard is a single list of paths, variables, and functions that are used throughout the pipeline. 

If a path is used in multiple places, it is only listed once. Then updates only require changing the path in one place.

tools/pipeline/punchcard.csv





