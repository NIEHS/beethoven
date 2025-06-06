#!/bin/bash

#SBATCH --partition=geo

# usage: build_apptainer_image.sh [full file path]
# where full file path ends with .sif, with full directory path to save the image
# after the image is built, group write/execution privileges are given

# Recommended to run this script interactively via `sh build_container_covariates.sh`
apptainer build --fakeroot container_covariates.sif container_covariates.def
