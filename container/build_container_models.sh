#!/bin/bash

# usage: build_apptainer_image.sh [full file path]
# where full file path ends with .sif, with full directory path to save the image
# after the image is built, group write/execution privileges are given

# Recommended to run this script interactively via `sh build_container_models.sh`
apptainer build --fakeroot ../container_models.sif container_models.def
