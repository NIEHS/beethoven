#!/bin/bash

# usage: build_apptainer_image.sh [full file path]
# where full file path ends with .sif, with full directory path to save the image
# after the image is built, group write/execution privileges are given

# Recommended to run this script interactively via `sh build_dl_calc.sh`
apptainer build --fakeroot beethoven_dl_calc.sif beethoven_dl_calc.def