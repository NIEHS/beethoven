#!/bin/bash

apptainer build --force --fakeroot \
    ../sif/container_covariates.sif \
    ../def/container_covariates.def
