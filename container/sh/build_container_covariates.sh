#!/bin/bash

apptainer build --fakeroot \
    ../sif/container_covariates.sif \
    ../def/container_covariates.def
