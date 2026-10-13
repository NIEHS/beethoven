#!/bin/bash

apptainer build --fakeroot \
    ../sif/container_models.sif \
    ../def/container_models.def
