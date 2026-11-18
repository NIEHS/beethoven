#!/bin/bash

apptainer build --force --fakeroot \
    ../sif/container_models.sif \
    ../def/container_models.def
