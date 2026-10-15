#!/bin/bash

apptainer build --force --fakeroot \
    ../sif/container_dev.sif \
    ../def/container_dev.def
