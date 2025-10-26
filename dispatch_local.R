#!/bin/bash

# this script is used to run the host process locally, but the dispatched
# processes are still run in Apptainer containers

.libPaths(c("./lib-flex", .libPaths()))
library(targets)
library(tarchetypes)
library(beethoven)

targets::tar_make(list_feat_proc_aqs_sites)