#!/bin/bash

#SBATCH --job-name=pipeline_bench
#SBATCH --output=/ddn/gs1/home/songi2/projects/beethoven/pipeline_out.out
#SBATCH --error=/ddn/gs1/home/songi2/projects/beethoven/pipeline_err.err
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=16g
#SBATCH --partition=geo
#SBATCH --mail-user=songi2@nih.gov

source /ddn/gs1/home/songi2/.profile
export R_LIBS_USER=$R_LIBS_USER:/ddn/gs1/biotools/R/lib64/R/library

nohup nice -4 Rscript /ddn/gs1/home/songi2/projects/beethoven/inst/targets/targets_start.R
