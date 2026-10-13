#!/bin/bash

#SBATCH --job-name=pipeline_part
#SBATCH --output=/ddn/gs1/home/songi2/projects/beethoven/pipeline_iout.out
#SBATCH --error=/ddn/gs1/home/songi2/projects/beethoven/pipeline_ierr.err
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=16g
#SBATCH --partition=geo
#SBATCH --mail-user=songi2@nih.gov

source /ddn/gs1/home/songi2/.profile

nohup nice -4 Rscript /ddn/gs1/home/songi2/projects/beethoven/inst/targets/targets_start.R
