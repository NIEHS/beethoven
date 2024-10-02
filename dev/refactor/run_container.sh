#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=32g
#SBATCH --partition=geo
#SBATCH --mail-user=manwareme@nih.gov

# run pipeline in the container
apptainer exec \
  --bind $PWD/inst:/inst \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
  --bind /ddn/gs1/home/manwareme/beethoven/beethoven_refactor:/_targets \
  --bind $PWD:/mnt \
  beethoven_dl_calc.sif \
  Rscript /mnt/inst/targets_start.R

# apptainer exec \
#   --bind $PWD/inst:/inst \
#   --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
#   --bind /ddn/gs1/home/manwareme/beethoven/beethoven_refactor:/_targets \
#   --bind $PWD:/mnt \
#   beethoven_dl_calc.sif R