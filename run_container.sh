#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --mail-user=manwareme@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --mem=500G
#SBATCH --cpus-per-task=204
#SBATCH --error=/ddn/gs1/home/manwareme/beethoven/beethoven/slurm/beethoven_%j.err
#SBATCH --output=/ddn/gs1/home/manwareme/beethoven/beethoven/slurm/beethoven_%j.out

# run pipeline in the container
apptainer exec \
  --bind $PWD/inst:/inst \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
  --bind /ddn/gs1/home/manwareme/beethoven/_targets:/_targets \
  --bind $PWD:/mnt \
  beethoven_dl_calc.sif \
  Rscript --no-init-file --no-environ /mnt/inst/targets/targets_start.R

# apptainer exec \
#   --bind $PWD/inst:/inst \
#   --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
#   --bind /ddn/gs1/home/manwareme/beethoven/beethoven_refactor:/_targets \
#   --bind $PWD:/mnt \
#   beethoven_dl_calc.sif R