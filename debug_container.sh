#!/bin/bash

DEBUG_TARGET=$1

#SBATCH --job-name=beethoven
#SBATCH --mail-user=manwareme@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --mem=100G
#SBATCH --cpus-per-task=50
#SBATCH --error=/ddn/gs1/home/manwareme/beethoven/beethoven/slurm/beethoven_%j.err
#SBATCH --output=/ddn/gs1/home/manwareme/beethoven/beethoven/slurm/beethoven_%j.out

# run pipeline in the container
apptainer exec \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind /ddn:/input \
  --bind $PWD/_targets:/opt/_targets \
  container/container_covariates.sif \
  Rscript --no-init-file -e "targets::tar_read('$DEBUG_TARGET')"
  # Rscript --no-init-file -e "targets::tar_meta(fields = error, complete_only = TRUE)"
  # Rscript --no-init-file -e "sf::st_write(targets::tar_read('$DEBUG_TARGET'), '/mnt/sf_base.gpkg')"

# run interactive R session in the container
# apptainer exec --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input --bind $PWD:/mnt beethoven_dl_calc.sif R