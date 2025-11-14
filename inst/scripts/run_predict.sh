#!/bin/bash

#SBATCH --job-name=gridh3
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --error=slurm/predict_%j.err
#SBATCH --output=slurm/predict_%j.out

source beethoven_env.txt

# Download and calculate covariates via container_covariates.sif
apptainer exec \
  --env-file beethoven_env.txt \
  --bind $APPTAINERENV_ROOT_DIR:/mnt \
  --bind $APPTAINERENV_INST_DIR:/inst \
  --bind $APPTAINERENV_INPUT_DIR:/input \
  --bind $APPTAINERENV_STORE_DIR:/opt/_targets   \
  --bind $APPTAINERENV_SLURM_MUNGE:/run/munge \
  --bind $APPTAINERENV_SLURM_ETC:/etc/slurm \
  container_covariates.sif \
  /usr/local/lib/R/bin/Rscript --no-init-file /mnt/inst/targets/targets_start.R
