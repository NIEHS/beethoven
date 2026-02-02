#!/bin/bash

#SBATCH --job-name=covariate
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --mem=900G
#SBATCH --cpus-per-task=225
#SBATCH --error=slurm/cov_%j.err
#SBATCH --output=slurm/cov_%j.out


source beethoven_env.txt


# Set stack size limit for large merge of TRI covariates.
ulimit -s 20000

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
  /usr/local/lib/R/bin/Rscript --no-init-file -e "targets::tar_make()"
