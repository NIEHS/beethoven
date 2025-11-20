#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=normal
#SBATCH --ntasks=1
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH --error=slurm/beethoven_%j.err
#SBATCH --output=slurm/beethoven_%j.out

# source beethoven_binds.txt

# Fit CPU-enabled base learner models via container_models.sif.
# apptainer exec \
#   --env-file beethoven_env.txt \
#   --bind ${ROOT_DIR}:/mnt \
#   --bind ${INST_DIR}:/inst \
#   --bind ${INPUT_DIR}:/input \
#   --bind ${STORE_DIR}:/opt/_targets   \
#   --bind ${SLURM_MUNGE}:/run/munge \
#   --bind ${SLURM_ETC}:/etc/slurm \
#   container_models.sif \
#   /usr/local/lib/R/bin/Rscript --no-init-file -e "targets::tar_make()"

# Fit CPU-enabled base learner models via container_models.sif.
# apptainer shell \
#   --env-file beethoven_env.txt \
#   --bind ${ROOT_DIR}:/mnt \
#   --bind ${INST_DIR}:/inst \
#   --bind ${INPUT_DIR}:/input \
#   --bind ${STORE_DIR}:/opt/_targets   \
#   --bind ${SLURM_MUNGE}:/run/munge \
#   --bind ${SLURM_ETC}:/etc/slurm \
#   container_models.sif 

Rscript -e "targets::tar_make()"