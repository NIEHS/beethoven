#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --output=slurm/beethoven_%j.out

#############################      COVARIATES      #############################
# Download and calculate covariates via container_covariates.sif
echo "Submitting {beethoven} covariates targets ..."
sbatch --wait inst/scripts/run_covariates.sh

#############################        MODELS        #############################
# Fit CPU-enabled base learner models via container_models.sif.
echo "Submitting {beethoven} CPU-enabled base model targets ..."
sbatch --wait inst/scripts/run_models_cpu.sh

# Fit GPU-enabled base learner models via container_models.sif.
echo "Submitting {beethoven} GPU-enabled base model targets ..."
sbatch --wait inst/scripts/run_models_gpu.sh

# Fit CPU-enbaled meta learner models via container_models.sif.
echo "Submitting {beethoven} CPU-enabled meta models targets ..."
sbatch --wait inst/scripts/run_models_meta.sh
