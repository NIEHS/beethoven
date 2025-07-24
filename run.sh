#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --output=slurm/beethoven_%j.out

#############################      COVARIATES      #############################
# Download and calculate AQS sites covariates via container_covariates.sif
echo "Submitting {beethoven} covariates targets ..."
sbatch --wait inst/scripts/run_covariates.sh

#############################        MODELS        #############################
# Fit CPU-enbaled meta learner models via container_models.sif.
echo "Submitting {beethoven} models targets ..."
sbatch --wait inst/scripts/run_models.sh

#############################      PREDICTION      #############################
# Calculate prediction grid covariates and predict via container_covariates.sif
# echo "Submitting {beethoven} prediction targets ..."
# sbatch --wait inst/scripts/run_predict.sh
