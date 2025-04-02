#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --partition=highmem
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --output=slurm/beethoven_%j.out


#############################      COVARIATES      #############################
# Download and calculate AQS sites covariates via container_covariates.sif
echo "Submitting {beethoven} covariates targets ..."
sbatch --wait inst/scripts/run_covariates.sh

#############################        MODELS        #############################
# Fit CPU-enabled {elnet} models on `geo` cluster via container_models.sif.
echo "Submitting {beethoven} {elnet} base model targets ..."
sbatch --wait inst/scripts/run_models_elnet.sh

# Fit CPU-enabled {lightGBM} models on `normal` cluster via container_models.sif.
echo "Submitting {beethoven} {lightGBM} base model targets ..."
sbatch --wait inst/scripts/run_models_lgb.sh

# Fit GPU-enabled {brulee} models on `geo` cluster via container_models.sif.
echo "Submitting {beethoven} {brulee} base model targets ..."
sbatch --wait inst/scripts/run_models_mlp.sh

# Fit CPU-enbaled meta learner models via container_models.sif.
echo "Submitting {beethoven} CPU-enabled meta models targets ..."
sbatch --wait inst/scripts/run_models_meta.sh

#############################      PREDICTION      #############################
# Calculate prediction grid covariates and predict via container_covariates.sif
# echo "Submitting {beethoven} prediction targets ..."
# sbatch --wait inst/scripts/run_predict.sh
