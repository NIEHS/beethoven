#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --mail-user=mitchell.manware@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --mem=1000G
#SBATCH --cpus-per-task=250
#SBATCH --gres=gpu:4
#SBATCH --error=slurm/beethoven_%j.err
#SBATCH --output=slurm/beethoven_%j.out

############################      CERTIFICATES      ############################
# Export CURL_CA_BUNDLE and SSL_CERT_FILE environmental variables to vertify
# servers' SSL certificates during download.
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

###############################      GPU SETUP     #############################
# Ensure all allocated GPUs are visible
export CUDA_VISIBLE_DEVICES=$(echo $(seq 0 $((SLURM_GPUS_ON_NODE-1))) | tr ' ' ',')

#############################      COVARIATES      #############################
# Set environmental variable to indicate download and covariate
# calculation targets.
export BEETHOVEN=covariates

# Download and calculate covariates via container_covariates.sif
apptainer exec \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
  --bind $PWD/_targets:/opt/_targets \
  container_covariates.sif \
  Rscript --no-init-file /mnt/inst/targets/targets_start.R

#############################        MODELS        #############################
# Set environmental variable to indicate model fitting targets.
export BEETHOVEN=models

# Fit models via container_models.sif external to the targets pipeline.
apptainer exec \
  --nv \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
  --bind $PWD/_targets:/opt/_targets \
  container_models.sif \
  /usr/local/lib/R/bin/Rscript --no-init-file /mnt/inst/exec/model_mlp.R

#############################      POST MODELS     #############################
# Run post-model targets via container_models.sif.
apptainer exec \
  --nv \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
  --bind $PWD/_targets:/opt/_targets \
  container_models.sif \
  /usr/local/lib/R/bin/Rscript --no-init-file /mnt/inst/targets/targets_start.R
