#!/bin/bash

#SBATCH --job-name=lgb
#SBATCH --mail-user=mitchell.manware@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --cpus-per-task=1
#SBATCH --error=slurm/lgb_%j.err
#SBATCH --output=slurm/lgb_%j.out

############################      CERTIFICATES      ############################
# Export CURL_CA_BUNDLE and SSL_CERT_FILE environmental variables to vertify
# servers' SSL certificates during download.
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

###############################      GPU SETUP     #############################
# Ensure all allocated GPUs are visible
export CUDA_VISIBLE_DEVICES=$(echo $(seq 0 $((SLURM_GPUS_ON_NODE-1))) | tr ' ' ',')

#############################        MODELS        #############################
# Set environmental variable to indicate {lightGBM} model fitting targets.
export BEETHOVEN=lgb

# Fit CPU-enabled base learner models via container_models.sif.
apptainer exec \
  --nv \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
  --bind /ddn/gs1/group/set/Projects/beethoven/targets:/opt/_targets \
  --bind /run/munge:/run/munge \
  --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm \
  container_models.sif \
  /usr/local/lib/R/bin/Rscript --no-init-file /mnt/inst/targets/targets_start.R
