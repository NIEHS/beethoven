#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --mail-user=manwareme@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --mem=1000G
#SBATCH --cpus-per-task=250
#SBATCH --error=slurm/beethoven_%j.err
#SBATCH --output=slurm/beethoven_%j.out

# export CURL_CA_BUNDLE and SSL_CERT_FILE environmental variables to vertify
# servers' SSL certificates during download
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

# Download and calculate covariates via container_covariates.sif
apptainer exec \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
  --bind $PWD/_targets:/opt/_targets \
  container_covariates.sif \
  Rscript --no-init-file /mnt/inst/targets/targets_start.R
