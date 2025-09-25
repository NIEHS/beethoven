#!/bin/bash

#SBATCH --job-name=gridh3
#SBATCH --mail-user=geoissong@snu.ac.kr
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=compute
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --error=slurm/predict_%j.err
#SBATCH --output=slurm/predict_%j.out

############################      CERTIFICATES      ############################
# Export CURL_CA_BUNDLE and SSL_CERT_FILE environmental variables to vertify
# servers' SSL certificates during download.
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

#############################      PREDICTION COVARIATEs      #############################
# Set environmental variable to indicate download and covariate
# calculation targets.
export BEETHOVEN=predict

###############################      GPU SETUP     #############################
# Ensure all allocated GPUs are visible
export CUDA_VISIBLE_DEVICES=$(echo $(seq 0 $((SLURM_GPUS_ON_NODE-1))) | tr ' ' ',')


# Set stack size limit for large merge of TRI covariates.
ulimit -s 20000

# Download and calculate covariates via container_covariates.sif
apptainer exec \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind /ddn:/input \
  --bind $PWD/_targets:/opt/_targets \
  --bind /run/munge:/run/munge \
  --bind /usr/local/etc/slurm.conf:/usr/local/etc/slurm.conf \
  --bind /etc/hosts:/etc/hosts \
  container_covariates.sif \
  /usr/local/lib/R/bin/Rscript --no-init-file /mnt/inst/targets/targets_start.R
