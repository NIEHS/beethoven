#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --mail-user=mitchell.manware@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --mem=10G
#SBATCH --cpus-per-task=20
#SBATCH --error=slurm/cov_%j.err
#SBATCH --output=slurm/cov_%j.out

############################      CERTIFICATES      ############################
# Export CURL_CA_BUNDLE and SSL_CERT_FILE environmental variables to vertify
# servers' SSL certificates during download.
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

############################          CORES         ############################
cores="${1:-${SLURM_CPUS_PER_TASK:-1}}"

############################       COVARIATES       ############################
snakemake \
    --cores "${cores}" \
    --use-apptainer \
    -p
