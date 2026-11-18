#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --mail-user=mitchell.manware@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=geo
#SBATCH --ntasks=1
#SBATCH --mem=250G
#SBATCH --cpus-per-task=20
#SBATCH --error=slurm/beethoven_%j.err
#SBATCH --output=slurm/beethoven_%j.out

############################      CERTIFICATES      ############################
# Export CURL_CA_BUNDLE and SSL_CERT_FILE environment variables to verify
# servers' SSL certificates during download.
export CURL_CA_BUNDLE=/etc/pki/tls/certs/ca-bundle.crt
export SSL_CERT_FILE=/etc/pki/tls/certs/ca-bundle.crt

############################          CORES         ############################
cores="${1:-${SLURM_CPUS_PER_TASK:-1}}"

############################       COVARIATES       ############################
snakemake \
    --cores "${cores}" \
    --use-conda \
    --use-apptainer \
    -p
