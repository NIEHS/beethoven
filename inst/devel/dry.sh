#!/bin/bash

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
    --use-conda \
    --use-apptainer \
    --dry-run \
    -p
