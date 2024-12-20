#!/bin/bash

# export CURL_CA_BUNDLE and SSL_CERT_FILE environmental variables to vertify
# servers' SSL certificates during download
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

# Run download and covariate calculation tests via container_covariates.sif.
apptainer exec \
  --bind $PWD:/mnt \
  --bind /tmp:/opt/tmp \
  container_covariates.sif Rscript -e \
  ".libPaths(.libPaths()[2:3]); \
   files <- list.files('/mnt/tests/testthat', full.names = TRUE); \
   test_files <- grep('base|meta', files, value = TRUE, invert = TRUE); \
   source_files <- list.files('/mnt/R', full.names = TRUE); \
   covr::file_coverage(source_files, test_files)"

# Run base- and meta-learner tests via container_models.sif.
# apptainer exec \
#   --bind /tmp:/opt/tmp \
#   container_models.sif Rscript -e \
#   ".libPaths(.libPaths()[2:3]); \
#    files <- list.files('/mnt/tests/testthat/', full.names = TRUE); \
#    test_files <- grep('base|meta', files, value = TRUE); \
#    source_files <- list.files('/mnt/R/', full.names = TRUE); \
#    covr::file_coverage(source_files, test_files)"