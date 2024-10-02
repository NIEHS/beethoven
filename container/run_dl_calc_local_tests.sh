#!/bin/bash
#SBATCH --job-name=dl_calc_tests
#SBATCH --partition=geo
#SBATCH --mem=128G
#SBATCH --cpus-per-task=4
#SBATCH --ntasks=16
#SBATCH --output=../slurm_messages/slurm-%j.out
#SBATCH --error=../slurm_messages/slurm-%j.err
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=ALL




apptainer exec \
  --bind $PWD/../inst:/pipeline \
  --bind $PWD/../input:/input \
  --bind $PWD/../_targets:/opt/_targets \
  --bind $PWD/..:/mnt \
  beethoven_dl_calc.sif \
  Rscript /mnt/tests/testthat/test_download.R


