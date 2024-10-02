#!/bin/bash
#SBATCH --job-name=download_calc
#SBATCH --partition=geo
#SBATCH --mem=128G
#SBATCH --cpus-per-task=4
#SBATCH --ntasks=16
#SBATCH --output=../slurm_messages/slurm-%j.out
#SBATCH --error=../slurm_messages/slurm-%j.err
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=ALL


# Run the container
# .sif file sites in "root/container", thus we need to go up one level with bind mounts 
apptainer exec \
  --bind $PWD/inst:/pipeline \
  --bind $PWD/input:/input \
  --bind $PWD/_targets:/opt/_targets \
  --bind $PWD:/mnt \
  beethoven_dl_calc.sif \
  Rscript /mnt/run.R


