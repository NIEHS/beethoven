#!/bin/bash

#SBATCH --job-name=model_dispatch
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=normal
#SBATCH --ntasks=1
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH --error=slurm/model_dispatch_%j.err
#SBATCH --output=slurm/model_dispatch_%j.out


# Fit CPU-enabled base learner models via container_models.sif.
apptainer exec \
  --env-file beethoven_env.txt \
  --bind $PWD:/mnt \
  --bind $PWD/inst:/inst \
  --bind /ddn/gs1/group/set/Projects/NRT-AP-Model/input:/input \
  --bind /ddn/gs1/group/set/Projects/beethoven/targets:/opt/_targets \
  --bind /run/munge:/run/munge \
  --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm \
  container_models.sif \
  /usr/local/lib/R/bin/Rscript --no-init-file -e "targets::tar_make()"
