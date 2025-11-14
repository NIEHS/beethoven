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


Rscript -e "targets::tar_make()"