#!/bin/bash
#SBATCH --job-name=env 
#SBATCH --partition=geo
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --error=slurm/update_env_%j.err
#SBATCH --output=slurm/update_env_%j.out
#SBATCH --mail-type=ALL

conda env update -f environment.yaml --solver libmamba