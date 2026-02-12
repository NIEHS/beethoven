#!/bin/bash
#SBATCH --job-name=env 
#SBATCH --partition=geo
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --error=slurm/create_env_%j.err
#SBATCH --output=slurm/create_env_%j.out
#SBATCH --mail-type=ALL

conda env create -f environment.yml --solver libmamba