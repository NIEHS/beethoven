#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=normal
#SBATCH --ntasks=1
#SBATCH --mem=12G
#SBATCH --cpus-per-task=2
#SBATCH --error=slurm/beethoven_%j.err
#SBATCH --output=slurm/beethoven_%j.out

# Source and read in the environment variables from beethoven_env.txt
source beethoven_env.txt

# Activate conda environment
eval "$(conda shell.bash hook)"
conda activate beethoven-conda-env

# Get TAR_PROJECT from command line argument or use default
TAR_PROJECT=${1:-download}

# Validate TAR_PROJECT value
if [[ "$TAR_PROJECT" != "download" && "$TAR_PROJECT" != "cov_data" && "$TAR_PROJECT" != "cov_pred" && "$TAR_PROJECT" != "base_learner" && "$TAR_PROJECT" != "meta_learner" && "$TAR_PROJECT" != "deploy" ]]; then
    echo "Error: TAR_PROJECT must be one of 'download', 'cov_data', 'cov_pred', 'base_learner', 'meta_learner', or 'deploy', got '$TAR_PROJECT'"
    exit 1
fi

export TAR_PROJECT

Rscript -e "targets::tar_make()"