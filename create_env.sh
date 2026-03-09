#!/bin/bash
#SBATCH --job-name=env
#SBATCH --partition=normal
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --error=slurm/create_env_%j.err
#SBATCH --output=slurm/create_env_%j.out
#SBATCH --mail-type=ALL

# ============================================================
# STEP 1 of 3 — Create the conda environment
# ============================================================
# Run this first, before r_package_install.sh.
# After this succeeds, optionally run generate_conda_lock.sh
# to snapshot the resolved package versions.
#
# Two modes:
#   Default  : resolves fresh from environment.yaml
#              sbatch create_env.sh
#
#   Lock mode: installs the exact versions recorded in conda-lock.yml
#              (requires conda-lock and a committed conda-lock.yml)
#              USE_LOCK=1 sbatch create_env.sh
#
# Workflow order:
#   1. sbatch create_env.sh          <- this script
#   2. sbatch r_package_install.sh   <- installs non-conda R packages
#   3. sbatch generate_conda_lock.sh <- (optional) snapshot exact versions

eval "$(conda shell.bash hook)"

if [[ "${USE_LOCK:-0}" == "1" ]]; then
  echo "Creating environment from conda-lock.yml (exact pinned versions)"
  conda-lock install \
    --name beethoven-conda-env \
    conda-lock.yml
else
  echo "Creating environment from environment.yaml (resolved versions)"
  conda env create -f environment.yaml --solver libmamba
fi