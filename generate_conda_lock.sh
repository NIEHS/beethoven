#!/bin/bash
#SBATCH --job-name=conda_lock
#SBATCH --partition=normal
#SBATCH --mem=8G
#SBATCH --cpus-per-task=2
#SBATCH --ntasks=1
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --error=slurm/conda_lock_%j.err
#SBATCH --output=slurm/conda_lock_%j.out
#SBATCH --mail-type=ALL

# ============================================================
# STEP 3 of 3 — Generate conda-lock.yml (optional but recommended)
# ============================================================
# Run AFTER create_env.sh has completed. Snapshots the exact resolved
# package versions into conda-lock.yml so the environment can be
# recreated bit-for-bit on any node.
#
# Workflow order:
#   1. sbatch create_env.sh          <- creates the conda environment
#   2. sbatch r_package_install.sh   <- installs non-conda R packages
#   3. sbatch generate_conda_lock.sh <- this script (run after step 1)
#
# After this succeeds, commit both environment.yaml AND conda-lock.yml.
# To recreate from the lock: USE_LOCK=1 sbatch create_env.sh

eval "$(conda shell.bash hook)"

# Install conda-lock into the project env (user-writable) if not present.
# The system base env (/ddn/gs1/biotools/anaconda3) is read-only.
CONDA_ENV="beethoven-conda-env"

if ! conda run -n "${CONDA_ENV}" conda-lock --version &>/dev/null; then
  conda install -n "${CONDA_ENV}" conda-lock -y -c conda-forge \
    --solver libmamba
fi

# Use libmamba solver for faster resolution
export CONDA_SOLVER=libmamba

conda run -n "${CONDA_ENV}" conda-lock lock \
  --file environment.yaml \
  --platform linux-64 \
  --lockfile conda-lock.yml
