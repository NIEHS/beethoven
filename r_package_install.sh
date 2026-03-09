#!/bin/bash
#SBATCH --job-name=install
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=normal
#SBATCH --ntasks=1
#SBATCH --mem=12G
#SBATCH --cpus-per-task=2
#SBATCH --error=slurm/install_%j.err
#SBATCH --output=slurm/install_%j.out

# ============================================================
# STEP 2 of 3 — Install non-conda R packages
# ============================================================
# Run AFTER create_env.sh has completed successfully.
# Installs R packages not available in conda-forge.
#
# Workflow order:
#   1. sbatch create_env.sh          <- creates the conda environment
#   2. sbatch r_package_install.sh   <- this script
#   3. sbatch generate_conda_lock.sh <- (optional) snapshot exact versions
#
# Packages managed here (NOT in environment.yaml):
#   - amadeus      : installed from GitHub @migrate branch (change to CRAN
#                    once NIEHS/amadeus#migrate is merged)
#   - chopin       : GitHub-only (ropensci/chopin)
#   - missRanger   : only old version in pkgs/r; use CRAN (note case: missRanger)
#   - crew.cluster : conda has 0.4.0; override here with dev build from
#                    /ddn/gs1/group/set/crew.cluster — remove once published
#   - otelsdk      : disabled; r-otel is provided by environment.yaml and
#                    otelsdk has an ABI conflict with conda's opentelemetry
#
# DO NOT add nanonext / mirai / crew here — they are managed by conda.

# Source and read in the environment variables from beethoven_env.txt
source beethoven_env.txt

# Activate conda environment
eval "$(conda shell.bash hook)"
conda activate beethoven-conda-env

# Remove any stale R package locks
rm -rf ~/.conda/envs/beethoven-conda-env/lib/R/library/00LOCK*

# amadeus: install from @migrate branch (change to CRAN once merged)
Rscript -e "remotes::install_github('NIEHS/amadeus@migrate', upgrade = FALSE)"

# chopin: spatial workflow helper (GitHub only)
Rscript -e "remotes::install_github('ropensci/chopin', upgrade = FALSE)"

# missRanger: imputation helper (DESCRIPTION Import; old version in pkgs/r)
Rscript -e "install.packages('missRanger', repos = 'https://cran.r-project.org')"

# otelsdk: links against bundled opentelemetry which conflicts with conda's
# r-otel ABI; r-otel=0.2.0 is already provided by environment.yaml.
# Re-enable if otelsdk is explicitly required and ABI conflict is resolved.
# Rscript -e "install.packages('otelsdk', repos = 'https://cran.r-project.org')"

# crew.cluster dev build: overrides the conda 0.4.0 with a local source build.
# Remove this block once the local version is published to CRAN/conda-forge.
Rscript -e "install.packages('/ddn/gs1/group/set/crew.cluster', repos = NULL, type = 'source')"

# Install beethoven itself from local source.
# Use R CMD INSTALL (not remotes::install_local) to avoid hanging on
# GitHub SHA checks for packages listed in DESCRIPTION Remotes:.
R CMD INSTALL .
