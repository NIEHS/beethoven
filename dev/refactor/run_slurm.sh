#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --output=/ddn/gs1/home/manwareme/beethoven/beethoven/dev/refactor/slurm/output.out
#SBATCH --error=/ddn/gs1/home/manwareme/beethoven/beethoven/dev/refactor/slurm/error.err
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=32g
#SBATCH --partition=geo
#SBATCH --mail-user=manwareme@nih.gov

export PATH=/ddn/gs1/tools/set/R432/bin/R:/ddn/gs1/tools/cuda11.8/bin:$PATH
export LD_LIBRARY_PATH=/ddn/gs1/tools/set/R432/lib64/R/lib:/ddn/gs1/tools/cuda11.8/lib64:$LD_LIBRARY_PATH
export R_LIBS_USER=/ddn/gs1/tools/set/R432/lib64/R/library:$R_LIBS_USER

# modify it into the proper directory path. and output/error paths in the
# # SBATCH directives
# USER_PROJDIR=/ddn/gs1/home/$USER/projects
USER_PROJDIR=/ddn/gs1/home/manwareme/beethoven/

nohup nice -4 /ddn/gs1/tools/set/R432/bin/Rscript --no-init-file --no-environ $USER_PROJDIR/beethoven/dev/refactor/inst/targets_start.R