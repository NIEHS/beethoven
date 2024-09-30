#!/bin/bash

#SBATCH --job-name=crew
#SBATCH --output=/ddn/gs1/home/manwareme/beethoven/targets_crew/slurm/output.out
#SBATCH --error=/ddn/gs1/home/manwareme/beethoven/targets_crew/slurm/error.err
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=32g
#SBATCH --partition=geo
#SBATCH --mail-user=manwareme@nih.gov

export PATH=/ddn/gs1/tools/set/R432/bin/R:/ddn/gs1/tools/cuda11.8/bin:$PATH
export LD_LIBRARY_PATH=/ddn/gs1/tools/set/R432/lib64/R/lib:/ddn/gs1/tools/cuda11.8/lib64:$LD_LIBRARY_PATH
export R_LIBS_USER=/ddn/gs1/tools/set/R432/lib64/R/library:$R_LIBS_USER

# Run the R script with tar_make()
nohup nice -4 /ddn/gs1/tools/set/R432/bin/Rscript --no-init-file --no-environ crew.R
