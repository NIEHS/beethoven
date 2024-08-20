#!/bin/bash

#SBATCH --job-name=pipeline_bench
#SBATCH --output=/ddn/gs1/home/songi2/projects/beethoven/pipeline_out.out
#SBATCH --error=/ddn/gs1/home/songi2/projects/beethoven/pipeline_err.err
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=32g
#SBATCH --partition=geo
#SBATCH --mail-user=songi2@nih.gov

export PATH=$PATH:/ddn/gs1/tools/cuda11.8/bin
export LD_LIBRARY_PATH=/ddn/gs1/biotools/R/lib64/R/customlib:/ddn/gs1/tools/cuda11.8/lib64:$LD_LIBRARY_PATH
if [ "$USER" != "songi2" ]; then
    export R_LIBS_USER=/ddn/gs1/biotools/R/lib64/R/custompkg:$R_LIBS_USER:/ddn/gs1/biotools/R/lib64/R/library
else
    export R_LIBS_USER=/ddn/gs1/home/songi2/r-libs:$R_LIBS_USER:/ddn/gs1/biotools/R/lib64/R/library
fi

# modify it into the proper directory path. and output/error paths in the
# # SBATCH directives
USER_PROJDIR=/ddn/gs1/home/$USER/projects

nohup nice -4 Rscript $USER_PROJDIR/beethoven/inst/targets/targets_start.R
