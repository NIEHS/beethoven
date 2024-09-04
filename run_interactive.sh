#!/bin/bash

export PATH=$PATH:/ddn/gs1/tools/cuda11.8/bin
export LD_LIBRARY_PATH=/ddn/gs1/biotools/R/lib64/R/customlib:/ddn/gs1/tools/cuda11.8/lib64:$LD_LIBRARY_PATH
if [ "$USER" != "songi2" ]; then
    # export R_LIBS_USER=/ddn/gs1/biotools/R/lib64/R/custompkg:$R_LIBS_USER:/ddn/gs1/biotools/R/lib64/R/library
    export R_LIBS_USER=/ddn/gs1/group/set/isong-archive/r-libs:$R_LIBS_USER:/ddn/gs1/biotools/R/lib64/R/library

else
    export R_LIBS_USER=/ddn/gs1/home/songi2/r-libs:$R_LIBS_USER:/ddn/gs1/biotools/R/lib64/R/library
fi


# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.
nohup nice -4 R CMD BATCH inst/targets/targets_start.R &