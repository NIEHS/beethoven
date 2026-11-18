#!/bin/bash

export PATH=/ddn/gs1/tools/set/R432/bin/R:/ddn/gs1/tools/cuda11.8/bin:$PATH
export LD_LIBRARY_PATH=/ddn/gs1/tools/set/R432/lib64/R/lib:/ddn/gs1/tools/cuda11.8/lib64:$LD_LIBRARY_PATH
export R_LIBS_USER=/ddn/gs1/tools/set/R432/lib64/R/library:$R_LIBS_USER

# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.
nohup nice -4 R CMD BATCH inst/targets/targets_start.R &