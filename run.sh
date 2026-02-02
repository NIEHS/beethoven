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



# apptainer exec \
#   --bind $PWD:/mnt \
#   --bind $PWD/inst:/inst \
#   --bind /ddn/gs1/group/set/beethoven_store/cas/input:/input \
#   --bind /ddn/gs1/group/set/beethoven_store/cas/_targets:/opt/_targets \
#   --bind /run/munge:/run/munge \
#   --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm \
#   --env-file beethoven_env.txt \
#   container_mlverse.sif  \
# export TAR_PROJECT=/ddn/gs1/group/set/beethoven_store/cas/_targets

APPTAINER_BINDPATH=$PWD:/mnt,$PWD/inst:/inst,/ddn/gs1/group/set/beethoven_store/input:/input,/ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets,/run/munge:/run/munge,/ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm
export APPTAINER_BINDPATH

Rscript  inst/targets/targets_start.R


# Add paths from sys_beethoven.sh if needed