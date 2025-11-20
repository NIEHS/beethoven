#!/bin/bash

#SBATCH --job-name=beethoven
#SBATCH --mail-user=kyle.messier@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=normal
#SBATCH --ntasks=1
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH --error=slurm/beethoven_%j.err
#SBATCH --output=slurm/beethoven_%j.out


############################################
# BIND PATHS 
############################################
ROOT_DIR=/ddn/gs1/home/messierkp/projects/beethoven
INPUT_DIR=/ddn/gs1/home/messierkp/projects/beethoven/input
STORE_DIR=/ddn/gs1/home/messierkp/projects/beethoven/opt/_targets
INST_DIR=/ddn/gs1/home/messierkp/projects/beethoven/inst
SLURM_MUNGE=/run/munge
SLURM_ETC=/ddn/gs1/tools/slurm/etc/slurm

export APPTAINER_BINDPATH="\
$PWD:/mnt,\
$PWD/inst:/inst,\
$PWD/input:/input,\
$PWD/opt/_targets:/opt/_targets,\
$SLURM_MUNGE:/run/munge,\
$SLURM_ETC:/etc/slurm,\
$PWD/.netrc:/mnt/.netrc,\
$PWD/.dodsrc:/mnt/.dodsrc,\
$PWD/.urs_cookies:/mnt/.urs_cookies"


apptainer exec container_models.sif Rscript -e "targets::tar_make()"