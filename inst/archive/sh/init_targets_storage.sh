#!/bin/bash

# Edited: 08/15/2024
# Insang Song
# Objective: to configure targets storage directory from _targets.R

# Modify $USER if you don't want to use system username
USER_SESSION_B=$USER

# beethoven project directory
USER_HOME="/ddn/gs1/home/$USER_SESSION_B"

if [ ! -f "_targets.R" ]; then
  echo "Invalid attempt. _targets.R does not exist in the current directory."
  exit 1
fi

# Define the external directory for storing targets
# Search for a "store = " in tar_config_set inside _targets.R
STR_PATH_SEARCH="store = *"
STR_PATH_FOUND=$(grep -o "$STR_PATH_SEARCH" _targets.R)

# Check if the path was found
if [ -z "$STR_PATH_SEARCH" ]; then
  echo "String '$STR_PATH_FOUND' not found in _targets.R. Halting execution."
  exit 1
else
  # Remove specific strings from the found string
  # Example: Remove "store =" from the found string
  DIR_TARGETS_USER=$(echo "$STR_PATH_FOUND" | sed 's/store = //g')
fi


# Check if the directory exists, if not, create it
if [ ! -d "$DIR_TARGETS_USER" ]; then
  mkdir -p "$DIR_TARGETS_USER"
  echo "_targets storage folder $DIR_TARGETS_USER is created."
elif [ -d "/pipeline" ]; then
  echo "It seems the pipeline is placed in a container. Make sure that you have a correctly configured Apptainer image file."
  echo "Also make sure setup_hook.sh and sbatch is executable."
  echo "_targets storage location: /opt/_targets (CONTAINER INTERNAL)"
  echo "Please check whether the pipeline execution script binds the correct directory to /opt/_targets before running the pipeline."
else
  echo "Directory $DIR_TARGETS_USER already exists."
fi
