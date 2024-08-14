#!/bin/bash

# beethoven project directory
BEETHOVEN=""

# Define the external directory for storing targets
# Should match the directory defined in _targets.R
USER_DIR="/ddn/$USER"



# Check if the directory exists, if not, create it
if [ -d "/ddn" ]; then
  mkdir -p "$USER_DIR"
elif [ -d "/pipeline" ]; then
  echo "It seems the pipeline is placed in a container. Make sure that you have a correctly configured Apptainer image file."
  echo "Also make sure setup_hook.sh and sbatch is executable."
else
  echo "Directory $USER_DIR already exists."
fi


# Assign user-specific system variables
USERNAME=$(whoami)
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# Export variables
export USERNAME
export TIMESTAMP

# Print the assigned variables
echo "USERNAME: $USERNAME"
echo "TIMESTAMP: $TIMESTAMP"
if [ -d "$USER_DIR" ]; then
  echo "_targets storage location: $USER_DIR"
else
  echo "_targets storage location: /opt/_targets (INTERNAL)"
fi
