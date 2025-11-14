#!/bin/bash

#SBATCH --job-name=apptainer_test
#SBATCH --output=slurm/apptainer_test_%j.out
#SBATCH --error=slurm/apptainer_test_%j.err
#SBATCH --partition=normal
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --cpus-per-task=1

# Load environment variables
source beethoven_binds.txt

echo "===== OUTSIDE CONTAINER ====="
echo "APPTAINERENV_ variables:"
env | grep '^APPTAINERENV_'
echo ""
echo "Checking bind source paths:"
for var in ROOT_DIR INST_DIR INPUT_DIR STORE_DIR SLURM_MUNGE SLURM_ETC; do
  path_var="APPTAINERENV_${var}"
  echo "$var -> ${!path_var}"
  if [ -d "${!path_var}" ]; then
    echo "✅ Exists: ${!path_var}"
  else
    echo "❌ Missing: ${!path_var}"
  fi
done
echo ""

# Test inside the container
apptainer exec \
  --env-file beethoven_env.txt \
  --bind $APPTAINERENV_ROOT_DIR:/mnt \
  --bind $APPTAINERENV_INST_DIR:/inst \
  --bind $APPTAINERENV_INPUT_DIR:/input \
  --bind $APPTAINERENV_STORE_DIR:/opt/_targets \
  --bind $APPTAINERENV_SLURM_MUNGE:/run/munge \
  --bind $APPTAINERENV_SLURM_ETC:/etc/slurm \
  container_models.sif \
  bash -c '
    echo "===== INSIDE CONTAINER ====="
    echo "Environment variables:"
    env | grep "^APPTAINERENV_"
    echo ""
    echo "Bound directories:"
    for path in /mnt /inst /input /opt/_targets /run/munge /etc/slurm; do
      echo -n "$path -> "
      if [ -d "$path" ]; then
        echo "✅ exists"
        ls -lah "$path" | head -n 5
      else
        echo "❌ missing"
      fi
    done
  '
