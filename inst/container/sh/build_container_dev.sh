#!/usr/bin/env bash
set -euo pipefail

# Resolve %files paths consistently, even when invoked outside the repository.
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_dir=$(cd -- "$script_dir/../../.." && pwd)
cd "$repo_dir"

output=${1:-inst/container/sif/container_dev.sif}
resume_root=${2:-${BEETHOVEN_BUILD_ROOTFS:-}}
build_jobs=${BEETHOVEN_BUILD_JOBS:-4}
if [[ ! "$build_jobs" =~ ^[1-9][0-9]*$ ]]; then
    echo 'BEETHOVEN_BUILD_JOBS must be a positive integer.' >&2
    exit 2
fi
if [[ -e "$output" ]]; then
    echo "Image already exists: $output. Supply a new output path to preserve it." >&2
    exit 2
fi
mkdir -p -- "$(dirname -- "$output")"
build_workspace=$(mktemp -d "${TMPDIR:-/tmp}/beethoven-container-build.XXXXXX")
export APPTAINER_CACHEDIR=${APPTAINER_CACHEDIR:-$build_workspace/cache}
export APPTAINER_TMPDIR=${APPTAINER_TMPDIR:-$build_workspace/tmp}
mkdir -p "$APPTAINER_CACHEDIR" "$APPTAINER_TMPDIR"
echo "Build workspace: $build_workspace"
echo "Building $output; runtime tests are deferred until explicitly approved."

# Keep failed build files for diagnosis. Do not overwrite older images or run tests.
if [[ -n "$resume_root" ]]; then
    # Resume only a partial build of this exact inventory, never a host conda env.
    cmp -- inst/conda/requirements.txt "$resume_root/opt/beethoven-build/requirements.txt"
    apptainer build --fakeroot --notest --no-cleanup --sandbox --update \
        --build-arg "BUILD_JOBS=$build_jobs" \
        "$resume_root" inst/container/def/container_dev.def
    apptainer build --notest "$output" "$resume_root"
else
    apptainer build --fakeroot --notest --no-cleanup \
        --build-arg "BUILD_JOBS=$build_jobs" \
        "$output" inst/container/def/container_dev.def
fi

sha256sum -- "$output" > "$output.sha256"
echo "Built $output. Tests have NOT been run."
