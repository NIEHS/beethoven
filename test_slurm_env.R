# test_slurm_env.R
cat("\n=== SLURM ENVIRONMENT CHECK ===\n")

# 1. Print key Slurm-related environment variables
slurm_vars <- Sys.getenv(
    c(
        "SLURM_JOB_ID",
        "SLURM_JOB_NAME",
        "SLURM_JOB_NODELIST",
        "SLURM_SUBMIT_DIR",
        "SLURM_CLUSTER_NAME",
        "SLURM_CPUS_PER_TASK",
        "SLURM_MEM_PER_NODE",
        "SLURM_NTASKS",
        "SLURM_PROCID"
    ),
    unset = NA
)

print(slurm_vars[!is.na(slurm_vars)])

# 2. Print key Apptainer / Bind-related environment variables
cat("\n=== APPTAINER / BIND PATHS ===\n")
apptainer_vars <- Sys.getenv()[grep("APPTAINER", names(Sys.getenv()))]
print(apptainer_vars)

# 3. Check mounted directories (i.e., binds)
cat("\n=== DIRECTORY CHECKS ===\n")
dirs_to_check <- c(
    "/mnt",
    "/inst",
    "/input",
    "/opt/_targets",
    "/etc/slurm",
    "/run/munge"
)
for (d in dirs_to_check) {
    cat(sprintf("%s exists: %s\n", d, dir.exists(d)))
    if (dir.exists(d)) {
        cat(sprintf("Contents of %s:\n", d))
        print(head(list.files(d, recursive = FALSE)))
    }
}

# 4. Show currently loaded packages and their source paths
cat("\n=== LOADED PACKAGES ===\n")
pkgs <- loadedNamespaces()
pkg_paths <- sapply(pkgs, function(x) {
    path <- tryCatch(find.package(x), error = function(e) NA)
    path
})
print(as.data.frame(pkg_paths))

# 5. Print session info for completeness
cat("\n=== SESSION INFO ===\n")
print(sessionInfo())
