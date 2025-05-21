################################################################################
############################         STAGE          ############################
cat("Running {beethoven}", Sys.getenv("BEETHOVEN"), "targets ...\n")

############################        SETTINGS        ############################
# Set paths for R, CUDA, and LD_LIBRARY_PATH, and check for CUDA availability.
beethoven:::sys_beethoven()

# Check .libPaths().
cat("Active library paths:\n")
.libPaths()

# Check PATH.
cat("Active PATH:\n")
Sys.getenv("PATH")

# Check LD_LIBRARY_PATH
cat("Active LD_LIBRARY_PATH:\n")
Sys.getenv("LD_LIBRARY_PATH")

############################      RUN PIPELINE      ############################
targets::tar_make(list_dt_train, reporter = "verbose_positives")
