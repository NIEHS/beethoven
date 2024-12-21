################################################################################
##############################      LIBPATHS       #############################
.libPaths(
  grep(
    paste0("biotools|", Sys.getenv("USER")), .libPaths(),
    value = TRUE,
    invert = TRUE
  )
)

.libPaths(
  c("/mnt/lib-flex", .libPaths())
)

cat("Active library paths:\n")
.libPaths()

############################      RUN PIPELINE      ############################
targets::tar_make(reporter = "verbose_positives")
