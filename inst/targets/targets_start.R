################################################################################
##############################      LIBPATHS       #############################
.libPaths(
  grep(
    paste0("biotools|", Sys.getenv("USER")), .libPaths(),
    value = TRUE,
    invert = TRUE
  )
)
cat("Active library paths:\n")
.libPaths()

############################      RUN PIPELINE      ############################
targets::tar_make(reporter = "verbose_positives")
