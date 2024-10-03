################################################################################
##############################      LIBPATHS       #############################
print("Imported library paths:")
.libPaths()
.libPaths(
  grep(
    paste0("biotools|", Sys.getenv("USER")), .libPaths(),
    value = TRUE,
    invert = TRUE
  )
)
print("Set library paths:")
.libPaths()

############################      RUN PIPELINE      ############################
targets::tar_make()
