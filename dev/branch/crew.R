################################################################################
##### make pipeline
.libPaths()
targets::tar_destroy(ask = FALSE)
targets::tar_make()


################################################################################
##### checks
# targets::tar_read(library)
# targets::tar_read(libPaths)
# targets::tar_meta(fields = warnings, complete_only = TRUE)
# targets::tar_read(geos)

# source("../hpc/bash_functions.R")
# node("gn040815") # geo cluster
# queue()
# geo()
# job(308331)
# format(object.size(list_geos), units = "Gb")
