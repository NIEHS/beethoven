# load targets
library(targets)

# assume that the working directory is beethoven git repository directory
# only runs after package deployment
file.copy(
  from = system.file("targets", "_targets.R", package = "beethoven"),
  to = "_targets.R"
)

tar_make_future(
  workers = 30
)


# selective execution, mix with time components
# status saving with timestamp? editable log/config file?
tar_make_future(
  names = contains("download")
)

tar_visnetwork(targets_only = TRUE)
