# load targets
library(
  beethoven,
  lib.loc = "/ddn/gs1/home/manwareme/R/x86_64-pc-linux-gnu-library/4.3"
)
library(targets)
# assume that the working directory is beethoven git repository directory
# only runs after package deployment
# file.copy(
#   from = system.file("targets", "_targets.R", package = "beethoven"),
#   to = "_targets.R"
# )

tar_make_future(
  workers = 16
)
# TODO: should find a way of auto-invalidate feat_calc_(modis|viirs|geoscf)
#     when the date range changes in the configuration.
# manual example includes:
# targets::tar_invalidate(
#   matches("feat_calc_(modis|viirs|geoscf)")
# )

# selective execution, mix with time components
# status saving with timestamp? editable log/config file?
# tar_make_future(
#   names = contains("download")
# )

# tar_visnetwork(targets_only = TRUE)
