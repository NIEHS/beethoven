
library(targets)
tar_make_future(
  workers = 20
)

tar_visnetwork(targets_only = TRUE)
