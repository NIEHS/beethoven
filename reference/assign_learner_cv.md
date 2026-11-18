# Shuffle cross-validation mode for each learner type

Shuffle cross-validation mode for each learner type

## Usage

``` r
assign_learner_cv(
  learner = c("lgb", "mlp", "elnet"),
  cv_mode = c("spatiotemporal", "spatial", "temporal"),
  num_models = 100L,
  num_device = ifelse(torch::cuda_device_count() > 1, 2, 1),
  crs = 5070L,
  cellsize = 100000L,
  balance = FALSE
)
```

## Arguments

- learner:

  character(1). The base learner to be used. Default is "mlp". Available
  options are "mlp", "lgb", "elnet".

- cv_mode:

  character(1). The cross-validation mode to be used. Default is
  "spatiotemporal". Available options are "spatiotemporal", "spatial",
  "temporal".

- num_models:

  integer(1). The number of repetitions for each `cv_mode`.

- num_device:

  integer(1). The number of CUDA devices to be used. Each device will be
  assigned to each eligible learner (i.e., lgb, mlp).

- crs:

  Coordinate reference system in `sf` style. Default is 5070L Albers
  Equal Area Projected

- cellsize:

  cellsize for cross-validation spatial blocks. crs units. Default is
  100km

- balance:

  logical(1). If TRUE, the number of CUDA devices will be equally
  distributed based on the number of eligible devices.

## Value

A data frame with three columns: learner, cv_mode, and device.
