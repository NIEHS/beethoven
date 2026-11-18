# Define a base learner model based on parsnip and tune

Define a base learner model based on parsnip and tune

## Usage

``` r
switch_model(
  model_type = c("mlp", "mlp2", "xgb", "lgb", "elnet", "brulee_linear"),
  device = c("cpu", "cuda", "gpu")
)
```

## Arguments

- model_type:

  character(1). Model type to be used. Default is "mlp". Available
  options are "mlp", "xgb", "lgb", "elnet".

- device:

  character(1). The device to be used for training. GPU acceleration is
  possible for `brulee`-engine multi-layer perceptron
  (`model_type = "mlp"`;`device = "cuda"`) and for `lightgbm`-engine
  boosted tree (`model_type = "lgb"; device = "gpu"`). Ensure your
  system is equipped with CUDA-enabled graphical processing units if
  utilizing `cuda` or `gpu`.

## Value

A parsnip model object.
