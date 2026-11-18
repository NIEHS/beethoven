# Base learner: tune hyperparameters and retrieve the best model

Multilayer perceptron model with different configurations of hidden
units, dropout, activation, and learning rate using brulee and
tidymodels. With proper settings, users can utilize graphics processing
units (GPU) to speed up the training process.

## Usage

``` r
fit_base_learner(
  rset = NULL,
  full_data = NULL,
  model = NULL,
  tune_grid_size = 10L,
  yvar = "Arithmetic.Mean",
  xvar = NULL,
  drop_vars = NULL,
  normalize = TRUE,
  metric = "rmse",
  ...
)
```

## Arguments

- rset:

  A space/time CV set generated from beethoven

- full_data:

  The full data frame to be used for training after the tuning. Passed
  to [`parsnip::fit`](https://generics.r-lib.org/reference/fit.html).

- model:

  The parsnip model object. Preferably generated from `switch_model`.

- tune_grid_size:

  numeric(1), finetune grid size.

- yvar:

  The target variable.

- xvar:

  The predictor variables.

- drop_vars:

  character vector or numeric. The variables to be dropped from the
  data.frame.

- normalize:

  logical(1). If `TRUE`, all numeric predictors are normalized. Default
  is `FALSE`.

- metric:

  character(1). The metric to be used for selecting the best. Must be
  one of "rmse", "rsq", "msd", "mae". Default = "rmse"

- ...:

  Additional arguments to be passed.

## Value

The fitted workflow.

## Details

LightGBM model is fitted at the defined rate (`r_subsample`) of the
input dataset by grid or Bayesian optimization search. With proper
settings, users can utilize graphics processing units (GPU) to speed up
the training process.

XGBoost model is fitted at the defined rate (`r_subsample`) of the input
dataset by grid or Bayesian optimization search. With proper settings,
users can utilize graphics processing units (GPU) to speed up the
training process.

Elastic net model is fitted at the defined rate (`r_subsample`) of the
input dataset by grid search or Bayesian optimization.

- MLP: Hyperparameters `hidden_units`, `dropout`, `activation`, and
  `learn_rate` are tuned. `With tune_mode = "grid"`, users can modify
  `learn_rate` explicitly, and other hyperparameters will be predefined
  (56 combinations per `learn_rate` for mlp).

- XGBoost: Hyperparameters `mtry`, `ntrees`, and `learn_rate` are tuned.
  With `tune_mode = "grid"`, users can modify `learn_rate` explicitly,
  and other hyperparameters will be predefined (30 combinations per
  `learn_rate`).

- LightGBM: Hyperparameters `mtry`, `ntrees`, and `learn_rate` are
  tuned. With `tune_mode = "grid"`, users can modify `learn_rate`
  explicitly, and other hyperparameters will be predefined (30
  combinations per `learn_rate`).

- Elastic net: Hyperparameters `mixture` and `penalty` are tuned.

Tuning is performed based on random grid search (size = 10).

## Note

tune package should be 1.2.0 or higher. brulee, xgboost, and lightgbm
should be installed with GPU support. Grid search is not activated in
this function, regardless of other parts' description.
