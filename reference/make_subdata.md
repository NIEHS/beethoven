# Make sampled subdataframes for base learners

Per beethoven resampling strategy, this function selects the predefined
number of rows from the input data table and saves the row index in
.rowindex field.

## Usage

``` r
make_subdata(data, n = NULL, p = 0.3, ngroup_init = NULL)
```

## Arguments

- data:

  An object that inherits data.frame.

- n:

  The number of rows to be sampled.

- p:

  The proportion of rows to be used. Default is 0.3.

- ngroup_init:

  integer(1). Initial number of splits for pairing groups. Default is
  NULL. Ensures that subsample is divisible by `ngroup_init` for
  `generate_cv_index_spt`.

## Value

The row index of the original data. The name of the original data object
is stored in attribute "object_origin".
