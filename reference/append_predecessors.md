# Append Predecessors

This function appends predecessors to an existing object or creates a
new object if none exists.

## Usage

``` r
append_predecessors(
  path_qs = "output/qs",
  period_new = NULL,
  input_new = NULL,
  nthreads = 8L
)
```

## Arguments

- path_qs:

  The path where the predecessors will be stored.

- period_new:

  The new period to be appended.

- input_new:

  The new input object to be appended.

- nthreads:

  The number of threads to be used.

## Value

If no existing predecessors are found, the function saves the new input
object and returns the name of the saved file. If existing predecessors
are found, the function appends the new input object to the existing
ones and returns the combined object.
