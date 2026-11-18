# Load arguments from the formatted argument list file

This function loads the list object of arguments to be injected into the
calculation functions defined at each target. The arguments are numeric
or character, and some of these are function names. In this case, the
internal function `unmarshal_function` is called to convert the function
name to the actual function.

## Usage

``` r
loadargs(argfile, dataset)
```

## Arguments

- argfile:

  character(1). Path to the argument file. RDS format.

- dataset:

  character(1). Dataset name.

## Value

A list of arguments stored in `dataset` slot of the argument file.
