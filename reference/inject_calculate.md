# Injects the calculate function with specified arguments.

This function injects the calculate function with the specified
arguments, allowing for dynamic customization of the function's
behavior.

## Usage

``` r
inject_calculate(covariate, locs, injection)
```

## Arguments

- covariate:

  character(1). The name of the covariate to be calculated.

- locs:

  The locations to be used in the calculation.

- injection:

  Additional arguments to be injected into the calculate function.

## Value

The result of the calculate function with the injected arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
inject_calculate(
  locs = my_locs, buffer = 10, domain = my_domain,
  injection = list(arg1 = "value1", arg2 = "value2")
)
} # }
```
