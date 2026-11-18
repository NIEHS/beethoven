# Read an integer from an environment variable

Converts a nonempty environment variable with
[`base::as.integer()`](https://rdrr.io/r/base/integer.html) and checks
the lower bound. Values that cannot be converted or are below `minimum`
raise an error.

## Usage

``` r
env_integer(name, default, minimum = 1L)
```

## Arguments

- name:

  character(1). Name of the environment variable.

- default:

  numeric(1). Value returned, after integer conversion, when the
  variable is unset or empty. The default is not checked against
  `minimum`.

- minimum:

  integer(1). Smallest permitted environment-variable value.

## Value

A single integer.
