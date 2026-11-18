# Read a logical flag from an environment variable

Recognizes `"1"`, `"true"`, `"yes"`, and `"y"` as true, and `"0"`,
`"false"`, `"no"`, and `"n"` as false, ignoring case. Other nonempty
values raise an error.

## Usage

``` r
env_flag(name, default = FALSE)
```

## Arguments

- name:

  character(1). Name of the environment variable.

- default:

  logical(1). Value returned when the variable is unset or empty.

## Value

A single logical value.
