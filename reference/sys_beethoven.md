# Configure Library Paths and Environment Variables for Beethoven Workflow

This function sets up the library paths and environmental variables
required for running the Beethoven workflow in a containerized
environment.

## Usage

``` r
sys_beethoven(
  libpaths = .libPaths(grep(paste0("biotools|", Sys.getenv("USER")), .libPaths(), value =
    TRUE, invert = TRUE)),
  path = paste0("/usr/local/cuda/bin:", "/usr/local/nvidia/bin:", "/usr/local/cuda/bin:",
    "/usr/local/sbin:", "/usr/local/bin:", "/usr/sbin:", "/usr/bin:", "/sbin:", "/bin"),
  ld_library_path = "/usr/local/cuda/lib64",
  cuda_home = "/usr/local/cuda"
)
```

## Arguments

- libpaths:

  A character vector specifying the library paths to use. By default, it
  excludes user-specific and host paths from
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).

- path:

  A character string specifying the system `PATH` environment variable.
  Defaults to a container-friendly configuration with CUDA paths.

- ld_library_path:

  A character string specifying the `LD_LIBRARY_PATH` environment
  variable. Defaults to `"/usr/local/cuda/lib64"`.

- cuda_home:

  A character string specifying the `CUDA_HOME` environment variable.
  Defaults to `"/usr/local/cuda"`.
