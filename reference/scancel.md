# Cancel current workflow.

Cancel current workflow.

## Usage

``` r
scancel(job = NULL)
```

## Arguments

- job:

  character(1). SLURM job number or "full", to cancel all jobs running
  on `$USER` SLURM profile.

## Value

NULL; cancels jobs.
