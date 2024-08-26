################################################################################
##### unit and integration tests for targets controller functions
##### main files: R/targets_control.R

################################################################################
##### set_slurm_resource
test_that("set_slurm_resource returns the correct structure", {
  # expect no error
  testthat::expect_no_error(
    res <- set_slurm_resource(
      template_file = "inst/targets/template_slurm.tmpl",
      partition = "cluster",
      ncpus = 2L,
      ntasks = 2L,
      memory = 8,
      user_email = "test@mail.com",
      error_log = "slurm_error.log"
    )
  )
  # expect output is a list
  testthat::expect_true(is.list(res))
  # expect output$future is a tar_resources_object
  testthat::expect_s3_class(res$future, "tar_resources_future")
})
