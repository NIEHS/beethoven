print_controllers <-
  list(
    targets::tar_target(
      chr_cntrlrs_test,
      command = {
        # check all available controllers
        cat("Active LD_LIBRARY_PATH:\n")

        cntrlrs <- targets::tar_option_get("controller")
        mycntrlrs <- names(cntrlrs$private$.controllers)
        print(mycntrlrs)
        mycntrlrs

      }
    )


  )