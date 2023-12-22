## After loading and attaching NRTAPModel package
"_PACKAGE"


.onLoad <- function(lib, pkg) {
  packageStartupMessage("
    NRTAPmodel 1.0.0
    ---------------------------------------------------------------------
    Please run startup_nrtap() to apply default settings of this package.
    Consider running ?startup_nrtap for details.
    ---------------------------------------------------------------------\n")
}



#' Start-up settings
#' @param workdir character(1). Working directory path.
#' @param user_mode logical(1).
#' Use an user directory for fitting models.
#' When \code{TRUE}, a directory with the current session's user name will be
#' created under [working directory]/output/[username].
#' @param username character(1). Used username. Default is
#' the "effective_user" element of \code{[Sys.info()]} outputs.
#' @export
startup_nrtap <-
  function(
    workdir = "./",
    user_mode = TRUE,
    username = Sys.info()["effective_user"]
  ) {
    setwd(workdir)
    cat(sprintf("Your home directory is set to %s\n", getwd()))
    cat("If it seems incorrect, please set your working directory manually.\n")
    cat("This package supposes the working directory has subdirectories\n")
    cat("for running the entire pipeline:\n")
    cat("   - ./R\n")
    cat("   - ./inst\n")
    cat("   - ./input\n")
    cat("   - ./output\n")
    cat("   - ./tests\n")
    cat("   - ./tools\n\n")
    # Directories listed below should align with additional settings
    # related to pipeline settings, HPC deployment, etc.

    if (username != Sys.info()["effective_user"]) {
      username_in <- username
    }

    userpath <- file.path(workdir, "output", username_in)
    if (user_mode) {
      if (!dir.exists(userpath)) {
        dir.create(userpath, recursive = TRUE)
        cat(sprintf("%s is created for running the pipeline in user mode.\n",
                    userpath))
      }
    }
    cat(sprintf("%s is created for running the pipeline in user mode.\n",
                userpath))
  }
