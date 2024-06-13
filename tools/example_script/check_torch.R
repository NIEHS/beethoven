#' Check torch installation and load
#' @param default_device character(1). "cpu", "cuda", or "mps"
#' @returns NULL
#' @author Insang Song
#' @importFrom torch torch_is_installed
#' @importFrom torch torch_device
#' @importFrom torch install_torch
#' @importFrom torch cuda_is_available
#' @importFrom torch backends_mps_is_available
#' @export
check_and_load_torch <- function(
  default_device = c("cpu", "cuda", "mps")
) {
  default_device <- match.arg(default_device)

  if (!torch::torch_is_installed()) {
    torch::install_torch()
  }
  if (!torch::cuda_is_available()) {
    if (default_device == "cuda") {
      message("There is no device found to use CUDA. Trying other devices...")
    }
  }
  if (!torch::backends_mps_is_available()) {
    if (default_device == "mps") {
      message("MPS is not available in your system. Setting cpu as default.")
      default_device <- "cpu"
    }
  }
  torch::torch_device(default_device)
}

