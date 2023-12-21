### ----- test reshaping ------
testthat::test_that("base learner preparation", {
  withr::local_package("terra")
  withr::local_package("torch")
  withr::local_package("dplyr")
  withr::local_package("data.table")

  nsp <- 50L
  nt <- 60L
  np <- 4L

  # random data with 50 (spatial)*60 (temporal)*4 (covariates)
  sphere <-
    terra::ext(c(xmin = 0, xmax = 100, ymin = 0, ymax = 80))
  pnts <- terra::spatSample(sphere, nsp, lonlat = FALSE, as.points = TRUE)
  pst <- split(seq(1, nt), seq(1, nt))
  pst <- lapply(pst,
                function(x) {
                             pnts$pid <- seq(1, nsp)
                             return(pnts)})
  pst <- Reduce(rbind, pst)
  pst$time <- rep(as.Date("2024-01-19") + seq(0, nt - 1), each = nsp)
  pst$pm2.5 <- rgamma(nsp * nt, 32, 1.6)
  
  pstx <- rgamma(nsp * nt * np, 1, 0.002)
  pstx <- matrix(pstx, nrow = nsp * nt, ncol = np)
  pstx <- as.data.frame(pstx)
  colnames(pstx) <- sprintf("X%d", seq(1, np))
  pst <- cbind(pst, pstx)

  source("./R/manipulate_spacetime_data.R")
  pstdt <- convert_stobj_to_stdt(pst)

  colindx <-
    grep(
         paste0("(", paste(sprintf("X%d", seq(1, np)), collapse = "|"), ")"),
         colnames(pstdt$stdt))
  pstdt$stdt[, ..colindx] |>
    as.matrix() |>
    torch::torch_tensor(dtype = torch::torch_float64()) |>
    torch::torch_reshape(shape = list(-1))

})




### ------ test end ------
