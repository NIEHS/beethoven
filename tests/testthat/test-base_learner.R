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

  pstdt <- convert_stobj_to_stdt(pst)

  colindx <-
    grep(
         paste0("(", paste(sprintf("X%d", seq(1, np)), collapse = "|"), ")"),
         colnames(pstdt$stdt))

})


testthat::test_that("base_learner_fit works", {
  withr::local_package("terra")
  withr::local_package("ranger")
  withr::local_package("dplyr")
  withr::local_package("data.table")

  nsp <- 50L
  nt <- 60L
  np <- 10L

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
  cns_covar <- sprintf("X%d", seq(1, np))
  colnames(pstx) <- cns_covar
  pst <- cbind(pst, pstx)

  pstdt <- convert_stobj_to_stdt(pst)

  colindx <-
    grep(
         paste0("(", paste(sprintf("X%d", seq(1, np)), collapse = "|"), ")"),
         colnames(pstdt$stdt))

  testthat::expect_no_error(
    test_fit_rf <- base_learner_fit(pstdt,
                                    "randomforest",
                                    independent_name = cns_covar,
                                    cv_mode = "lblo", blocks = c(25L, 20L))
  )
  testthat::expect_no_error(
    test_fit_xg <- base_learner_fit(pstdt,
                                    "randomforest",
                                    independent_name = cns_covar,
                                    cv_mode = "lblo", blocks = c(25L, 20L))
  )
  testthat::expect_s3_class(test_fit_rf, "ranger")
  testthat::expect_s3_class(test_fit_xg, "xgb.Booster")
})


testthat::test_that("base learner data cv fit: ranger", {
  withr::local_package("terra")
  withr::local_package("ranger")
  withr::local_package("dplyr")
  withr::local_package("data.table")

  nsp <- 50L
  nt <- 60L
  np <- 10L

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

  pstdt <- convert_stobj_to_stdt(pst)

  colindx <-
    grep(
         paste0("(", paste(sprintf("X%d", seq(1, np)), collapse = "|"), ")"),
         colnames(pstdt$stdt))

  testthat::expect_no_error(
    res_check_fit_rf <- base_learner_fit_ranger(res_datap$ymat, res_datap$xmat)
  )


})

testthat::test_that("base learner cv fit: xgboost", {
  withr::local_package("terra")
  withr::local_package("xgboost")
  withr::local_package("dplyr")
  withr::local_package("data.table")

  nsp <- 50L
  nt <- 60L
  np <- 10L

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

  testthat::expect_no_error(
    pstdt <- convert_stobj_to_stdt(pst)
  )
  colindx <-
    grep(
         paste0("(", paste(sprintf("X%d", seq(1, np)), collapse = "|"), ")"),
         colnames(pstdt$stdt))

  res_datap <- base_learner_prep(
    learner = "xgboost",
    data = pstdt,
    dependent_name = "pm2.5",
    independent_name = sprintf("X%d", seq(1, np)))

  testthat::expect_no_error(
    res_check_fit_xg <- base_learner_fit_xgboost(res_datap$ymat, res_datap$xmat)
  )

  testthat::expect_no_error(
    res_check_fit_ranger_lblo <-
      base_learner_fit(
        data = pstdt,
        learner = "randomforest",
        dependent_name = "pm2.5",
        independent_name = colnames(pstdt$stdt)[-1:-5],
        cv_mode = "lblo",
        blocks = c(20, 20))
  )

  testthat::expect_no_error(
    res_xgb_cv <- base_learner_fit(
      data = pstdt,
      learner = "xgboost",
      dependent_name = "pm2.5",
      independent_name = colnames(pstdt$stdt)[-1:-5],
      cv_mode = "lblo",
      blocks = c(25, 25)
    )
  )

})



### ------ test end ------
