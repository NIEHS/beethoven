
library(NRTAPmodel)
library(torch)
library(luz)

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
xtensor <-
    pstdt$stdt[, ..colindx] |>
    data.table::melt(measure.vars = colnames(pstdt$stdt)[colindx]) |>
    _[,2] |>
    as.matrix() |>
    torch::torch_tensor(dtype = torch::torch_float()) |>
    torch::torch_reshape(shape = list(1, 50, 60, 4))

colindy <- grep("pm2.5", colnames(pstdt$stdt))
ytensor <- pstdt$stdt[, ..colindy] |>
  as.matrix() |>
  torch::torch_tensor(dtype = torch::torch_float()) |>
  torch::torch_reshape(shape = list(1, 50, 60, 1))


torch::torch_float()

if (torch_is_installed()) {
model <- nn_module(
  "testConv2d",
  initialize = function() {
    self$conv1 <- nn_conv2d(4, 16, 5, 2) # N * 4 * 50 * 60
    self$conv2 <- nn_conv2d(16, 48, 3, 1) # N * 16 * 23 * 28
    # self$dropout1 <- nn_dropout2d(0.25)
    # self$dropout2 <- nn_dropout2d(0.33)
    self$fc1 <- nn_linear(4928, 100)
    self$fc2 <- nn_linear(100, 1)
  },
  forward = function(input) {
    input %>%
        self$conv1() %>%
        nnf_relu() %>%
        self$conv2() %>%
        nnf_relu() %>%
        nnf_max_pool2d(2) %>%
        self$fc1() %>%
        nnf_relu() %>%
        self$fc2()

  }
)

}
vart
train_data <- torch::dataset(
  name = "faux_dataset",
  initialize = function() {
    self$data <- list()
    self$data$y <- ytensor
    self$data$x <- xtensor
  },

  .getbatch = function(index) {
    x <- self$data$x[index, 2:-1]
    y <- self$data$y[index, 1]

    list(x, y)
  },

  .length = function() {
    self$data$y$size()[[1]]
  }
)


train_dl <- torch::tensor_dataset(vart)
fitted <- model %>%
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam,
    metrics = list(
      luz_metric_rmse()
    )
  ) %>%
  fit(data = train_data, epochs = 10)


# example
train_dl <- dataloader(train_ds, batch_size = 128, shuffle = TRUE)
test_dl <- dataloader(test_ds, batch_size = 128)

net <- nn_module(
  "Net",
  
  initialize = function() {
    self$conv1 <- nn_conv2d(1, 32, 3, 1)
    self$conv2 <- nn_conv2d(32, 64, 3, 1)
    self$dropout1 <- nn_dropout2d(0.25)
    self$dropout2 <- nn_dropout2d(0.5)
    self$fc1 <- nn_linear(9216, 128)
    self$fc2 <- nn_linear(128, 10)
  },
  
  forward = function(x) {
    x %>%                                  # N * 1 * 28 * 28
      self$conv1() %>%                     # N * 32 * 26 * 26
      nnf_relu() %>%                       
      self$conv2() %>%                     # N * 64 * 24 * 24
      nnf_relu() %>% 
      nnf_max_pool2d(2) %>%                # N * 64 * 12 * 12
      self$dropout1() %>% 
      torch_flatten(start_dim = 2) %>%     # N * 9216
      self$fc1() %>%                       # N * 128
      nnf_relu() %>% 
      self$dropout2() %>% 
      self$fc2()                           # N * 10
  }
)
fitted <- net %>%
  setup(
    loss = nn_cross_entropy_loss(),
    optimizer = optim_adam,
    metrics = list(
      luz_metric_accuracy()
    )
  ) %>%
  fit(train_dl, epochs = 10, valid_data = test_dl)


luz::setup(model, loss= luz::luz_metric_rmse(), optimizer = torch::optim_adam(model$parameters()), metrics = luz::get_metrics(luz::luz_metric_mse()))
luz::evaluate()
luz::fit(model, vart)

torch::nnf_avg_pool2d(inputs = )

