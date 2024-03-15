
pak::pak("luz")
pak::pak("torchopt")
library(beethoven)
library(amadeus)
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
               return(pnts)
             })
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
    colnames(pstdt$stdt)
  )
xtensor <-
  pstdt$stdt[, ..colindx] %>%
  data.table::melt(measure.vars = colnames(pstdt$stdt)[colindx]) %>%
  .[, 2] %>%
  as.matrix() %>%
  torch::torch_tensor(dtype = torch::torch_float()) %>%
  torch::torch_reshape(shape = list(50, 60, 4))

colindy <- grep("pm2.5", colnames(pstdt$stdt))
ytensor <- pstdt$stdt[, ..colindy] %>%
  as.matrix() %>%
  torch::torch_tensor(dtype = torch::torch_float()) %>%
  torch::torch_reshape(shape = list(50, 60, 1))

# torch::torch_float()

if (torch_is_installed()) {
  model <- nn_module(
    "testConv2d",
    initialize = function() {
      self$conv1 <- nn_conv2d(4, 16, 5, 2) # N * 4 * 50 * 60
      self$conv2 <- nn_conv2d(16, 48, 3, 1) # N * 16 * 23 * 28
      # self$dropout1 <- nn_dropout2d(0.25)
      # self$dropout2 <- nn_dropout2d(0.33)
      self$fc1 <- nn_linear(192, 100)
      self$fc2 <- nn_linear(100, 1)
    },
    forward = function(input) {
      input %>%
        self$conv1() %>%
        nnf_relu() %>%
        self$conv2() %>%
        nnf_relu() %>%
        nnf_max_pool2d(2) %>%
        nnf_relu() %>%
        self$fc1() %>%
        nnf_relu() %>%
        self$fc2()
    }
  )
}

train_data <- torch::dataset(
  name = "faux_dataset",
  initialize = function(yt, xt) {
    self <- list()
    self$y <- torch_tensor(yt)
    self$x <- torch_tensor(xt)
  },

  .getitem = function(index_s, index_t = seq_len(dim(self$y)[3])) {
    list(self$x[index_s, index_t, ],
         self$y[index_s, index_t, ])
  },

  .length = function() {
    dim(self$y)[1]
    #self$y$size
  }
)

train_data_call <- train_data(ytensor, xtensor)
tdc <- train_data(ytensor, xtensor)
#tdc$.getitem(1, 2)
#tdc$.length()
# train_dl <- torch::tensor_dataset(vart)
# example
train_dl <- dataloader(tdc, batch_size = 128, shuffle = TRUE)
# test_dl <- dataloader(train_data, batch_size = 128)
fitted <- model %>%
  setup(
    loss = torch::nn_mse_loss,
    optimizer = torchopt::optim_adamw,
    metrics = list(
      luz_metric_rmse()
    )
  ) %>%
  luz::fit(data = train_dl, epochs = 10)

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

luz::setup(model, loss = luz::luz_metric_rmse(), optimizer = torch::optim_adam(model$parameters()), metrics = luz::get_metrics(luz::luz_metric_mse()))
luz::evaluate()
luz::fit(model, vart)

torch::nnf_avg_pool2d(inputs = )

# Copilot Suggestion: basic use case
library(torch)
torch::cuda_get_device_capability(device = 1)
torch::cuda_is_available()
torch::backends_cudnn_is_available()
torch::cuda_runtime_version()
torch::backends_mkldnn_is_available()
torch::torch_device("mkl:0")

?chopin::any_class_args()

# Assuming x is your 4D input tensor
x <- torch_rand(c(256, 50, 1000, 1000))

# Define the ConvLSTM module
ConvLSTM <- nn_module(
  "ConvLSTM",
  initialize = function() {
    self$conv1 <- nn_conv2d(1, 16, kernel_size = 3, stride = 1, padding = 1)
    self$dropout <- nn_dropout2d(0.5)
    self$flatten <- nn_flatten(3, 4)
    self$lstm <- nn_lstm(16, 32, batch_first = TRUE)
    self$fc <- nn_linear(32, 1)
  },
  forward = function(x) {
    x <- self$conv1(x)
    x <- nnf_relu(x)
    x <- self$dropout(x)
    x <- self$flatten(x)
    # x <- x$flatten(dim(x)[1], dim(x)[2], prod(dim(x)[c(3, 4)])) # Flatten the spatial dimensions
    x <- self$lstm(x)
    x <- self$fc(x)
    x
  }
)

# Initialize the model
model <- ConvLSTM()

# Assuming y is your target tensor
y <- torch_rand(c(256, 1e6))

# Define the loss function and the optimizer
loss_fn <- nn_mse_loss()
optimizer <- optim_adam(model$parameters)

# Training loop
for (epoch in 1:100) {
  # Forward pass
  prediction <- model(x)
  loss <- loss_fn(prediction, y)
  
  # Backward pass and optimization
  loss$backward()
  optimizer$step()
  optimizer$zero_grad()
  
  # Print loss for every 10 epochs
  if (epoch %% 10 == 0) {
    cat("Epoch: ", epoch, " Loss: ", as.numeric(loss), "\n")
  }
}
