

library(torch)
library(luz)
library(data.table)

### see how it works
gen_stdt <- function(
  xdim = 40L,
  ydim = 30L,
  tdim = 60L,
  day_start = Sys.Date(),
  expand_attr = 6L
) {
  xspread <- seq(1, xdim)
  yspread <- seq(1, ydim)
  tspread <- day_start + (seq(1, tdim) - 1)

  xytdt <-
    expand.grid(
      x = xspread,
      y = yspread,
      t = tspread
    )
  xytdt$xyid <- as.integer(as.factor(unique(sprintf("%02d%02d", xytdt$x, xytdt$y))))

  if (expand_attr > 0) {
    attrs <- runif(nrow(xytdt) * expand_attr, 0, 20)
    attrs <- matrix(attrs, nrow = nrow(xytdt), ncol = expand_attr)
    attrs <- as.data.frame(attrs)
    colnames(attrs) <- sprintf("Var%03d", seq_len(expand_attr))
    xytdt <- cbind(xytdt, attrs)
  }
  xytdt <- data.table::as.data.table(xytdt)
  data.table::setorderv(xytdt, c("t", "x", "y"))
  #xytdt <- xytdt[order(xytdt$xyid), ]
  return(xytdt)
}
checked <- gen_stdt()
checkedm <- checked[, 5:10] |> as.matrix()
checked_tens <- torch::torch_tensor(checkedm, dtype = torch::torch_float32())
checked_tensrs <- torch::torch_reshape(checked_tens, list(60, 40, 30, 6))
head(checked)

library(terra)
checkedras <-
terra::rast(checked[checked$t == as.Date("2023-12-22"), c(1,2,5)],
            type = "xyz")
checkedras[1:3,40:38]
#torch::torch_reshape(checked[5:10], list(40, 30, 60, 6))

### function translation
SimpleRNN <- torch::nn_module(
    "SimpleRNN",
    initialize = function(input_size, hidden_size=32, output_size=1, num_layers=1, dropout=0.25){
        # super?
        # super(SimpleRNN, self)$__init__()
        self$hidden_size = hidden_size
        self$rnn = torch::nn_rnn(
            input_size = input_size,
            hidden_size = hidden_size,
            nonlinearity = "relu",    # 'tanh' or 'relu'
            num_layers = num_layers,
            dropout = dropout,
            batch_first = TRUE
        )
        self$linear = torch::nn_linear(hidden_size, output_size)
    },
    forward = function(x, hidden) {
        output = self$rnn(x, hidden)[[1]]
        hidden = self$rnn(x, hidden)[[2]]
        pred = self$linear(output[, -1, ])
        return(list(pred, hidden))
    },
    init_hidden = function(){
        return(torch::torch_randn(1, 24, self$hidden_size))
    }
)

SimpleRNN %>%
setup(loss = nn_mse_loss(), optimizer = optim_adam)

SimpleGRU <- torch::nn_module(
    initialize = function(self, input_size, hidden_size, output_size=1, num_layers=1, dropout=0.25) {
        super(SimpleGRU, self).__init__()
        self$hidden_size = hidden_size
        self$gru = nn.GRU(
            input_size=input_size,
            hidden_size=hidden_size,
            num_layers=num_layers,
            # dropout=dropout,
            batch_first=TRUE
        )
        self$linear = nn.Linear(hidden_size, output_size)
    }
    forward = function(self, x, hidden) {
        output, hidden = self$gru(x, hidden)
        pred = self$linear(output[, -1, ])
        return(pred, hidden)
    }
    init_hidden = function(self) {
        return(torch::torch_randn(1, 24, self$hidden_size, pin_memory = TRUE))
    }
)

SimpleLSTM <- torch::nn_module(
    initialize = function(self, input_size, hidden_size, output_size=1, num_layers=1, dropout=0.25){
        super(SimpleLSTM, self).__init__()
        self$hidden_size = hidden_size
        self$lstm = torch::nn_lstm(
            input_size=input_size,
            hidden_size=hidden_size,
            num_layers=num_layers,
            # dropout=dropout,
            batch_first=TRUE
        )
        self$linear = torch::nn_linear(hidden_size, output_size)
    }
    forward = function(self, x) {
        output, (h_n, c_n) = self$lstm(x)
        pred = self$linear(output[, -1, ])
        return pred
    }

    init_hidden = function(self) {
        return torch::torch_randn(1, 24, self$hidden_size, pin_memory = TRUE)
    }
)

TCN <- torch::nn_module(
    initialize = function(self, input_size, output_size, num_channels, kernel_size, dropout){
        super(TCN, self).__init__()
        self$tcn = TemporalConvNet(input_size, num_channels, kernel_size, dropout=dropout)
        self$linear = nn.Linear(num_channels[-1], output_size)
    }
    forward = function(self, x){
        output = torch::torch_transpose(self$tcn(torch::torch_transpose(x, 1, 2)), 1, 2)
        pred = self$linear(output[, -1, ])
        return(pred)
    }
)

Chomp1d <- torch::nn_module(
    initialize = function(self, chomp_size){ }
        super(Chomp1d, self).__init__()
        self$chomp_size = chomp_size

    forward = function(self, x){ }
        return x[, , -self$chomp_size].contiguous()
)

TemporalBlock <- torch::nn_module(
    initialize = function(self, n_inputs, n_outputs, kernel_size, stride, dilation, padding, dropout=0.2) {
        super(TemporalBlock, self).__init__()
        self$conv1 = weight_norm(torch::nn_conv1d(n_inputs, n_outputs, kernel_size,
                                           stride=stride, padding=padding, dilation=dilation))
        self$chomp1 = Chomp1d(padding)
        self$relu1 = torch::nn_relu()
        self$dropout1 = torch::nn_dropout(dropout)

        self$conv2 = weight_norm(torch::nn_conv1d(n_outputs, n_outputs, kernel_size,
                                           stride=stride, padding=padding, dilation=dilation))
        self$chomp2 = Chomp1d(padding)
        self$relu2 = torch::nn_relu()
        self$dropout2 = torch::nn_dropuot(dropout)

        self$net = nn.Sequential(self$conv1, self$chomp1, self$relu1, self$dropout1,
                                 self$conv2, self$chomp2, self$relu2, self$dropout2)
        if (n_inputs != n_outputs) {
            self_downsample <- torch::nn_conv1d(n_inputs, n_outputs, 1)
        } else {
            self$downsample <- NULL
        }
        self$relu = torch::nn_relu()
        self$init_weights()
    }

    init_weights = function(self) {
        self$conv1.weight.data.normal_(0, 0.01)
        self$conv2.weight.data.normal_(0, 0.01)
        if !is.null(self$downsample) {
            self$downsample.weight.data.normal_(0, 0.01)
        }
    }
    forward = function(self, x) {
        out = self$net(x)
        res = x if self$downsample is None else self$downsample(x)
        return self$relu(out + res)
    }
)

TemporalConvNet <- torch::nn_module(
    initialize = function(self, num_inputs, num_channels, kernel_size=2, dropout=0.2){ }
        super(TemporalConvNet, self).__init__()
        layers = []
        num_levels = len(num_channels)
        for (i in range(num_levels)) {
            dilation_size = 2 ** i
            if (i == 0) {
                in_channels = num_inputs
            } else {
                in_channels = num_channels[i - 1]
            }
            out_channels = num_channels[i]
            layers <- layers + TemporalBlock(in_channels, out_channels, kernel_size, stride=1, dilation=dilation_size,
                                     padding=(kernel_size-1) * dilation_size, dropout=dropout)
        }
        self$network = nn.Sequential(*layers)

    forward = function(self, x) {
        return(self$network(x))
    }
)

STCN <- torch::nn_module(
    initialize = function(self, input_size, in_channels, output_size, num_channels, kernel_size, dropout) {
        super(STCN, self)$__init__()
        self$conv = torch::nn_sequential(
            torch::nn_conv2d(in_channels=in_channels, out_channels=64, kernel_size=(1, 1), stride=1, padding=0),
            torch::nn_batchnorm2d(64),
            torch::nn_relu(),
            torch::nn_conv2d(in_channels=64, out_channels=1, kernel_size=(1, 1), stride=1, padding=0),
            torch::nn_batchnorm2d(1),
            torch::nn_relu()
        )
        self$tcn = TemporalConvNet(input_size, num_channels, kernel_size, dropout=dropout)
        self$linear = nn.Linear(num_channels[-1], output_size)
    }
    forward = function(self, x){
        conv_out = torch::torch_squeeze(self$conv(x), 0)
        output = self$tcn(torch::torch_transpose(torch::torch_transpose(conv_out, 1, 2), 1, 2))
        pred = self$linear(output[, -1, ])
        return(pred)
    }
)