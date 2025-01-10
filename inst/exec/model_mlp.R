############################        SETTINGS        ############################
cat("Running non-target {beethoven}", Sys.getenv("BEETHOVEN"), "...\n")
cat("Model: {brulee} multi-layer perceptron\n")

# Set paths for R, CUDA, and LD_LIBRARY_PATH, and check for CUDA availability.
beethoven:::sys_beethoven()

# Model object path.
list_learner_mlp <- "/inst/extdata/list_learner_mlp.pt"
if (!file.exists(list_learner_mlp)) {
  ###########################       BRULEE MLP       ###########################
  # Read full covariate data.
  dt_feat_calc_xyt <- qs2::qs_read("/opt/_targets/objects/dt_feat_calc_xyt")

  # Set model.
  mlp_model <- beethoven::switch_model("mlp", device = "cuda")

  # Set grid.
  mlp_grid <- expand.grid(
    hidden_units = list(c(64, 64), c(64, 128), c(128, 128)),
    dropout = c(0.2, 0.3333333),
    activation = c("relu"),
    learn_rate = c(0.1, 0.05, 0.01)
  )

  # Fit mlp with {brulee} (spatiotemporal fold cross validation and
  # grid tuning).
  mlp_start <- Sys.time()
  mlp <- beethoven::fit_base_learner(
    learner = "mlp",
    dt_full = dt_feat_calc_xyt,
    r_subsample = 0.3,
    model = mlp_model,
    folds = 10L,
    cv_mode = "spatiotemporal",
    tune_mode = "grid",
    tune_grid_in = mlp_grid,
    tune_grid_size = 10,
    learn_rate = 0.1,
    yvar = "Arithmetic.Mean",
    xvar = seq(5, ncol(dt_feat_calc_xyt)),
    nthreads = 1,
    trim_resamples = FALSE,
    return_best = TRUE
  )
  mlp_end <- Sys.time()
  cat(
    "Run time: ",
    round(difftime(mlp_end, mlp_start, units = "mins"), 2),
    "minutes\n"
  )

  torch::torch_save(mlp, path = list_learner_mlp)
} else {
  cat("Skipped {brulee} multi-layer perceptron\n")
}
