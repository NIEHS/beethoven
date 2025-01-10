
target_baselearner <-
  list(
    targets::tar_target(
      list_learner_mlp,
      command = torch::load_torch("/inst/extdata/list_learner_mlp.pt"),
      format = "torch"
    )   
  )
  