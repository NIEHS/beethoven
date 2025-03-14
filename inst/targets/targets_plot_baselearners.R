################################################################################
##### Set base learner types, cross validation methods, and {tune}-able
##### hyperparameters.
target_plot_baselearners <-
  list(
    tar_target(
        plot_base_elnets_cv,
        command = {
            r2 <- yardstick::rsq(
        data = data.frame(
            truth = dt_feat_calc_xyt$Arithmetic.Mean,
            pred = fit_learner_base_elnet[[2]][[1]]$.pred
        ),
        truth = truth,
        estimate = pred
       )
        }
    )
  )



