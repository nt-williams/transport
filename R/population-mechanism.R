crossfit_population <- function(x, ...) {
  UseMethod("crossfit_population")
}

#' @export
crossfit_population.TransportTask <- function(task, learners, control, pb) {
  ans <- vector("list", length = task$nfolds())

  for (fold in seq_along(task$folds)) {
    train <- task$training(fold)
    valid <- task$validation(fold)

    ans[[fold]] <- future::future({
      estimate_population.TransportTask(train, valid, learners, control, pb)
    },
    seed = TRUE)
  }

  ans <- future::value(ans)

  list(prob = recombine(rbind_depth(ans, "prob_target"), task$folds),
       fits = lapply(ans, \(x) x[["fit"]]))
}

estimate_population.TransportTask <- function(train, valid, learners, control, pb) {
  on.exit(pb())

  train$reset()
  valid$reset()

  features <- train$features("S")
  target <- train$col_roles$S

  fit <- train(
    train$select(c(features, target))$data(),
    target,
    learners,
    "binomial",
    train$select(train$col_roles$id)$data(),
    control$.learners_trt_folds,
    control$.discrete,
    control$.info
  )

  list(prob_target = matrix(predict(fit, newdata = valid$data()), ncol = 1),
       fit = return_full_fit(fit, control))
}
