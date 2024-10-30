crossfit_propensity <- function(x, ...) {
  UseMethod("crossfit_propensity")
}

#' @export
crossfit_propensity.TransportTask <- function(task, learners, control, pb) {
  ans <- vector("list", length = task$nfolds())

  for (fold in seq_along(task$folds)) {
    train <- task$training(fold)
    valid <- task$validation(fold)

    ans[[fold]] <- future::future({
      estimate_propensity.TransportTask(train, valid, learners, control, pb)
    },
    seed = TRUE)
  }

  ans <- future::value(ans)

  # do something
}

estimate_propensity.TransportTask <- function(train, valid, learners, control, pb) {
  on.exit(pb())

  train$reset()
  valid$reset()

  features <- train$features("A")
  target <- train$col_roles$A

  # Subset columns
  train$select(c(features, target))

  fit <- train_nuisance(
    train$data(),
    target,
    learners,
    "binomial",
    "lmtp_id",
    control$.learners_trt_folds,
    control$.discrete,
    control$.info
  )

  list(prop_score = matrix(predict(fit, newdata = valid$data()), ncol = 1),
       fit = fit)
}
