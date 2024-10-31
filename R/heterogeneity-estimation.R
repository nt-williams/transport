crossfit_heterogeneity <- function(x, ...) {
  UseMethod("crossfit_heterogeneity")
}

#' @export
crossfit_heterogeneity.TransportTask <- function(task, learners, control) {
  ans <- vector("list", length = task$nfolds())

  for (fold in seq_along(task$folds)) {
    train <- task$training(fold)
    valid <- task$validation(fold)

    ans[[fold]] <- future::future({
      estimate_heterogeneity.TransportTask(train, valid, learners, control)
    },
    seed = TRUE)
  }

  ans <- future::value(ans)

  list(pred = recombine(rbind_depth(ans, "pred"), task$folds),
       fits = lapply(ans, \(x) x[["fit"]]))
}

estimate_heterogeneity.TransportTask <- function(train, valid, learners, control) {
  train$reset()
  valid$reset()

  features <- "estimated_source_prob"
  target <- "estimated_cate"

  fit <- train(
    train$select(c(features, target))$data(),
    target,
    learners,
    "continuous",
    train$select(train$col_roles$id)$data(),
    control$.learners_folds,
    control$.discrete,
    control$.info
  )

  list(pred = matrix(predict(fit, newdata = valid$data()), ncol = 1),
       fit = return_full_fit(fit, control))
}
